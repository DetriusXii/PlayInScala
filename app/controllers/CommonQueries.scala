package controllers

import scalaz._
import scalaz.effects._
import play.api.db.DB
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.adapters._

object CommonQueries extends States  {
  import play.api.Play._
  
  lazy val locations: List[Location] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.locations.toList
    }
  })
  
  lazy val adjacencies: List[Adjacency] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.adjacencies.toList
    }
  })
  
  lazy val provinces: List[Province] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.provinces.toList
      }
    })
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.orderTypeUnitTypes.toList
      }
    })

  lazy val orderTypes: List[OrderType] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.orderTypes.toList
      }
    })


  def getAdjacentLocationsForLocation(loc: Location): List[Location] = {
    adjacencies.filter(_.srcLocation == loc.id).map(adj => {
      locations.find(_.id == adj.dstLocation)
    }).flatten
  }
  
  def getCoastLocationsFromLandLocation(loc: Location): List[Location] = {
    locations.filter(_ match {
      case Location(loc.province, Coast.NO_COAST) => false
      case Location(loc.province, _) => true
      case _ => false
    })
  }
  
  def hasLandLocation(loc: Location): Boolean =
    locations.exists(_ match {
      case Location(loc.province, Coast.NO_COAST) => true
      case _ => false
    })
  
  def getAllLandUnits: List[DiplomacyUnit] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.diplomacyUnits)(dpu => {
        where(dpu.unitType === UnitType.ARMY) select(dpu)
      }).toList
    }
  })
  
  def getAllFleetUnits: List[DiplomacyUnit] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.diplomacyUnits)(dpu => {
        where(dpu.unitType === UnitType.FLEET) select(dpu)
      }).toList
    }
  })
  
  def getAllUnits: List[DiplomacyUnit] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.diplomacyUnits
      }.toList
    })
  
  def getDiplomacyUnit(location: Location): Option[DiplomacyUnit] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits)(dpu => {
          where(dpu.unitLocation === location.id) select(dpu)
        }).toList.firstOption
      }
    })

  def getPlayers: List[Player] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.players)(ply => select(ply)).toList
      }
    })
  

              

  
  def findAllPathsExternal(originLocation: Location, 
      allFleetUnits: List[DiplomacyUnit]): List[List[Location]] = {
    def findAllPaths(currentLocation: Location,
    	allPaths: List[List[Location]],
    	presentPath: List[Location]): List[List[Location]] = {
      val isCurrentLocOnOriginLoc = currentLocation match {
        case Location(originLocation.province, originLocation.coast) => true
        case _ => false
      }
      val isCurrentLocOnPath = presentPath.exists(_.id == currentLocation.id)
      val hasFleetUnit = allFleetUnits.exists(_.unitLocation == currentLocation.id)
      val isCurrentLocLandLocation = currentLocation match {
        case Location(_, Coast.NO_COAST) => true
        case _ => false
      }
     
      val isCurrentLocOnOriginProvince = 
        currentLocation.province.equals(originLocation.province)
      
      if (isCurrentLocLandLocation && isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        val coastLocations = getCoastLocationsFromLandLocation(currentLocation)
        coastLocations.foldLeft(allPaths)((u, v) => {
          findAllPaths(v, allPaths, newPath)
        })
      } else if (isCurrentLocLandLocation && !isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        newPath :: allPaths
      } else if (!isCurrentLocLandLocation && isCurrentLocOnOriginProvince) {
        val adjLocations = getAdjacentLocationsForLocation(currentLocation)
        val oceanAdjLocations = adjLocations.filter(!hasLandLocation(_))
        val newPath = currentLocation :: presentPath
        oceanAdjLocations.foldLeft(allPaths)((u, v) => findAllPaths(v, u, newPath))
      } else if (!isCurrentLocOnOriginProvince && hasLandLocation(currentLocation)) {
        val newPath = currentLocation :: presentPath;
        val landLocationOption = locations.find(_ match {
          case Location(currentLocation.province, Coast.NO_COAST) => true
          case _ => false
        })
        landLocationOption match {
          case Some(loc) => findAllPaths(loc, allPaths, newPath)
          case None => allPaths
        }
      } else if (isCurrentLocOnPath) {
        allPaths
      } else if (!isCurrentLocOnPath && hasFleetUnit) {
        val newPath = currentLocation :: presentPath
        val adjLocations = getAdjacentLocationsForLocation(currentLocation)
        adjLocations.foldLeft(allPaths)((u, v) => findAllPaths(v, u, newPath))
      } else {
        allPaths
      }
    }
  
    findAllPaths(originLocation, Nil, Nil)
  }


  def getConvoys(diplomacyUnit: DiplomacyUnit): List[(Location, List[Location])] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.FLEET, _, unitLocation, _, _) => {
        locations.find(_.id == unitLocation).flatMap(loc => {
          val isCoastal = hasLandLocation(loc)

          if (isCoastal) {
            None
          } else {
            Some(loc)
          }
        }).map(fleetLoc => {
          val allLandUnits: List[DiplomacyUnit] = getAllLandUnits
          val allFleetUnits: List[DiplomacyUnit] = getAllFleetUnits
          val allLandUnitLocations = allLandUnits.
            map((ldpu: DiplomacyUnit) => locations.find(_.id == ldpu.unitLocation)).
            flatten
          allLandUnitLocations.map((loc: Location) => {
            val allPaths: List[List[Location]] = findAllPathsExternal(loc, allFleetUnits)
            val targetDestinations: List[Location] = allPaths.filter((path: List[Location]) => {
                path.exists(_.id == fleetLoc.id)
              }).map(_ match {
                case h :: tail => h
              })
              (loc, targetDestinations)
            }).filter(!_._2.isEmpty)
        }) match {
          case Some(x) => x
          case None => Nil
        }
      }
      case _ => Nil
    }
  }

  def getGamePlayerEmpire(gameName: String, playerName: String): Option[GamePlayerEmpire] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.gamePlayerEmpires) {gpe =>
          where(gpe.gamePlayerKey in from(Jdip.gamePlayers) {gp => 
            where(gp.gameName === gameName and gp.playerName === playerName)
            select(gp.id)})
          select(gpe)
        }
      }.firstOption
    })

  def getDiplomacyUnits(gamePlayerEmpire: GamePlayerEmpire): List[DiplomacyUnit] = {
    from(Jdip.diplomacyUnits) {dpu =>
      where(dpu.owner === gamePlayerEmpire.id)
      select(dpu)
    }.toList
  }
}
