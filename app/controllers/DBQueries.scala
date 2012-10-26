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

object DBQueries extends States  {
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
      from(Jdip.diplomacyUnits)(dpu => (
        where(dpu.unitType === UnitType.ARMY) 
        select(dpu)
      )).toList
    }
  })
  
  def getAllFleetUnits: List[DiplomacyUnit] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.diplomacyUnits)(dpu => (
        where(dpu.unitType === UnitType.FLEET) 
        select(dpu)
      )).toList
    }
  })
  
  def getAllUnits: List[DiplomacyUnit] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.diplomacyUnits.toList
      }
    })
  
  def getDiplomacyUnit(location: Location): Option[DiplomacyUnit] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits)(dpu => (
          where(dpu.unitLocation === location.id) 
          select(dpu)
        )).toList.firstOption
      }
    })

  def getPlayers: List[Player] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.players)(select(_)).toList
      }
    })

  def getGamePlayerEmpire(gameName: String, playerName: String): Option[GamePlayerEmpire] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.gamePlayerEmpires) (gpe =>
          where(gpe.gamePlayerKey in from(Jdip.gamePlayers) (gp => 
            where(gp.gameName === gameName and gp.playerName === playerName)
            select(gp.id)))
          select(gpe)
        ).toList.firstOption
      }
    })

  def getDiplomacyUnits(gamePlayerEmpire: GamePlayerEmpire): List[DiplomacyUnit] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits) (dpu =>
	      where(dpu.owner === gamePlayerEmpire.id)
	      select(dpu)
	    ).toList
      }
    })
  
  def getGamesForUser(username: String): List[Game] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.games) (g =>
          where(g.id in from(Jdip.gamePlayers) (gp => 
          	where (gp.playerName like username)
            select(gp.gameName)
          ))
          select(g)
        ).toList
      }
    })
    
   def getGameTimesForGames(gameTimeIDs: List[Int]): List[GameTime] =
     DB.withConnection((conn: java.sql.Connection) => {
       val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
       using(dbSession) {
         from(Jdip.gameTimes) {gt =>
           where(gt.id in gameTimeIDs)
           select(gt)
         }.toList
       }
     })
}
