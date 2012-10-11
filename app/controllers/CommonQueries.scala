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
  
  def getFormattedLocationName(location: Location): String = 
    location match {
    	case Location(prov, Coast.NO_COAST) => prov
    	case Location(prov, Coast.ALL_COAST) => prov
    	case Location(prov, coast) => "%s-%s" format (prov, coast)
	  }
  
	def getMoves(srcLocationID: Int): Iterable[Location] = from(Jdip.locations)(loc => 
              where(loc.id in from(Jdip.adjacencies)(adj =>
                where(adj.srcLocation === srcLocationID) select(adj.dstLocation)))
              select(loc))
    
  def getMoves(srcDiplomacyUnit: DiplomacyUnit): Iterable[Location] = 
    getMoves(srcDiplomacyUnit.unitLocation)
    
  def getFormattedMoves(srcLocationID: Int): Iterable[String] = 
    getMoves(srcLocationID).map(getFormattedLocationName(_))
      
  def getMoveOrdersMap(diplomacyUnits: Iterable[DiplomacyUnit]) = 
    diplomacyUnits.map(dpu => {
      val srcLocationOption = Jdip.locations.lookup(dpu.unitLocation)
      srcLocationOption.map(loc => (getFormattedLocationName(loc), getFormattedMoves(dpu.unitLocation)))
    }).flatten
              
  def getSupportHolds(srcLocationID: Int): Iterable[Location] = {
    val srcAdjacencies = Jdip.adjacencies.filter(adj => adj.srcLocation == srcLocationID)
    val dpuProvinces = 
      Jdip.diplomacyUnits.map(dpu => Jdip.locations.lookup(dpu.unitLocation)).flatten.map(_ match {
        case Location(province, _) => province
      })
    val srcAdjacenciesWithUnitPresent = srcAdjacencies.filter(adj => {
      val dstLocationOption = Jdip.locations.lookup(adj.dstLocation)
      dstLocationOption match {
        case Some(Location(province, _)) => dpuProvinces.exists(_ == province)
        case None => false
      }
    })
    val dstLocationsWithUnitPresent = 
      srcAdjacenciesWithUnitPresent.map(adj => Jdip.locations.lookup(adj.dstLocation)).flatten
    dstLocationsWithUnitPresent
  }
	
	 def getSupportHolds(srcDiplomacyUnit: DiplomacyUnit): Iterable[Location] = 
	   getSupportHolds(srcDiplomacyUnit.unitLocation)
	
	 def getFormattedSupportHolds(srcLocationID: Int): Iterable[String] =
	   getSupportHolds(srcLocationID).map(getFormattedLocationName(_))
	  
  def getSupportHoldsMap(diplomacyUnits: Iterable[DiplomacyUnit]) =
    diplomacyUnits.map(dpu => {
      val srcLocationOption = Jdip.locations.lookup(dpu.unitLocation)
      srcLocationOption.map(loc => 
        (getFormattedLocationName(loc), getSupportHolds(dpu.unitLocation).map(getFormattedLocationName(_))))
    }).flatten
	
  def getSupportMoves(srcLocationID: Int): Iterable[(Location, Iterable[Location])] = {
	  val movesForAllOtherUnits = 
	     Jdip.diplomacyUnits.filter(_.unitLocation != srcLocationID).map(dpu =>
	      	(dpu, getMoves(dpu.unitLocation))
	     )
	  val movesForThisUnit = getMoves(srcLocationID)
	   
	  val movesThatMatterFromOtherUnits = movesForAllOtherUnits.map(u => {
	    val otherUnit = u._1
	    val movesForOtherUnit = u._2
	     
	    val movesThatMatterFromOtherUnit = movesForOtherUnit.filter(_ match {
	      case Location(dstProvinceForOtherUnit, _) => movesForThisUnit.exists(_ match {
	        case Location(`dstProvinceForOtherUnit`, _) => true
	        case Location(_, _) => false
	      })
	    })
	     
	    Jdip.locations.lookup(otherUnit.unitLocation).map((_, movesThatMatterFromOtherUnit))
	  }).flatten
	   
    movesThatMatterFromOtherUnits.filter(_ match {
	     case (_, movesFromOtherUnit) => !movesFromOtherUnit.isEmpty
	  })
  }
	 
  def getSupportMoves(srcDiplomacyUnit: DiplomacyUnit): Iterable[(Location, Iterable[Location])] =
	   getSupportMoves(srcDiplomacyUnit.unitLocation)
   
  def getFormattedSupportMoves(srcLocationID: Int): Iterable[(String, Iterable[String])] =
	  getSupportMoves(srcLocationID).map(u => 
      (getFormattedLocationName(u._1), u._2.map(getFormattedLocationName(_))))

  def getSupportMovesMap(diplomacyUnits: Iterable[DiplomacyUnit]): 
    Iterable[(String, Iterable[(String, Iterable[String])])] = 
      diplomacyUnits.map(dpu => {
        val srcLocationOption = Jdip.locations.lookup(dpu.unitLocation)
        srcLocationOption.map((srcLocation: Location) => 
            (getFormattedLocationName(srcLocation), getFormattedSupportMoves(srcLocation.id)))
      }).flatten
  

  
  
  def findAllPaths(currentLocation: Location,
		  		   originLocation: Location, 
		  		   allPaths: List[List[Location]],
		  		   presentPath: List[Location],
		  		   allFleetUnits: List[DiplomacyUnit]): List[List[Location]] = {
 
    val isCurrentLocOnOriginLoc = currentLocation.province.equals(originLocation.province)
    
    val landLocationOption = locations.find(_ match {
      case Location(currentLocation.province, Coast.NO_COAST) if !isCurrentLocOnOriginLoc => true
      case _ => false
    })
    
    val hasFleetUnit = allFleetUnits.find(_.unitLocation == currentLocation.id) match {
      case Some(_) => true
      case None => false
    }
    
    landLocationOption match {
      case Some(loc: Location) => (loc :: presentPath) :: allPaths
      case None if hasFleetUnit && !isCurrentLocOnOriginLoc => {
        
        val adjacenciesForLocation = adjacencies.filter(_.srcLocation == currentLocation.id)
        val adjacentLocations = locations.filter(loc => adjacenciesForLocation.exists(_.dstLocation == loc.id))
        val adjacentLocationsWithoutLand = adjacentLocations.filter(loc => 
          !locations.exists(_ match {
            case Location(loc.province, Coast.NO_COAST) => true
            case _ => false
          })
        )
        val newPresentPath = currentLocation :: presentPath
        val adjacentLocationsNotOnPath = 
          adjacentLocationsWithoutLand.filter(loc => !newPresentPath.exists(_.id == loc.id))
        
        adjacentLocations.foldLeft(allPaths)((ap: List[List[Location]], loc: Location) => {
          findAllPaths(loc, originLocation, ap, newPresentPath, allFleetUnits)
        })
      }
      case _ => allPaths
    }
  }
  
  def getConvoys(srcDiplomacyUnit: DiplomacyUnit): List[(Location, Iterable[Location])] = {
    val allLandUnits = getAllLandUnits
    val allFleetUnits = getAllFleetUnits
    
    val landUnitPaths = allLandUnits.map(dpu => {
      val landUnitLocationOption = locations.find(_.id == dpu.unitLocation)
      val seaLocationsOnProvinceOption = landUnitLocationOption.map(loc =>
      	locations.filter(_ match {
      	  case Location(loc.province, Coast.NO_COAST) => false
      	  case Location(loc.province, _) => true
      	  case _ => false
      	})
      )
      seaLocationsOnProvinceOption.map((locs: List[Location]) => {
        locs.foldLeft(Nil: List[List[Location]])((paths: List[List[Location]], v: Location) => {
          findAllPaths(v, v, paths, v :: Nil, allFleetUnits)
        })
      }).flatMap((allPaths: List[List[Location]]) => landUnitLocationOption.map((_, allPaths)))
    }).flatten
    
    landUnitPaths.filter((u: Tuple2[Location, List[List[Location]]]) => {
      val landLocation = u._1
      val paths = u._2
      
      paths.exists(path => {
        path.exists(_.id = srcDiplomacyUnit.unitLocation)
      })
    })
  }
}
