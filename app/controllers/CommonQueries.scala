package controllers

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas._
import com.squeryl.jdip.tables._
import scalaz._
import scalaz.effects._
import play.api.db.DB
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

object CommonQueries extends States with  {
  import play.api.Application._
  
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
      })
    }
  })
  
  def getAllFleetUnits: List[DiplomacyUnit] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.diplomacyUnits)(dpu => {
        where(dpu.unitType === UnitType.FLEET) select(dpu)
      })
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
  
  private trait VisitedQuery
  private type LocationWithMarkerNodeList = Iterable[Tuple2[VisitedMarker, Location]]
  private type VisitedMarker = STRef[VisitedQuery, Boolean]
  private type ForallCommonQueries[A] = Forall[({type Q[S] = ST[S, A]})#Q]

  def traverseAdjacencies(loc : Location,
		  				  originLocation: Location,
                          convoyableLocations: List[Location],
                          nodeList: LocationWithMarkerNodeList): List[Location] = {
    val updatedNodeList = markLocationAsVisited(loc, nodeList)

    val newConvoyableLocations = Jdip.locations.find(_ match {
      case Location(loc.province, Coast.NO_COAST) if !loc.province.equals(originLocation.province) => true
      case _ => false
    }) match {
      case Some(newLoc: Location) => newLoc :: convoyableLocations
      case None => convoyableLocations
    }
    
    val isLocationOnLand = Jdip.locations.exists(_ match {
      case Location(loc.province, Coast.NO_COAST) => true
      case _ => false
    })
    
    if (!isLocationOnLand) {
      val adjacenciesForLocation = Jdip.adjacencies.filter(_.srcLocation == loc.id)
      val nextUnvisitedLocations = Jdip.locations.filter(loc => 
	  	adjacenciesForLocation.exists(_.dstLocation == loc.id)).
	  	filter(loc => isLocationVisited(loc, updatedNodeList))
	  
	  val nextLocationsWithFleetUnits = nextUnvisitedLocations.filter(loc =>
	      Jdip.diplomacyUnits.exists(_ match {
	        case DiplomacyUnit(UnitType.FLEET, _, loc.id, _, _) => true
	        case _ => false
	      }))
	  val nextLocationsWithFleetUnitsAndOpenSeas = nextLocationsWithFleetUnits.filter(_ match {
	      case Location(province, _) => Jdip.locations.exists(_ match {
	        case Location(`province`, Coast.NO_COAST) => false
	        case _ => true
	      })
	  })
	  
	  val nextLocationsWithLand = nextUnvisitedLocations.filter(_ match {
	    case Location(province, _) => Jdip.locations.exists(_ match {
	      case Location(`province`, Coast.NO_COAST) => true
	      case _ => false
	    })
	  })
	  
	  val nextLocationsEitherOpenSeaFleetOrLand = 
	    nextLocationsWithLand ++ nextLocationsWithFleetUnitsAndOpenSeas
	  
	  nextLocationsEitherOpenSeaFleetOrLand.foldLeft(newConvoyableLocations)((u, v) => 
      	traverseAdjacencies(v, originLocation, u, updatedNodeList))
    } else {
      newConvoyableLocations
    }
    
  }

  private def isLocationVisited(loc: Location, nodeList: LocationWithMarkerNodeList): Boolean = {
    val visitedMarkerOption = nodeList.find(_._2.id == loc.id)
    val visitedOption = visitedMarkerOption.map(u => {
      val visitedMarkerSTRef = u._1
      runST(new ForallCommonQueries[Boolean] { 
          def apply[VisitedQuery] = visitedMarkerSTRef.read.asInstanceOf[ST[VisitedQuery, Boolean]] })
    })

    visitedOption match {
      case Some(false) => true
      case _ => false
    }
  }

  private def markLocationAsVisited(loc: Location, 
                            nodeList: LocationWithMarkerNodeList): LocationWithMarkerNodeList = {
    val trueSTRef = new VisitedMarker(true)
    nodeList.find(_._2.id == loc.id).map(u => {
      runST(new ForallCommonQueries[Unit] { 
          def apply[VisitedQuery] = (u._1 swap trueSTRef).asInstanceOf[ST[VisitedQuery, Unit]] })
    })
    nodeList
  }

	def getMovesByConvoy(srcDiplomacyUnit: DiplomacyUnit): Iterable[Location] = {
	  (srcDiplomacyUnit.unitType match {
	    case UnitType.ARMY => Some(srcDiplomacyUnit)
	    case UnitType.FLEET => None
	  }).flatMap(dpu => {
	    val unitLocationOption: Option[Location] = Jdip.locations.lookup(dpu.unitLocation)
	     
	    val coastsOnProvinceOption: Option[Iterable[Location]] = unitLocationOption.map(_ match {
	      case Location(province, Coast.NO_COAST) => Jdip.locations.filter(_ match {
	        case Location(`province`, coast) => !coast.equals(Coast.NO_COAST)
	        case Location(_, _) => false
	      })
	    })
	  
	  unitLocationOption.flatMap(loc => 
	  	coastsOnProvinceOption.map((dpu, _, loc))
	  )
    }).flatMap((u: Tuple3[DiplomacyUnit, Iterable[Location], Location]) => {
      val diplomacyUnit = u._1
      val coastLocations = u._2
      val originLocation = u._3

      val locationsWithVisitedMarker: LocationWithMarkerNodeList = 
        Jdip.locations.map((new VisitedMarker(false), _))

      Some(coastLocations.foldLeft(Nil: List[Location])((u, v) => 
         traverseAdjacencies(v, originLocation, u, locationsWithVisitedMarker)))
    }) match {
      case Some(x: Iterable[Location]) => x
      case None => Nil
    }
  }

  def getMovesByConvoyMap(diplomacyUnits: Iterable[DiplomacyUnit]): Iterable[(String, Iterable[String])] = {
    diplomacyUnits.map(dpu => {
      val dpuLocationOption = Jdip.locations.lookup(dpu.unitLocation)
      dpuLocationOption.map(loc => {
        (getFormattedLocationName(loc), getMovesByConvoy(dpu).map(getFormattedLocationName(_)))
      })
    }).flatten
  }
  
  def findAllPaths(currentLocation: Location,
		  		   originLocation: Location, 
		  		   allPaths: List[List[Location]],
		  		   presentPath: List[Location],
		  		   allFleetUnits: List[DiplomacyUnit]): List[List[Location]] = {
 
    val landLocationOption = locations.find(_ match {
      case Location(currentLocation.province, Coast.NO_COAST) => true
      case _ => false
    })
    
    val hasFleetUnit = allFleetUnits.find(_.unitLocation == currentLocation.id)
    val newPresentPath = loc :: presentPath
    
    landLocationOption match {
      case Some(loc: Location) => newPresentPath :: allPaths
      case None if hasFleetUnit => {
        val newPresentPath = currentLocation :: presentPath
        
        val adjacenciesForLocation = adjacencies.filter(_.srcLocation == currentLocation.id)
        val adjacentLocations = locations.filter(loc => adjacenciesForLocation.exists(_.dstLocation == loc.id))
        val adjacentLocationsNotOnPath = adjacentLocations.filter(loc => !newPresentPath.exists(_.id == loc.id))
        
        adjacentLocations.foldLeft(allPaths)((ap: List[List[Location]], loc: Location) => {
          findAllPaths(loc, originLocation, ap, newPresentPath, allFleetUnits)
        })
      }
      case None if !hasFleetUnit => allPaths
    }
  }
  
  def getConvoys(srcDiplomacyUnit: DiplomacyUnit): Iterable[(Location, Iterable[Location])] = {
    val allLandUnits = getAllLandUnits
    allLandUnits.map(dpu => {
      val landUnitLocationOption = locations.find(_.id == dpu.unitLocation)
      val seaLocationsOnProvinceOption = landUnitLocationOption.map(loc =>
      	locations.filter(_ match {
      	  case Location(loc.province, Coast.NO_COAST) => false
      	  case Location(loc.province, _) => true
      	  case _ => false
      	})
      )
      
    })
    
    val convoyableMovesForLandUnits = allLandUnits.map(getMovesByConvoy(_))
    val 
  }
}
