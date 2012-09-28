package controllers

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas._
import com.squeryl.jdip.tables._
import scalaz._
import scalaz.effects._

object CommonQueries extends States {

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
                          convoyableLocations: List[Location],
                          nodeList: LocationWithMarkerNodeList): List[Location] = {
    val updatedNodeList = markLocationAsVisited(loc, nodeList)

    val newConvoyableLocations = Jdip.locations.find(_ match {
      case Location(loc.province, Coast.NO_COAST) => true
      case _ => false
    }) match {
      case Some(newLoc: Location) => newLoc :: convoyableLocations
      case None => convoyableLocations
    }
     
    val adjacenciesForLocation = Jdip.adjacencies.filter(_.srcLocation == loc.id)
    val nextUnvisitedLocations = Jdip.locations.filter(loc => 
      adjacenciesForLocation.exists(_.dstLocation == loc.id)).
      filter(loc => isLocationVisited(loc, updatedNodeList))

    val nextLocationsWithFleetUnits = nextUnvisitedLocations.filter(loc =>
      Jdip.diplomacyUnits.exists(_ match {
        case DiplomacyUnit(UnitType.FLEET, _, loc.id, _, _) => true
        case _ => false
      })) 

    nextLocationsWithFleetUnits.foldLeft(newConvoyableLocations)((u, v) => 
      traverseAdjacencies(v, u, updatedNodeList))
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
	      })
	    })
	     
      coastsOnProvinceOption.map((dpu, _))
    }).flatMap((u: Tuple2[DiplomacyUnit, Iterable[Location]]) => {
      val diplomacyUnit = u._1
      val coastLocations = u._2

      val locationsWithVisitedMarker: LocationWithMarkerNodeList = 
        Jdip.locations.map((new VisitedMarker(false), _))

      Some(coastLocations.foldLeft(Nil: List[Location])((u, v) => 
         traverseAdjacencies(v, u, locationsWithVisitedMarker)))
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
}
