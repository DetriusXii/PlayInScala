package controllers

import com.squeryl.jdip.tables._

object DiplomacyQueries {
  def getMoves(srcLocationID: Int): List[Location] = {
    val locationOption = CommonQueries.locations.find(_.id == srcLocationID)
    locationOption.map(CommonQueries.getAdjacentLocationsForLocation(_)) match {
      case Some(u: List[_]) => u
      case None => Nil
    } 
  }
    
  def getMoves(srcDiplomacyUnit: DiplomacyUnit): List[Location] = 
    getMoves(srcDiplomacyUnit.unitLocation)
    
  def getFormattedMoves(srcLocationID: Int): List[String] = 
    getMoves(srcLocationID).map(getFormattedLocationName(_))
      
  def getMoveOrdersMap(diplomacyUnits: Iterable[DiplomacyUnit]) = 
    diplomacyUnits.map(dpu => {
      val srcLocationOption = CommonQueries.locations.find(_.id == dpu.unitLocation)
      srcLocationOption.map(loc => 
        (getFormattedLocationName(loc), getFormattedMoves(dpu.unitLocation)))
    }).flatten
    
  def getFormattedLocationName(location: Location): String = 
    location match {
    	case Location(prov, Coast.NO_COAST) => prov
    	case Location(prov, Coast.ALL_COAST) => prov
    	case Location(prov, coast) => "%s-%s" format (prov, coast)
	  }
  
  def getSupportHolds(srcLocationID: Int): List[Location] = {
    val locationOption = CommonQueries.locations.find(_.id == srcLocationID)
    val adjacentLocations = 
      locationOption.map(CommonQueries.getAdjacentLocationsForLocation(_)).flatten
    val provincialLocations =
      CommonQueries.locations.filter(_ match {
        case Location(province, _) => 
          adjacentLocations.exists(_.province.equals(province))
      })
    val allUnits = CommonQueries.getAllUnits
    val provincialLocationsWithUnitPresent =
      provincialLocations.filter(loc => allUnits.exists(_.unitLocation == loc.id))
    
    
    
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
  

  def getMovesByConvoy(diplomacyUnit: DiplomacyUnit): List[Location] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.ARMY, _, unitLocation, _, _) => 
        locations.filter(_.id == unitLocation).flatMap(loc => {
          val allPaths = findAllPathsExternal(loc, getAllFleetUnits)
          allPaths.map(_ match {
            case h :: tail => h
          })
        })
      case _ => Nil
    }
  }
}