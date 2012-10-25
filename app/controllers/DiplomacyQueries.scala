package controllers

import com.squeryl.jdip.tables._

object DiplomacyQueries {
  def getMoves(srcLocationID: Int): List[Location] = {
    val locationOption = DBQueries.locations.find(_.id == srcLocationID)
    locationOption.map(DBQueries.getAdjacentLocationsForLocation(_)) match {
      case Some(u) => u.asInstanceOf[List[Location]]
      case None => Nil
    } 
  }
    
  def getMoves(srcDiplomacyUnit: DiplomacyUnit): List[Location] = 
    getMoves(srcDiplomacyUnit.unitLocation)
    
  def getFormattedMoves(srcLocationID: Int): List[String] = 
    getMoves(srcLocationID).map(getFormattedLocationName(_))
      
  def getMoveOrdersMap(diplomacyUnits: List[DiplomacyUnit]) = 
    diplomacyUnits.map(dpu => {
      val srcLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      srcLocationOption.map(loc => 
        (getFormattedLocationName(loc), getFormattedMoves(dpu.unitLocation)))
    }).flatten
    
  def getFormattedLocationName(location: Location): String = location match {
    	case Location(prov, Coast.NO_COAST) => prov
    	case Location(prov, Coast.ALL_COAST) => prov
    	case Location(prov, coast) => "%s-%s" format (prov, coast)
  }
  
  def getSupportHolds(srcLocationID: Int): List[Location] = {
    val locationOption = DBQueries.locations.find(_.id == srcLocationID)
    val adjacentLocations = 
      locationOption.map(DBQueries.getAdjacentLocationsForLocation(_)).flatten
    val provincialLocations =
      DBQueries.locations.filter(_ match {
        case Location(province, _) => 
          adjacentLocations.exists(_.province.equals(province))
      })
    val allUnits = DBQueries.getAllUnits
    val provincialLocationsWithUnitPresent =
      provincialLocations.filter(loc => allUnits.exists(_.unitLocation == loc.id))
    provincialLocationsWithUnitPresent
  }
	
  def getSupportHolds(srcDiplomacyUnit: DiplomacyUnit): List[Location] = 
    getSupportHolds(srcDiplomacyUnit.unitLocation)
	
  def getFormattedSupportHolds(srcLocationID: Int): List[String] =
    getSupportHolds(srcLocationID).map(getFormattedLocationName(_))
	  
  def getSupportHoldsMap(diplomacyUnits: List[DiplomacyUnit]) =
    diplomacyUnits.map(dpu => {
      val srcLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      srcLocationOption.map(loc => 
        (getFormattedLocationName(loc), 
         getSupportHolds(dpu.unitLocation).map(getFormattedLocationName(_))))
    }).flatten
	
  def getSupportMoves(srcLocationID: Int): List[(Location, List[Location])] = {
	val allUnitLocations: List[Location] = DBQueries.getAllUnits.map(dpu => 
	  DBQueries.locations.find(_.id == dpu.unitLocation)).flatten  
    
    val movesForAllOtherUnits: List[(Location, List[Location])] =
      allUnitLocations.filter(_.id != srcLocationID).map(loc =>
	  	(loc, getMoves(loc.id)))
	  	
	val movesForThisUnit = getMoves(srcLocationID)
	val movesThatMatterForOtherUnits = movesForAllOtherUnits.map(u => {
	  val otherUnitLocation: Location = u._1
	  val movesForOtherUnit: List[Location] = u._2
	  
	  val movesThatMatterForOtherUnit = movesForOtherUnit.filter(_ match {
	  	case Location(dstProvinceForOtherUnit, _) => movesForThisUnit.exists(_ match {
	  	  case Location(`dstProvinceForOtherUnit`, _) => true
	  	  case Location(_, _) => false
	  	})
	  })
	  
	  (otherUnitLocation, movesThatMatterForOtherUnit)
	})
	
	movesThatMatterForOtherUnits.filter(_ match {
	  case (_, movesThatMatter) => !movesThatMatter.isEmpty
	})
  }
	 
  def getSupportMoves(srcDiplomacyUnit: DiplomacyUnit): List[(Location, List[Location])] =
	getSupportMoves(srcDiplomacyUnit.unitLocation)
   
  def getFormattedSupportMoves(srcLocationID: Int): List[(String, List[String])] =
  	getSupportMoves(srcLocationID).map(u => 
      (getFormattedLocationName(u._1), u._2.map(getFormattedLocationName(_))))

  def getSupportMovesMap(diplomacyUnits: List[DiplomacyUnit]): 
    List[(String, List[(String, List[String])])] = 
      diplomacyUnits.map(dpu => {
        val srcLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
        srcLocationOption.map((srcLocation: Location) => 
            (getFormattedLocationName(srcLocation), 
                getFormattedSupportMoves(srcLocation.id)))
      }).flatten
  

  def getMovesByConvoy(diplomacyUnit: DiplomacyUnit): List[Location] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.ARMY, _, unitLocation, _, _) => 
        DBQueries.locations.filter(_.id == unitLocation).flatMap(loc => {
          val allPaths = findAllPathsExternal(loc, DBQueries.getAllFleetUnits)
          allPaths.map(_ match {
            case h :: tail => h
          })
        })
      case _ => Nil
    }
  }
  
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
      val hasLandLocation = DBQueries.hasLandLocation(currentLocation)
      
      if (isCurrentLocLandLocation && isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        val coastLocations = DBQueries.getCoastLocationsFromLandLocation(currentLocation)
        coastLocations.foldLeft(allPaths)((u, v) => {
          findAllPaths(v, allPaths, newPath)
        })
      } else if (isCurrentLocLandLocation && !isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        newPath :: allPaths
      } else if (!isCurrentLocLandLocation && isCurrentLocOnOriginProvince) {
        val adjLocations = DBQueries.getAdjacentLocationsForLocation(currentLocation)
        val oceanAdjLocations = adjLocations.filter(!DBQueries.hasLandLocation(_))
        val newPath = currentLocation :: presentPath
        oceanAdjLocations.foldLeft(allPaths)((u, v) => findAllPaths(v, u, newPath))
      } else if (!isCurrentLocOnOriginProvince && hasLandLocation) {
        val newPath = currentLocation :: presentPath;
        val landLocationOption = DBQueries.locations.find(_ match {
          case Location(currentLocation.province, Coast.NO_COAST) => true
          case _ => false
        })
        landLocationOption match {
          case Some(loc: Location) => findAllPaths(loc, allPaths, newPath)
          case None => allPaths
        }
      } else if (isCurrentLocOnPath) {
        allPaths
      } else if (!isCurrentLocOnPath && hasFleetUnit) {
        val newPath = currentLocation :: presentPath
        val adjLocations = DBQueries.getAdjacentLocationsForLocation(currentLocation)
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
        DBQueries.locations.find(_.id == unitLocation).flatMap(loc => {
          val isCoastal = DBQueries.hasLandLocation(loc)

          if (isCoastal) {
            None
          } else {
            Some(loc)
          }
        }).map(fleetLoc => {
          val allLandUnits: List[DiplomacyUnit] = DBQueries.getAllLandUnits
          val allFleetUnits: List[DiplomacyUnit] = DBQueries.getAllFleetUnits
          val allLandUnitLocations = allLandUnits.
            map((ldpu: DiplomacyUnit) => DBQueries.
                locations.find(_.id == ldpu.unitLocation)).
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
          case Some(x: List[(Location, List[Location])]) => x
          case None => Nil
        }
      }
      case _ => Nil
    }
  }
}