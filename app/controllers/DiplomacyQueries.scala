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
    getMoves(srcDiplomacyUnit.unitLocationID)
    
  def getTotalMoves(game: Game)(srcDiplomacyUnit: DiplomacyUnit): List[Location] = {
    val regularMoves = getMoves(srcDiplomacyUnit)
    val movesByConvoy = getMovesByConvoy(game)(srcDiplomacyUnit)
    
    val filteredMovesByConvoy = movesByConvoy.filter(loc =>
      !regularMoves.exists(_.id == loc.id)
    )
    
    regularMoves ++ filteredMovesByConvoy
  }
      
  def getMoveOrdersMap(game: Game)(diplomacyUnits: List[DiplomacyUnit]) = 
    diplomacyUnits.map(dpu => {
      val allMoves = getTotalMoves(game)(dpu)
      val allFormattedMoves = allMoves.map(_.presentationName)
      
      val srcLocation = dpu.unitLocation
      
      val srcLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      srcLocationOption.map(loc => 
        (loc.presentationName, allFormattedMoves))
       
    }).flatten
  
  def getSupportHolds(game: Game)(srcLocationID: Int): List[Location] = {
    val locationOption = DBQueries.locations.find(_.id == srcLocationID)
    val adjacentLocations = 
      locationOption.map(DBQueries.getAdjacentLocationsForLocation(_)).flatten
    val provincialLocations =
      DBQueries.locations.filter(_ match {
        case Location(province, _) => 
          adjacentLocations.exists(_.province.equals(province))
      })
    val allUnits = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    val provincialLocationsWithUnitPresent =
      provincialLocations.filter(loc => allUnits.exists(_.unitLocation == loc.id))
    provincialLocationsWithUnitPresent
  }
	
  def getSupportHolds(game: Game, srcDiplomacyUnit: DiplomacyUnit): List[Location] = 
    getSupportHolds(game)(srcDiplomacyUnit.unitLocationID)
	
  def getFormattedSupportHolds(game: Game, srcLocationID: Int): List[String] =
    getSupportHolds(game)(srcLocationID).map(_.presentationName)
	  
  def getSupportHoldsMap(game: Game)(diplomacyUnits: List[DiplomacyUnit]) =
    diplomacyUnits.map(dpu => {
      val srcLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      srcLocationOption.map(loc => 
        (loc.presentationName, 
         getSupportHolds(game)(dpu.unitLocationID).map(_.presentationName)))
    }).flatten
	
  def getSupportMoves(game: Game)(diplomacyUnit: DiplomacyUnit): List[(Location, List[Location])] = {
    val allOtherUnits = 
      DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game).
        	filter(_.id != diplomacyUnit.id)
     
    
    val movesForAllOtherUnits: List[(Location, List[Location])] = 
      allOtherUnits.map(dpu => {
        val otherUnitLocation = DBQueries.locations.find(_.id == dpu.unitLocation)
        otherUnitLocation.map((_, getTotalMoves(game)(dpu)))
      }).flatten
    
    val regularMovesForThisUnit = getMoves(diplomacyUnit)
    
    val movesThatMatterForAllOtherUnits = movesForAllOtherUnits.map(u => {
      val otherUnitLocation = u._1
      val totalMovesForOtherUnits = u._2
      
      val tMovesForOtherUnitsWhereThisUnitCanReach =
        totalMovesForOtherUnits.filter(loc => regularMovesForThisUnit.exists(_ match {
          case Location(loc.province, _) => true
          case _ => false
        }))
        
      (otherUnitLocation, tMovesForOtherUnitsWhereThisUnitCanReach)
    })
    
    val filteredMovesThatMatterForAllOtherUnits = 
      movesThatMatterForAllOtherUnits.filter(!_._2.isEmpty)
    filteredMovesThatMatterForAllOtherUnits
  } 

  def getSupportMovesMap(game: Game)(diplomacyUnits: List[DiplomacyUnit]): 
	  List[(String, List[(String, List[String])])] = {
    
    val allSupportMovesForUnits = diplomacyUnits.map(dpu => {
      val unitLocationOption = DBQueries.locations.find(_.id == dpu.unitLocationID)
      
      unitLocationOption.map(loc => 
        (loc, getSupportMoves(game)(dpu))
      )
    }).flatten
    
    val allStringFormattedSupportMovesForUnits =
      allSupportMovesForUnits.map(u => {
        val stringFormattedUnitLocation = u._1.presentationName
        val stringFormattedOtherUnits = u._2.map(v => {
          val stringFormattedUnitMovingLocation = v._1.presentationName
          val stringFormattedMoveDestinations = v._2.map(_.presentationName)
          
          (stringFormattedUnitMovingLocation, stringFormattedMoveDestinations)
        })
        
        (stringFormattedUnitLocation, stringFormattedOtherUnits)
      })
  
    allStringFormattedSupportMovesForUnits
  }
  

  def getMovesByConvoy(game: Game)(diplomacyUnit: DiplomacyUnit): List[Location] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.ARMY, _, unitLocation, _, _) => 
        DBQueries.locations.filter(_.id == unitLocation).flatMap(loc => {
          val allPaths = 
            findAllPathsExternal(loc, DBQueries.getAllFleetUnitsForGame(game))
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


  def getConvoys(game: Game)(diplomacyUnit: DiplomacyUnit): 
	  List[(Location, List[Location])] = {
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
          val allLandUnits: List[DiplomacyUnit] = 
            DBQueries.getAllLandUnitsForGame(game)
          val allFleetUnits: List[DiplomacyUnit] = 
            DBQueries.getAllFleetUnitsForGame(game)
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
  
  def getConvoysMap(game: Game)(diplomacyUnits: List[DiplomacyUnit]): 
	  List[(String, List[(String, List[String])])] = {
    val convoysForUnits = diplomacyUnits.map(dpu => {
      val convoysForUnit = getConvoys(game)(dpu)
      val stringFormattedConvoys = convoysForUnit.map(u => {
        val unitLocation = u._1
        val movesForUnit = u._2
        
        val stringFormattedUnitLocation = unitLocation.presentationName
        val stringFormattedMoves =
          movesForUnit.map(_.presentationName)
        (stringFormattedUnitLocation, stringFormattedMoves)
      }).filter(!_._2.isEmpty)
      
      
      val unitLocationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      unitLocationOption.map(loc => {
        val stringFormattedFleetUnitLocation = loc.presentationName
        (stringFormattedFleetUnitLocation, stringFormattedConvoys)
      })
    })
    
    convoysForUnits.flatten
  }
  
  def getArmyMovementPhaseOrderTypes(): List[scala.xml.Elem] = {
	getMovementPhaseOrderTypesForUnitType(UnitType.ARMY)
  }
  
  def getFleetMovementPhaseOrderTypes(): List[scala.xml.Elem] = {
	getMovementPhaseOrderTypesForUnitType(UnitType.FLEET)
  }

  def getMovementPhaseOrderTypesForUnitType(unitType: String): List[scala.xml.Elem] = {
	val orderTypes =
	  DBQueries.orderTypes.filter(_.phase.equals(Phase.MOVEMENT))
	val orderTypesForUnitTypes =
      DBQueries.orderTypeUnitTypes.filter(otut =>
      	orderTypes.exists(_.id.equals(otut.orderType))
      ).filter(_.unitType.equals(unitType))
  
    orderTypesForUnitTypes.sortWith((a, b) => (a.orderType, b.orderType) match {
      case (OrderType.HOLD, _) => true
      case (_, OrderType.HOLD) => false
      case _ => true
    }).map(otut => generateOptionFromOrderType(otut.orderType))
  }
 
  def generateOptionFromOrderType(orderType: String): scala.xml.Elem =
	<option>{orderType}</option>
}