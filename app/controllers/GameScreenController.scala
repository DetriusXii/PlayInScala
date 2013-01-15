package controllers
import play.api.libs.json._
import play.api.libs.json.Writes._
import com.squeryl.jdip.tables._
import play.api.libs.concurrent._
import com.squeryl.jdip.queries._
import play.api.mvc._
import play.api.Play._


object PotentialMoveOrderWrites extends 
  Writes[Tuple2[PotentialMoveOrder, Location]] {
  def getJSValuePromise(gpe: GamePlayerEmpire): Promise[JsValue] =
	Akka.future {
	  DBQueries.getPotentialMoveOrdersForGamePlayerEmpireAtCurrentTime(gpe)
	}.map((pmos: List[PotentialMoveOrder]) => {
	  val pmosWithLocations = pmos.map((pmo: PotentialMoveOrder) => {
		val locationOption = 
	      DBQueries.locations.find(_.id == pmo.moveLocationID)
	    locationOption.map((loc: Location) => (pmo, loc))
	  }).flatten
	    
	  Json.toJson(pmosWithLocations)(listWrites(this))
	})
    
  def writes(u: Tuple2[PotentialMoveOrder, Location]): JsValue =
	Json.toJson(Map(
	  GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString, 
	  GameScreenController.LOCATION_ID -> u._2.id.toString,
	  GameScreenController.PRESENTATION_NAME -> u._2.presentationName
	))
}
  
object PotentialSupportHoldOrderWrites extends 
  Writes[Tuple2[PotentialSupportHoldOrder, Location]] {
  def getJSValuePromise(gpe: GamePlayerEmpire): Promise[JsValue] = 
  	Akka.future {
	  DBQueries.getPotentialSupportHoldOrdersForGamePlayerEmpire(gpe)
	}.map((pshos: List[PotentialSupportHoldOrder]) => {
	  val pshosWithLocations = pshos.map((psho: PotentialSupportHoldOrder) => {
	  val locationOption =
	    DBQueries.locations.find(_.id == psho.supportHoldLocationID)
	      locationOption.map((loc: Location) => (psho, loc))
	  }).flatten
	    
	  Json.toJson(pshosWithLocations)(listWrites(this))
	})
    
  def writes(u: Tuple2[PotentialSupportHoldOrder, Location]): JsValue =
    Json.toJson(Map(
      GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString,
      GameScreenController.LOCATION_ID -> u._2.id.toString,
      GameScreenController.PRESENTATION_NAME -> u._2.presentationName
    ))
}

object PotentialSupportMoveOrderWrites extends
  Writes[Tuple3[PotentialSupportMoveOrder, Location, Location]] {
  def getJSValuePromise(gpe: GamePlayerEmpire): Promise[JsValue] =
    Akka.future {
	  DBQueries.getPotentialSupportMoveOrdersForGamePlayerEmpire(gpe)
    }.map((psmos: List[PotentialSupportMoveOrder]) => {
      val psmosWithLocations =
        psmos.map((psmo: PotentialSupportMoveOrder) => {
          val sourceLocationOption = 
            DBQueries.locations.find(_.id == psmo.supportMoveSourceLocationID)
          val targetLocationOption =
            DBQueries.locations.find(_.id == psmo.supportMoveTargetLocationID)
        
          for (sourceLocation <- sourceLocationOption;
        	  targetLocation <- targetLocationOption
          ) yield ((psmo, sourceLocation, targetLocation))
        }).flatten
      
      Json.toJson(psmosWithLocations)(listWrites(this))
	})
  
  def writes(u: Tuple3[PotentialSupportMoveOrder, Location, Location]):
    JsValue =
      Json.toJson(Map(
        GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString,
        GameScreenController.SOURCE_LOCATION_ID -> u._2.id.toString,
        GameScreenController.SOURCE_PRESENTATION_NAME -> u._2.presentationName,
        GameScreenController.TARGET_LOCATION_ID -> u._3.id.toString,
        GameScreenController.TARGET_PRESENTATION_NAME -> u._3.presentationName
      ))
}
  
object PotentialConvoyOrderWrites extends 
  Writes[Tuple3[PotentialConvoyOrder, Location, Location]] {
  def getJSValuePromise(gpe: GamePlayerEmpire): Promise[JsValue] =
    Akka.future({
	  DBQueries.getPotentialConvoyOrdersForGamePlayerEmpire(gpe)
    }).map((pcos: List[PotentialConvoyOrder]) => {
      val pcosWithLocations = pcos.map((pco: PotentialConvoyOrder) => {
        val sourceLocationOption = 
          DBQueries.locations.find(_.id == pco.convoySourceLocationID)
        val targetLocationOption =
          DBQueries.locations.find(_.id == pco.convoyTargetLocationID)
          
        for (sourceLocation <- sourceLocationOption;
        	targetLocation <- targetLocationOption
        ) yield ((pco, sourceLocation, targetLocation))
         
      }).flatten
      
      Json.toJson(pcosWithLocations)(listWrites(this))
    })
    
  
  def writes(u: Tuple3[PotentialConvoyOrder, Location, Location]): JsValue =
	Json.toJson(Map(
	    GameScreenController.DIPLOMACY_UNIT_ID -> 
	    	u._1.diplomacyUnitID.toString,
		GameScreenController.SOURCE_LOCATION_ID -> u._2.id.toString,
		GameScreenController.SOURCE_PRESENTATION_NAME -> 
			u._2.presentationName,
		GameScreenController.TARGET_LOCATION_ID -> u._3.id.toString,
		GameScreenController.TARGET_PRESENTATION_NAME -> u._3.presentationName
	))
}
  
object GameScreenController extends Controller {
  val TARGET_LOCATION = "targetLocation"
  val SOURCE_LOCATION = "sourceLocation"
  val SOURCE_UNIT = "sourceUnit"
  val TARGET_UNIT = "targetUnit"
  val UNIT_ORDER = "unitOrder"
  val LOCATION_ID = "locationID"
  val PRESENTATION_NAME = "presentationName"
  val DIPLOMACY_UNIT_ID = "diplomacyUnitID"
  val SOURCE_LOCATION_ID = "sourceLocationID"
  val TARGET_LOCATION_ID = "targetLocationID"
  val SOURCE_PRESENTATION_NAME = "sourcePresentationName"
  val TARGET_PRESENTATION_NAME = "targetPresentationName"
  val DIPLOMACY_UNIT_ROW = "diplomacyUnitRow"
  
  private lazy val movementPhaseOrderTypes: List[OrderType] =
    DBQueries.orderTypes.filter((ot: OrderType) => 
      ot.phase.equals(Phase.MOVEMENT)
    )
    
  private lazy val fleetMovementPhaseOrderTypes: List[OrderType] =
    movementPhaseOrderTypes.filter((ot: OrderType) =>
      DBQueries.orderTypeUnitTypes.exists((otut: OrderTypeUnitType) =>
        otut.orderType.equals(ot.id) && otut.unitType.equals(UnitType.FLEET)
      )
    )
    
  private lazy val armyMovementPhaseOrderTypes: List[OrderType] =
    movementPhaseOrderTypes.filter((ot: OrderType) =>
      DBQueries.orderTypeUnitTypes.exists((otut: OrderTypeUnitType) =>
        otut.orderType.equals(ot.id) && otut.unitType.equals(UnitType.ARMY)  
      )  
    )
 
  private def getPotentialMoveOrders(gpe: GamePlayerEmpire): 
	  Promise[List[PotentialMoveOrder]] =
    Akka.future {
	  DBQueries.getPotentialMoveOrdersForGamePlayerEmpireAtCurrentTime(gpe)
	}
  private def getPotentialSupportHoldOrders(gpe: GamePlayerEmpire):
	  Promise[List[PotentialSupportHoldOrder]] =
	Akka.future {
	  DBQueries.getPotentialSupportHoldOrdersForGamePlayerEmpire(gpe)	
  	}
  private def getPotentialSupportMoveOrders(gpe: GamePlayerEmpire):
	Promise[List[PotentialSupportMoveOrder]] =
	  Akka.future {
	  	DBQueries.getPotentialSupportMoveOrdersForGamePlayerEmpire(gpe)
  	  }
  private def getPotentialConvoyOrders(gpe: GamePlayerEmpire):
	Promise[List[PotentialConvoyOrder]] = 
	  Akka.future {
	  	DBQueries.getPotentialConvoyOrdersForGamePlayerEmpire(gpe)
  	  }
  
    
  private def sortAndTransformOrderTypes(orderTypes: List[OrderType]): 
    List[scala.xml.Elem] = orderTypes.sortWith((a, b) => (a.id, b.id) match {
      case (OrderType.HOLD, _) => true
      case (_, OrderType.HOLD) => false
      case (OrderType.MOVE, _) => true
      case (_, OrderType.MOVE) => false
      case _ => true
    }).map((ot: OrderType) => <option>{ot.id}</option>)
    
    
  private def getTableRows(gpe: GamePlayerEmpire): 
    List[scala.xml.Elem] = {
    val diplomacyUnits: List[DiplomacyUnit] = 
        DBQueries.
          	getDiplomacyUnitsForGamePlayerEmpire(gpe)
      
    val locationIDs = diplomacyUnits.map(_.unitLocationID)
    val locations = 
      DBQueries.getLocationForLocationIDs(locationIDs)
        
    val fleetOrderTypes = 
	  sortAndTransformOrderTypes(fleetMovementPhaseOrderTypes)
	val armyOrderTypes = 
	  sortAndTransformOrderTypes(armyMovementPhaseOrderTypes)
	  
	  
	  
	val tableRows = (diplomacyUnits zip locations) map (u => {
        <tr id={ u._1.id.toString } class={ DIPLOMACY_UNIT_ROW } >
    	  <td>{u._1.unitType}</td>
    	  <td>{u._2.presentationName}</td>
    	  <td>
    	  	<select class={ UNIT_ORDER }>{u._1.unitType match {
    	  		case UnitType.ARMY => armyOrderTypes
    	  		case UnitType.FLEET => fleetOrderTypes
    	  	}}
    	  	</select>
    	  </td>
    	  <td class={ TARGET_LOCATION }></td>
    	  <td class={ SOURCE_LOCATION }></td>
    	</tr>
      })
      
    tableRows  
  }
    
  private def prepareStatus(gpe: GamePlayerEmpire): 
	Promise[SimpleResult[_]] = {
	  val gameOptionPromise = Akka.future { 
        DBQueries.getGameForGamePlayerEmpireID(gpe.id)
	  }
	  val gameMapOptionPromise: Promise[Option[GameMap]] = 
	    gameOptionPromise.map((gameOption: Option[Game]) =>
        gameOption.flatMap((game: Game) =>
          DBQueries.getGameMapForGameAtCurrentTime(game))
      )
	  
	  val potentialMoveOrderJSValue =
	    PotentialMoveOrderWrites.getJSValuePromise(gpe)
	  val potentialSupportHoldOrderJSValue =
	    PotentialSupportHoldOrderWrites.getJSValuePromise(gpe)
	  val potentialSupportMoveOrderJSValue =
	    PotentialSupportMoveOrderWrites.getJSValuePromise(gpe)
	  val potentialConvoyOrderJSValue =
	    PotentialConvoyOrderWrites.getJSValuePromise(gpe)
	  
	  
      val tableRows = getTableRows(gpe)
      
      
      for (gameMapOption <- gameMapOptionPromise;
    	potentialMoveOrders <- potentialMoveOrderJSValue;
    	potentialSupportHoldOrders <- potentialSupportHoldOrderJSValue;
    	potentialSupportMoveOrders <- potentialSupportMoveOrderJSValue;
    	potentialConvoyOrders <- potentialConvoyOrderJSValue
      ) yield {
        gameMapOption.map((gameMap: GameMap) =>
          views.html.Application.gameScreen(tableRows, 
            potentialMoveOrders,
            potentialSupportHoldOrders,
            potentialSupportMoveOrders,
            potentialConvoyOrders,
            new String(gameMap.gameMap),
            OrderHandler.SUBMIT_MOVE_ORDERS_URL  
          )
        ) match {
          case Some(view: Content) => Ok(view) 
          case None => PreconditionFailed("Failed to identify resources")
        }
      }
	}  
    
  def gameScreen(gameName: String = "") = 
    new ApplicationAction(Action { implicit request =>
  
      val usernameOption = session.get(Security.username)
      val gamePlayerEmpireOption = usernameOption.flatMap((username: String) => {
        DBQueries.getGamePlayerEmpire(gameName, username)
      })
          	
      Async {
        gamePlayerEmpireOption match {
          case Some(gpe: GamePlayerEmpire) => 
            prepareStatus(gpe)
          case None => 
            Akka.future {
              PreconditionFailed("GamePlayerEmpire does not exist")
            }
        }
      }
    })
}