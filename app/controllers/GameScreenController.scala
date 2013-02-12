package controllers
import play.api.libs.json._
import play.api.libs.json.Writes._
import com.squeryl.jdip.tables._
import play.api.libs.concurrent._
import com.squeryl.jdip.queries._
import play.api.mvc._
import play.api.Play._
import controllers.json._

object GameScreenController extends Controller {
  val FIRST_LOCATION = "firstLocation"
  val SECOND_LOCATION = "secondLocation"
  val UNIT_ORDER = "unitOrder"
  val LOCATION_ID = "locationID"
  val GAME_PLAYER_EMPIRE_ID = "gamePlayerEmpireID"
  val PRESENTATION_NAME = "presentationName"
  val DIPLOMACY_UNIT_ID = "diplomacyUnitID"
  val SUPPORT_LOCATION_ID = "supportLocationID"
  val TARGET_LOCATION_ID = "targetLocationID"
  val SUPPORT_PRESENTATION_NAME = "supportPresentationName"
  val TARGET_PRESENTATION_NAME = "targetPresentationName"
  val SUPPORT_SELECT = "supportSelect"
  val TARGET_SELECT = "targetSelect"
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
    
  private def getAddedDisabledOrderTypes(optionTypes: List[scala.xml.Elem],
      pshos: List[PotentialSupportHoldOrder],
      psmos: List[PotentialSupportMoveOrder],
      pcos: List[PotentialConvoyOrder])(unitId: Int): List[scala.xml.Elem] = {
    optionTypes.map(optionOrder => optionOrder.text match {
      case OrderType.SUPPORT_HOLD => 
          if (!pshos.filter(_.diplomacyUnitID == unitId).isEmpty) {
	        optionOrder
	      } else { 
	        <option disabled="disabled" >{ optionOrder.text }</option> 
	      }
      case OrderType.SUPPORT_MOVE =>
        if (!psmos.filter(_.diplomacyUnitID == unitId).isEmpty) {
          optionOrder
        } else {
          <option disabled="disabled" >{ optionOrder.text }</option>
        }
      case OrderType.CONVOY =>
        if (!pcos.filter(_.diplomacyUnitID == unitId).isEmpty) {
          optionOrder
        } else {
          <option disabled="disabled">{ optionOrder.text }</option>
        }
      case _ => optionOrder
    })
  }
    
    
  private def getTableRows(gpe: GamePlayerEmpire,
		  pshos: List[PotentialSupportHoldOrder],
		  psmos: List[PotentialSupportMoveOrder],
		  pcos: List[PotentialConvoyOrder]
  ): List[scala.xml.Elem] = {
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
	  
	val fleetOrderTypesCurried = 
	  getAddedDisabledOrderTypes(fleetOrderTypes, pshos, psmos, pcos) _
	val armyOrderTypesCurried =
	  getAddedDisabledOrderTypes(armyOrderTypes, pshos, psmos, pcos) _
	  
      
	val tableRows = (diplomacyUnits zip locations) map (u => {
	  val orderForUnitName = 
	    "%s%s" format (MovementPhaseOrderHandler.ORDER_PREFIX, 
	        u._1.id.toString)
	  
        <tr id={ u._1.id.toString } class={ DIPLOMACY_UNIT_ROW } >
    	  <td>{u._1.unitType}</td>
    	  <td class={ PRESENTATION_NAME }>{u._2.presentationName}</td>
    	  <td>
    	  	<select class={ UNIT_ORDER } name={ orderForUnitName }>{u._1.unitType match {
    	  		case UnitType.ARMY => armyOrderTypesCurried(u._1.id)
    	  		case UnitType.FLEET => fleetOrderTypesCurried(u._1.id)
    	  	}}
    	  	</select>
    	  </td>
    	  <td class={ FIRST_LOCATION }></td>
    	  <td class={ SECOND_LOCATION }></td>
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
	  
      val pmosPromise = getPotentialMoveOrders(gpe)
	  val pshosPromise = getPotentialSupportHoldOrders(gpe)
	  val psmosPromise = getPotentialSupportMoveOrders(gpe)
	  val pcosPromise = getPotentialConvoyOrders(gpe)
      
	  val pmosJSValuePromise = pmosPromise.map(pmos => 
	    new PotentialMoveOrderWrites(pmos).getJSValue
	  )
	  val pshosJSValuePromise = pshosPromise.map(pshos => 
	    new PotentialSupportHoldOrderWrites(pshos).getJSValue)
	  val psmosJSValuePromise = psmosPromise.map(psmos =>
	  	new PotentialSupportMoveOrderWrites(psmos).getJSValue
	  )
	  val pcosJSValuePromise = pcosPromise.map(pcos => 
	  	new PotentialConvoyOrderWrites(pcos).getJSValue
	  )
	  val upnJSValuePromise = Akka.future {
	    UniqueProvinceNameWrites.getJSValue
	  }
	  
	  
      val tableRowsPromise = for (pshos <- pshosPromise;
    		  				psmos <- psmosPromise;
    		  				pcos <- pcosPromise) yield {
        getTableRows(gpe, pshos, psmos, pcos)
      }
      
      
      for (gameMapOption <- gameMapOptionPromise;
    	potentialMoveOrders <- pmosJSValuePromise;
    	potentialSupportHoldOrders <- pshosJSValuePromise;
    	potentialSupportMoveOrders <- psmosJSValuePromise;
    	potentialConvoyOrders <- pcosJSValuePromise;
    	uniqueProvinceNames <- upnJSValuePromise;
    	tableRows <- tableRowsPromise
      ) yield {
        gameMapOption.map((gameMap: GameMap) =>
          views.html.Application.gameScreen(tableRows, 
            potentialMoveOrders,
            potentialSupportHoldOrders,
            potentialSupportMoveOrders,
            potentialConvoyOrders,
            uniqueProvinceNames,
            new String(gameMap.gameMap),
            MovementPhaseOrderHandler.SUBMIT_MOVE_ORDERS_URL,
            gpe.id.toString
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