package controllers

// Checking this
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import play.core.Router
import play.api.mvc._
import play.api.templates._
import play.api.db.DB
import scala.xml._
import scalaz._
import scala.xml.Elem
import com.squeryl.jdip.queries._
import play.api.libs.concurrent._

class ApplicationAction[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Result = action.apply(request)
  def parser: BodyParser[A] = action.parser
}

object Application extends Controller with OptionTs {
  val SOURCE_LOCATION = "sourceLocation"
  val TARGET_LOCATION = "targetLocation"
  
  import play.api.Play._
  
  def index = new ApplicationAction(Action {
	  Ok(views.html.Application.index())
  })
	
  def players = new ApplicationAction(Action {
    Ok(views.html.Application.players(HeaderTitles.PLAYERS_TITLE, DBQueries.getPlayers))
  })
    
  def games = new ApplicationAction(Action { implicit request =>
  	val usernameOption = session.get(Security.username)
  	usernameOption.map(username => {
  	  val gamesForUser = DBQueries.getGamesForUser(username)
      val gameTimeIDs = gamesForUser.map(_.gameTimeID)
	    val gameTimesForUser = 
        DBQueries.getGameTimesForGames(gameTimeIDs)

		
	    val gamesWithGameTimes = 
	  	  (gamesForUser zip gameTimesForUser)
	    val gameScreenURL = controllers.routes.Application.gameScreen("").url
		  Ok(views.html.Application.games(gameScreenURL, gamesWithGameTimes))
  	}) match {
  	  case Some(x) => x
  	  case None => PreconditionFailed("No username entered")
  	}
  })
  
  private lazy val movementPhaseOrderTypes: List[OrderType] =
    DBQueries.orderTypes.filter((ot: OrderType) => 
      ot.phase.equals(Phase.MOVEMENT)
    )
    
  private lazy val fleetMovementPhaseOrderTypes: List[OrderType] =
    DBQueries.orderTypes.filter((ot: OrderType) =>
      DBQueries.orderTypeUnitTypes.exists((otut: OrderTypeUnitType) =>
        otut.orderType.equals(ot.id) && otut.unitType.equals(UnitType.FLEET)
      )
    )
    
  private lazy val armyMovementPhaseOrderTypes: List[OrderType] =
    DBQueries.orderTypes.filter((ot: OrderType) =>
      DBQueries.orderTypeUnitTypes.exists((otut: OrderTypeUnitType) =>
        otut.orderType.equals(ot.id) && otut.unitType.equals(UnitType.ARMY)  
      )  
    )
  
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
	  
	  val potentialMoveOrdersPromise = Akka.future {
	    DBQueries.getPotentialMoveOrdersForGamePlayerEmpireAtCurrentTime(gpe)
	  }
	  val potentialSupportHoldOrdersPromise = Akka.future {
	    DBQueries.getPotentialSupportHoldOrdersForGamePlayerEmpire(gpe)
	  }
	  val potentialSupportMoveOrdersPromise = Akka.future {
	    DBQueries.getPotentialSupportMoveOrdersForGamePlayerEmpire(gpe)
	  }
	  val potentialConvoyOrdersPromise = Akka.future {
	     DBQueries.getPotentialConvoyOrdersForGamePlayerEmpire(gpe)
	  }
	  
	  val diplomacyUnits: List[DiplomacyUnit] = 
        DBQueries.
          	getDiplomacyUnitsForGamePlayerEmpire(gpe)
      
      val locationIDs = diplomacyUnits.map(_.unitLocationID)
      val locations = 
        DBQueries.getLocationFromLocationIDs(locationIDs)
        
      val fleetOrderTypes = 
        fleetMovementPhaseOrderTypes.sortWith((a, b) => (a.id, b.id) match {
            case (OrderType.HOLD, _) => true
            case (_, OrderType.HOLD) => false
            case _ => true
          }).map(
          (ot: OrderType) => <option>{ot.id}</option>)
      val armyOrderTypes = armyMovementPhaseOrderTypes.
        sortWith((a, b) => (a.id, b.id) match {
            case (OrderType.HOLD, _) => true
            case (_, OrderType.HOLD) => false
            case _ => true
          }).map(
          (ot: OrderType) => <option>{ot.id}</option>)
          
      val tableRows = (diplomacyUnits zip locations) map (u => {
        <tr id="{u._1.id}">
    	  <td>{u._1.unitType}</td>
    	  <td>{u._2.presentationName}</td>
    	  <td>
    	  	<select>{u._1.unitType match {
    	  		case UnitType.ARMY => armyOrderTypes
    	  		case UnitType.FLEET => fleetOrderTypes
    	  	}}
    	  	</select>
    	  </td>
    	  <td class="{TARGET_LOCATION}"></td>
    	  <td class="{SOURCE_LOCATION}"></td>
    	</tr>
      })
      
      for (gameMapOption <- gameMapOptionPromise;
    	potentialMoveOrders <- potentialMoveOrdersPromise;
    	potentialSupportHoldOrders <- potentialSupportHoldOrdersPromise;
    	potentialSupportMoveOrders <- potentialSupportMoveOrdersPromise;
    	potentialConvoyOrders <- potentialConvoyOrdersPromise
      ) yield {
        gameMapOption.map((gameMap: GameMap) =>
          views.html.Application.gameScreen(tableRows, 
            potentialMoveOrders,
            potentialSupportHoldOrders,
            potentialSupportMoveOrders,
            potentialConvoyOrders,
            new String(gameMap.gameMap))
        ) match {
          case Some(view: Content) => Ok(view) 
          case None => PreconditionFailed("Failed to identify resources")
        }
      }
	}
  
  def gameScreen(gameName: String = "") = 
    new ApplicationAction(Action { implicit request =>
      import Scalaz._
  
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
