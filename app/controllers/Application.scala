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
  	  val gamesForUser = DBQueries.dbQueries.getGamesForUser(username)
      val gameTimeIDs = gamesForUser.map(_.gameTimeID)
	    val gameTimesForUser = 
        DBQueries.dbQueries.getGameTimesForGames(gameTimeIDs)

		
	    val gamesWithGameTimes = 
	  	  (gamesForUser zip gameTimesForUser)
	    val gameScreenURL = controllers.routes.Application.gameScreen("").url
		  Ok(views.html.Application.games(gameScreenURL, gamesWithGameTimes))
  	}) match {
  	  case Some(x) => x
  	  case None => PreconditionFailed("No username entered")
  	}
  })
    
  
  private def prepareStatus(gameOption: Option[Game],
      gameMap: Promise[Option[GameMap]],
      diplomacyUnits: List[DiplomacyUnit]): Promise[SimpleResult[_]] =
    gameOption.flatMap((game: Game) => {
      val potentialMoveOrdersPromise = 
        DBQueries.getPotentialMoveOrders(game)
      val potentialSupportHoldOrders = 
        DBQueries.getPotentialSupportHoldOrders(game)
      val potentialSupportMoveOrders = 
        DBQueries.getPotentialSupportMoveOrders(game)
      val potentialConvoyOrders = 
        DBQueries.getPotentialConvoyOrders(game)
        
      val locationIDs = diplomacyUnits.map(_.unitLocationID)
      val locations = 
        DBQueries.getLocationFromLocationIDs(locationIDs)
      
        
      val fleetOrderTypes = 
        DBQueries.
          fleetMovementPhaseOrderTypes.sortWith((a, b) => (a.id, b.id) match {
            case (OrderType.HOLD, _) => true
            case (_, OrderType.HOLD) => false
            case _ => true
          }).map(
          (ot: OrderType) => <option>{ot.id}</option>)
      val armyOrderTypes = DBQueries.armyMovementPhaseOrderTypes.
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
        
      val gameMapOption =
        DBQueries.dbQueries.getGameMapForGameAtCurrentTime(game)
      gameMapOption.map((gameMap: GameMap) => {
        val stringXML = new String(gameMap.gameMap)
        //val gameMapElem = scala.xml.XML.loadString(stringXML)
        
        views.html.Application.gameScreen(tableRows, 
            potentialMoveOrders,
            potentialSupportHoldOrders,
            potentialSupportMoveOrders,
            potentialConvoyOrders,
            stringXML)
      })
    }) match {
      case Some(view: Content) => Ok(view)
      case None => PreconditionFailed(Txt("Failed to identify resources"))
    }
  
  def gameScreen(gameName: String = "") = 
    new ApplicationAction(Action { implicit request =>
      import Scalaz._
  
      
      
      val usernameOption = session.get(Security.username)
      val gamePlayerEmpireOption = usernameOption.flatMap((username: String) => {
        DBQueries.dbQueries.getGamePlayerEmpire(gameName, username)
      })
      val gameOption = gamePlayerEmpireOption.flatMap(_ => 
        DBQueries.dbQueries.getGame(gameName))
      val gameMapPromise: Promise[Option[GameMap]] = Akka.future {
        gameOption.flatMap((game: Game) =>
          DBQueries.dbQueries.getGameMapForGameAtCurrentTime(game))
      }
      val diplomacyUnits: List[DiplomacyUnit] = 
        gamePlayerEmpireOption.map((gpe: GamePlayerEmpire) =>
          DBQueries.
          	dbQueries.
          	getDiplomacyUnitsForGamePlayerEmpire(gpe)).
          	flatten[DiplomacyUnit].toList
          	
      Async {
        prepareStatus(gameOption, diplomacyUnits)
      }
    })

	  
}
