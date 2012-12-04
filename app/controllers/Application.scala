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
import com.squeryl.jdip.creators.PotentialMoveOrderCreator
import com.squeryl.jdip.creators.PotentialSupportHoldOrderCreator
import com.squeryl.jdip.creators.PotentialSupportMoveOrderCreator
import com.squeryl.jdip.creators.PotentialConvoyOrderCreator

class ApplicationAction[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Result = action.apply(request)
  def parser: BodyParser[A] = action.parser
}

object Application extends Controller with OptionTs {
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
      diplomacyUnits: List[DiplomacyUnit]): List[Status] =
    gameOption.map((game: Game) => {
      
      val potentialMoveOrders = DBQueries.getPotentialMoveOrders(game)
      val potentialSupportHoldOrders = DBQueries.getPotentialSupportHoldOrders(game)
      val potentialSupportMoveOrders = DBQueries.getPotentialSupportMoveOrders(game)
      val potentialConvoyOrders = DBQueries.getPotentialConvoyOrders(game)
        
      val locationIDs = diplomacyUnits.map(_.unitLocationID)
      val locations = 
        DBQueries.dbQueries.getLocationFromLocationIDs(locationIDs)
      
        
      val fleetOrderTypes = DBQueries.fleetMovementPhaseOrderTypes
      val armyOrderTypes = DBQueries.armyMovementPhaseOrderTypes
      
      val tableRow = (diplomacyUnits zip locations) map (u => {
        <tr id="{u._1.id}">
    	  <td>{u._1.unitType}</td>
    	  <td>{u._2.presentationName}</td>
    	  <td></td>
    	  <td></td>
    	  <td></td>
    	</tr>
      })
        
      views.html.Application.gameScreen(diplomacyUnits, 
          locations, 
          potentialMoveOrders, 
          potentialSupportHoldOrders, 
          potentialSupportMoveOrders,
          potentialConvoyOrders,
          )
    }) match {
      case Some
      case None => PreconditionFailed("Failed to identify resources")
    }
  
  def gameScreen(gameName: String = "") = 
    new ApplicationAction(Action { implicit request =>
      import Scalaz._
  
      
      
      val usernameOption = session.get(Security.username)
      val gamePlayerEmpireOption = usernameOption.map((username: String) => {
        DBQueries.dbQueries.getGamePlayerEmpire(gameName, username)
      })
      val gameOption = gamePlayerEmpireOption.map(_ => 
        DBQueries.dbQueries.getGame(gameName))
      val diplomacyUnits: List[DiplomacyUnit] = 
        gamePlayerEmpireOption.map((gpe: GamePlayerEmpire) =>
        DBQueries.dbQueries.getDiplomacyUnitsForGamePlayerEmpire(gpe)).flatten
      
      

      diplomacyUnitsValidation match {
        case Success((game, diplomacyUnits: List[_])) => {
          val potentialMoveOrderCreator = 
            new PotentialMoveOrderCreator(game, DBQueries.dbQueries)
          val potentialSupportHoldOrderCreator =
            new PotentialSupportHoldOrderCreator(game, DBQueries.dbQueries)
          val potentialSupportMoveOrderCreator =
            new PotentialSupportMoveOrderCreator(game, DBQueries.dbQueries)
          val potentialConvoyOrderCreator =
            new PotentialConvoyOrderCreator(game, DBQueries.dbQueries)
          
          val potentialMoveOrders =
            potentialMoveOrderCreator.createPotentialMoveOrders
          val potentialSupportHoldOrders =
            potentialSupportHoldOrderCreator.createPotentialSupportHoldOrders
          val potentialSupportMoveOrders =
            potentialSupportMoveOrderCreator.createPotentialSupportMoveOrders
          val potentialConvoyOrders =
            potentialConvoyOrdersCreator.createPotentialConvoyOrders
            
          val locationIDs = diplomacyUnits.map(_.unitLocationID)
          val locations = 
            DBQueries.dbQueries.getLocationsFromLocationIDs(locationIDs)
          
            
          Ok(views.html.Application.gameScreen(diplomacyUnits,
              locations,
                  moveOrdersMap,
                  supportHoldsMap,
                  supportMovesMap,
                  convoysMap,
                  DBQueries.fleetMovementPhaseOrderTypes,
                  DBQueries.armyMovementPhaseOrderTypes, 
                  JdipSVGRenderer.getRenderedDocument(game)))
        }
        case Failure(e: Exception) => Ok(e.getMessage)
      }
    })

	  
}
