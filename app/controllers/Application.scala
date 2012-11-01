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
  	  val gamesForUser = DBQueries.getGamesForUser(username)
	  val gameTimesForUser = DBQueries.getGameTimesForGames(gamesForUser.map(_.gameTime))
		
	  val gamesWithGameTimes = 
	  	(gamesForUser zip gameTimesForUser)
	  val gameScreenURL = controllers.routes.Application.gameScreen("").url
		Ok(views.html.Application.games(gameScreenURL, gamesWithGameTimes))
  	}) match {
  	  case Some(x) => x
  	  case None => PreconditionFailed("No username entered")
  	}
  })
	

  implicit def toOptionTFromOption[A](option: Option[A]): OptionT[List, A] = 
    optionT[List](option :: Nil)

  def gameScreen(gameName: String = "") = 
    new ApplicationAction(Action { implicit request =>
      import Scalaz._
  
      val usernameValidation: Validation[Exception, String] = 
        session.get(Security.username).toSuccess(new Exception("No username in session"))
      
      def getGamePlayerEmpireValidation(gameName: String, playerName: String) =
        DBQueries.getGamePlayerEmpire(gameName, playerName).
        toSuccess(
          new Exception(
            "No GamePlayerEmpireFound with game name %s and player name %s" format 
            (gameName, playerName)
          ))

      val diplomacyUnitsValidation = for (
        username <- usernameValidation;
        gamePlayerEmpire <- getGamePlayerEmpireValidation(gameName, username)) yield {
        DBQueries.getDiplomacyUnits(gamePlayerEmpire)
      }

      diplomacyUnitsValidation match {
        case Success(diplomacyUnits: List[_]) => {
          val moveOrdersMap = DiplomacyQueries.getMoveOrdersMap(diplomacyUnits)
          val supportHoldsMap = DiplomacyQueries.getSupportHoldsMap(diplomacyUnits)
          val supportMovesMap = DiplomacyQueries.getSupportMovesMap(diplomacyUnits)
          val convoysMap = DiplomacyQueries.getConvoysMap(diplomacyUnits)
          
          Ok(views.html.Application.gameScreen(getGameScreenData(diplomacyUnits),
                  moveOrdersMap,
                  supportHoldsMap,
                  convoysMap,
                  DiplomacyQueries.getFleetMovementPhaseOrderTypes(), 
                  DiplomacyQueries.getArmyMovementPhaseOrderTypes()))
        }
        case Failure(e: Exception) => Ok(e.getMessage)
      }
    })
	
	
  
	
	def pathsPic(): Action[AnyContent] = Action {
	  val diplomacyUnitOption = DBQueries.locations.find(_ match {
	    case Location("eng", Coast.ALL_COAST) => true
	    case _ => false
	  }).flatMap(DBQueries.getDiplomacyUnit(_))
	  diplomacyUnitOption.map(DiplomacyQueries.getConvoys(_)) match {
      	case Some(x) => Ok(x.toString)
      	case None => Ok("Could not reproduce")
	  }
	}

  def getSVGMap = Action { implicit request =>
    val svgMap = JdipSVGRenderer.getRenderedDocument
	  Ok(svgMap)
  }

	private def getGameScreenData(diplomacyUnits: List[DiplomacyUnit]): 
    List[Tuple2[String, String]] =
    diplomacyUnits.map((dpu: DiplomacyUnit) =>
      DBQueries.locations.find(_.id == dpu.unitLocation).
        map(DiplomacyQueries.getFormattedLocationName(_)).
        map((dpu.unitType, _))       
    ).flatten
	  
}
