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
import play.api.libs.json._

class ApplicationAction[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Result = action.apply(request)
  def parser: BodyParser[A] = action.parser
}

object Application extends Controller with OptionTs {
  val SOURCE_LOCATION = "sourceLocation"
  val TARGET_LOCATION = "targetLocation"
  val UNIT_ORDER = "unitOrder"
  
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
        DBQueries.getGameTimesForGameTimeIDs(gameTimeIDs)

		
	    val gamesWithGameTimes = 
	  	  (gamesForUser zip gameTimesForUser)
	    val gameScreenURL = controllers.routes.GameScreenController.gameScreen("").url
		  Ok(views.html.Application.games(gameScreenURL, gamesWithGameTimes))
  	}) match {
  	  case Some(x) => x
  	  case None => PreconditionFailed("No username entered")
  	}
  })	  
}
