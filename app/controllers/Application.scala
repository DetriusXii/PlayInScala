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

class ApplicationAction[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Result = action.apply(request)
  def parser: BodyParser[A] = action.parser
}

object Application extends Controller {
	import play.api.Play._
  
    def index = new ApplicationAction(Action {
	  Ok(views.html.Application.index())
	})
	

    def players = new ApplicationAction(Action {
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using (dbSession) {
          val players = Jdip.players
          Ok(views.html.Application.players(HeaderTitles.PLAYERS_TITLE, players))
        }
      })
      
    })
    
    def games = new ApplicationAction(Action { implicit request =>
      val username: String = session.get(Security.username) match {
        case Some(x: String) => x
        case _ => throw new Exception("No username in session")
      }
    
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using (dbSession) {
	        val gamePlayersForUser = from(Jdip.gamePlayers)((gp: GamePlayer) => 
	          where(gp.playerName === username) select(gp))
	        val gamesForUser = from(Jdip.games)((g: Game) => 
	          where(g.id in from(gamePlayersForUser)((gpu: GamePlayer) => select(gpu.gameName)))
	          select(g))
	          
	        val gameScreenURL = controllers.routes.Application.gameScreen("").url
	          
	        Ok(views.html.Application.games(gameScreenURL, gamesForUser))
        }
      })
    })
    
    def gameScreen(gameName: String = "") = new ApplicationAction(Action { implicit request =>
      val username: String = session.get(Security.username) match {
        case Some(x: String) => x
        case _ => throw new Exception("No username in session")
      }
      
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using(dbSession) {
          val game: Game = Jdip.games.lookup(gameName) match {
            case Some(g: Game) => g
            case _ => throw new Exception("No game found with game name %s" format gameName)
          }
          
          val player: Player = Jdip.players.lookup(username) match {
            case Some(p: Player) => p
            case _ => throw new Exception("No player found with player name %s" format username)
          }
          
          val gamePlayer: GamePlayer = from(Jdip.gamePlayers)((gp: GamePlayer) => {
            where(gp.gameName === game.id and gp.playerName === player.id) select(gp)
          }).first
          
          val gamePlayerEmpire: GamePlayerEmpire = from(Jdip.gamePlayerEmpires)((gpe: GamePlayerEmpire) => {
            where(gpe.gamePlayerKey === gamePlayer.id) select(gpe)
          }).first
          
          Ok("Hi")
        }
      })
    })
	
}
