package controllers


import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables.Game
import com.squeryl.jdip.tables.GamePlayer
import com.squeryl.jdip.tables.Player
import com.squeryl.jdip.tables.Empire
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import play.mvc._
import scala.xml._
import play.templates._

import play.db.DB

trait Secure {
  self: Controller =>
  @Before 
  def checkSecurity = {
    session(Authentication.USERNAME) match {
      case Some(x: String) => Continue
      case None => Action(Authentication.login)
    }
  }
}

object Application extends ScalaController with Secure {
    val GAME_SCREEN = "Application.gameScreen"
  
    def index = views.Application.html.index()

    def players = {
      
      val session = Session.create(DB.getConnection, new RevisedPostgreSqlAdapter)
      using(session) {
        val players = Jdip.players
        views.Application.html.players(HeaderTitles.PLAYERS_TITLE, players)
      }
    }
    
    def games = {
      val username: String = session(Authentication.USERNAME) match {
        case Some(x: String) => x
        case _ => throw new Exception("No username in session")
      }
    
      val dbSession = Session.create(DB.getConnection, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        val gamePlayersForUser = from(Jdip.gamePlayers)((gp: GamePlayer) => 
          where(gp.playerName === username) select(gp))
        val gamesForUser = from(Jdip.games)((g: Game) => 
          where(g.id in from(gamePlayersForUser)((gpu: GamePlayer) => select(gpu.gameName)))
          select(g))
        val gameScreenURL = Router.reverse(GAME_SCREEN).url
          
        views.Application.html.games(gameScreenURL, gamesForUser)
      }
    }
    
    def gameScreen(gameName: String) = {
      val username: String = session(Authentication.USERNAME) match {
        case Some(x: String) => x
        case _ => throw new Exception("No username in session")
      }
      
      <b>Hello world</b>
    }
  
    def empires  = {
      val session = Session.create(DB.getConnection, new RevisedPostgreSqlAdapter)
      using(session) {
        val empires: Iterable[Empire] = Jdip.empires map ((u: Empire) => u)
        views.Application.html.empires(empires)
      }
    }
    
   
}
