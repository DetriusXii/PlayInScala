package controllers


import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables.Players
import com.squeryl.jdip.tables.Empires
import com.squeryl.jdip.adapters.PostgreSqlAdapter
import play.mvc._
import scala.xml._
import play.templates._

import play.db.DB

trait Secure {
  self: Controller =>
  @Before 
  def checkSecurity = {
    session("username") match {
      case Some(x: String) => Continue
      case None => Action(Authentication.login)
    }
  }
}

object HeaderTitles {
  private def createURLRoutes: List[Tuple2[String, String]] = {

      val INDEX_TITLE = "Home"
      val INDEX = "Application.index"
      val PLAYERS_TITLE = "Players"
      val PLAYERS = "Application.players"
      val GAMES_TITLE = "Games"
      val GAMES = "Application.games"
      val EMPIRES_TITLE = "Empires"
      val EMPIRES = "Application.empires"
      val LOGOUT_TITLE = "Logout"
      val LOGOUT = "Authentication.logout"
    
      val TITLE_LIST = 
        INDEX_TITLE :: PLAYERS_TITLE :: GAMES_TITLE :: EMPIRES_TITLE :: LOGOUT_TITLE :: Nil
      val URL_ROUTES_TO = INDEX :: PLAYERS :: GAMES :: EMPIRES :: LOGOUT :: Nil
      
      return TITLE_LIST zip (URL_ROUTES_TO map ((u: String) => Router.reverse(u).url))
    }
    
    val headerTitlesAndURLs: List[Tuple2[String, String]] = createURLRoutes
}

object Application extends Controller with Secure{
    def index = views.Application.html.index()

    def players = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val players = Jdip.players
        val playersListItem = players map ((u: Players) => <li>{u.id}</li>)
        <html>
          <title>Players</title>
          <body>
            <ul>
              {playersListItem}
            </ul>
          </body>
        </html>
      }
    }
    
    def games = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val games = Jdip.games
        views.Application.html.games(games)
      }
    }
  
    def empires  = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val empires: Iterable[Empires] = Jdip.empires map ((u: Empires) => u)
        views.Application.html.empires(empires)
      }
    }
  
    def movement = {
      views.Application.html.movement()
    }
}
