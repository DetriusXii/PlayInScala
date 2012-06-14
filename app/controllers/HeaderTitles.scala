/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package controllers

import play.mvc._
import play.templates._

object HeaderTitles {
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
