/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package controllers

import play.api.mvc._
import play.api.templates._

object HeaderTitles {
  val INDEX_TITLE = "Home"
  val INDEX = routes.Application.index.url
  val PLAYERS_TITLE = "Players"
  val PLAYERS = routes.Application.players.url
  val GAMES_TITLE = "Games"
  val GAMES = routes.Application.games.url
  val LOGOUT_TITLE = "Logout"
  val LOGOUT = routes.Authentication.logout.url
  
  private def createURLRoutes: List[Tuple2[String, String]] = {
    
      val TITLE_LIST = 
        INDEX_TITLE :: PLAYERS_TITLE :: GAMES_TITLE :: LOGOUT_TITLE :: Nil
      val URL_ROUTES_TO = INDEX :: PLAYERS :: GAMES :: LOGOUT :: Nil
      
      return TITLE_LIST zip URL_ROUTES_TO
    }
    
    val headerTitlesAndURLs: List[Tuple2[String, String]] = createURLRoutes
}
