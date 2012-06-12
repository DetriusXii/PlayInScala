/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers


import play.mvc._
import scala.xml._
import play.mvc.results.ScalaAction
import play.templates._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables.Player
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import play.db._

object Authentication extends ScalaController {
  val USERNAME = "username"
  val AUTHENTICATION_MSG_KEY = "authenticationMsg"
  val AUTHENTICATE_NAME = "Authentication.authenticate"
  val NO_PASSWORD_MSG = "%s has entered an incorrect password"
  val NO_USER_MSG = "No user was found with the username %s"
  
  def login = views.html.login(Router.reverse(AUTHENTICATE_NAME).url)
  def noUserLogin(username: String) = 
    views.html.login(Router.reverse(AUTHENTICATE_NAME).url, NO_USER_MSG format username)
  
  def noPasswordLogin(username: String) = 
    views.html.login(Router.reverse(AUTHENTICATE_NAME).url, NO_PASSWORD_MSG format username)
    
  def authenticate(username: String = "", password: String = ""): ScalaAction = {
    if (username.isEmpty) {
      return Action(Authentication.login)
    }
    
    val dbSession = Session.create(DB.getConnection, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      val queriedPlayer = Jdip.players.lookup(username)
      queriedPlayer match {
        case Some(p: Player) => if (password.equals(p.password)) {
          session += USERNAME -> username
          Action(Application.index)
        } else {
          Action(Authentication.noPasswordLogin(username))
        }
        case None => Action(Authentication.noUserLogin(username))
      }
    }
  }
  
  def logout = {
    session.remove(USERNAME)
    Action(Authentication.login)
  }
}
