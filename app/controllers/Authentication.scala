/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers


import scala.xml._
import org.squeryl.{Session => DBSession, _}
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables.Player
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import play.api.mvc._
import play.api.templates._
import play.api.db.DB
import play.core.Router.StaticPart
import com.squeryl.jdip.queries._
import play.api.data._

object Authentication extends Controller {
  import play.api.Play._
  
  val NO_PASSWORD_MSG = "%s has entered an incorrect password"
  val NO_USER_MSG = "No user was found with the username %s"
  val PLEASE_ENTER_USERNAME = "Please enter a username"
  val AUTHENTICATE_URL = controllers.routes.Authentication.authenticate.url
  val PASSWORD = "password"
    
  val loginForm = Form(Forms.tuple(Security.username -> Forms.nonEmptyText, 
      PASSWORD -> Forms.text))
    
  def login = Action {
	  Ok(views.html.login(AUTHENTICATE_URL, loginForm))
  }
  
  def noUserLogin(username: String) = Action {
    Ok(views.html.login(AUTHENTICATE_URL, loginForm, NO_USER_MSG format username))
  }
  
  def noPasswordLogin(username: String) = Action {
    Ok(views.html.login(AUTHENTICATE_URL, 
        loginForm, NO_PASSWORD_MSG format username))
  }
  
  def noUserEnteredLogin = Action {
    Ok(views.html.login(AUTHENTICATE_URL, logiinForm, PLEASE_ENTER_USERNAME))
  }
    
  def authenticate: Action[AnyContent] =
    Action { implicit request => {
    	val (username, password) = request.body.asFormUrlEncoded match {
    	  case Some(x: Map[_, Seq[String]]) => {
    	    val username = x.getOrElse(Security.username, Nil).flatten.mkString("")
    	    val password = x.getOrElse(PASSWORD, Nil).flatten.mkString("")
    	    (username, password)
    	  }
    	  case None => ("", "")
    	}
	    if (username.isEmpty) {
	      Redirect(controllers.routes.Authentication.noUserEnteredLogin)
	    } else {
	    	DBQueries.getPlayerFromPlayerName(username) match {
	    	  case Some(p: Player) => if (password.equals(p.password)) {
		        Redirect(controllers.routes.Application.index).
		          withSession(session + (Security.username -> username))
		      } else {
		        Redirect(controllers.routes.Authentication.noPasswordLogin(username))
		      }
	    	  case None => 
	    	    Redirect(controllers.routes.Authentication.noUserLogin(username))
	    	}
	    }
	  }
  }
  
  def logout = Action {implicit request => {
	  session - (Security.username)
	  Redirect(controllers.routes.Authentication.login)
  	}
  }
}
