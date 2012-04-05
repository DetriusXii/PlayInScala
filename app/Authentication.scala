/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers

import play.mvc._
import scala.xml._
import play.templates._

object Authentication extends Controller {
  val AUTHENTICATE_NAME = "Authentication.authenticate"
  
  def login = views.html.login(Router.reverse(AUTHENTICATE_NAME).url)
  
  def authenticate(username: String = "", password: String = "") = {
    if (username.equals("DetriusXii") && password.equals("bal1ance")) {
      session += "username" -> username
      Action(Application.index)
    } else {
      Action(Authentication.login)
    }
  }
  
  def logout = {
    session.remove("username")
    Action(Authentication.login)
  }
}
