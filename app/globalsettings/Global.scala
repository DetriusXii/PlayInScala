package globalsettings

import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.db.DB
import controllers.ApplicationAction
import com.squeryl.jdip.adapters._

object Global extends GlobalSettings {
  import play.api.Play._
  
  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    request.session.get(Security.username) match {
      case Some(x) => super.onRouteRequest(request) 
      case None => {
        val handlerOption = super.onRouteRequest(request)
        handlerOption match {
          case Some(handler: Handler) => handler match {
            case action: ApplicationAction[_] => Some(Action {Redirect(controllers.routes.Authentication.login)})
            case _ => handlerOption
          }
          case None => handlerOption
        }
      }
    }
  }
  
  override def onStart(app: Application) {
    org.squeryl.SessionFactory.concreteFactory = Some(() => 
      org.squeryl.Session.create(DB.getConnection(), new RevisedPostgreSqlAdapter))
  }
}