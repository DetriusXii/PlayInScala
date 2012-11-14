package controllers
import play.api.mvc._

object OrderHandler extends Controller {

  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction {
    Action { implicit request =>
      for (postParameters <- request.body.asFormUrlEncoded;
    		postParameters
    }
  }
}