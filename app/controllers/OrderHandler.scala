package controllers
import play.api.mvc._
import scalaz._
import com.squeryl.jdip.tables._

object OrderHandler extends Controller {
  val GAME_PLAYER_EMPIRE_ID_NAME = "gamePlayerEmpireID"
  val UNIT_NAME = "unit"
  val ORDER_PREFIX = "order-"
  val SOURCE_PREFIX = "source-"
  val TARGET_PREFIX = "target-"
  val SUBMIT_MOVE_ORDERS_URL = 
    controllers.routes.OrderHandler.submitMoveOrders.url
    
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      
      val postParametersOption = request.body.asFormUrlEncoded
      
      
      Ok("Uoi have submitted your moves")
	})
  )
 
  
}