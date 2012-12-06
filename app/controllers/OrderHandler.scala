package controllers
import play.api.mvc._
import scalaz._
import com.squeryl.jdip.tables._

object OrderHandler extends Controller {
  val GAME_PLAYER_EMPIRE_ID_NAME = "gamePlayerEmpireID"
  val UNIT_NAME = "unit"
  val ORDER_SUFFIX = "order"
  val SOURCE_SUFFIX = "source"
  val TARGET_SUFFIX = "target"
  
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      
      val postParametersOption = request.body.asFormUrlEncoded
      
      
      Ok("Uoi have submitted your moves")
	})
  )
 
  
}