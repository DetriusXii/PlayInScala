package controllers
import play.api.mvc._
import scalaz._

object OrderHandler extends Controller with OptionTs {
  val GAME_PLAYER_EMPIRE_ID_NAME = "gamePlayerEmpireID"
  val UNIT_NAME = "unit"
  val ORDER_SUFFIX = "-order"
  val SOURCE_SUFFIX = "-source"
  val TARGET_SUFFIX = "-target"
  
  implicit def embedOptionInListT[A](option: Option[A]): ListT[Option, A] =
    option match {
    	case Some(a: A) => a :: ListT.empty[Option, A]
    	case None => ListT.empty[Option, A]
  	}
  
  
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction {
    Action { implicit request =>
      
      for (postParameters <- request.body.asFormUrlEncoded;
    	   gamePlayerEmpireID <- postParameters.get(GAME_PLAYER_EMPIRE_ID_NAME);
    	   gamePlayerEmpire <- DBQueries.getGamePlayerEmpire(gamePlayerEmpireID);
    	   					<- DBQueries.getDiplomacyUnitsForGamePlayerEmpire
    }
  }
}