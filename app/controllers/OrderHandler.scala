package controllers

import play.api.mvc._
import scalaz._
import com.squeryl.jdip.tables._
import com.squeryl.queries._
import ReaderTExtension._
import ReaderT._

object ReaderTExtension {
  implicit def readerTFunctor[M[_]: Functor, R]: Functor[({type y[q]=ReaderT[M, R, q]})#y] = 
    new Functor[({type y[q]= ReaderT[M, R, q]})#y] {
	  def fmap[A, B](x: ReaderT[M, R, A], f: A => B) = new ReaderT[M, R, B] {
		  val value = x.value ∘ (_ ∘ f)
    }
  }
  
  def readerTAsk[M[_], R](implicit m: Pure[M]) =
	 ReaderT((r: R) => m.pure(r))
}

object OrderHandler extends Controller {
  val GAME_PLAYER_EMPIRE_ID_NAME = "gamePlayerEmpireID"
  val UNIT_NAME = "unit"
  val ORDER_PREFIX = "order-"
  val SOURCE_PREFIX = "source-"
  val TARGET_PREFIX = "target-"
  val SUBMIT_MOVE_ORDERS_URL = 
    controllers.routes.OrderHandler.submitMoveOrders.url
    
  def isGamePlayerEmpireInSession(gpe: GamePlayerEmpire, 
      username: String): Boolean = {
    for (player <- DBQueries.getPlayerForGamePlayerEmpire(gamePlayerEmpire)
    ) yield {
      player.id.equals(username)
    } match {
      case Some(x: Boolean) => x
      case None => false
    }
    
  }
  
  
  def handleOrder(, postParameters: Map[String, Seq[String]])
  
  def processRequest(gpe: GamePlayerEmpire, 
      postParameters: Map[String, Seq[String]]) = {
    val dpuUnits = 
      DBQueries.getDiplomacyUnitsForGamePlayerEmpire(gpe)
    
    dpuUnits.foreach(dpu => {
      val unitOrderOption = 
        postParameters.get("%s%s" format (ORDER_PREFIX, dpu.id))
       
    })
  }
  
    
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      implicit val postParametersOption = request.body.asFormUrlEncoded
      val postEnvironment =
        ReaderT.readerTPure[Option, Map[String, Seq[String]]].pure(Unit)
      
      fmap(postEnvironment, _ => {})
      
      for (postParameters <- postParametersOption;
    	 ReaderT.readerTPure
    	 gpeID <- postParameters.
    	  	get(GameScreenController.GAME_PLAYER_EMPIRE_ID);
    	 gamePlayerEmpire <- DBQueries.getGamePlayerEmpire(gpeID.toInt);
    	 username <- request.session.get(Security.username);
    	  _ <- processRequest(gamePlayerEmpire, postParameters) if 
    	  	isGamePlayerEmpireInSession(gamePlayerEmpire, username)
      ) yield {
        isGamePlayerEmpireIDInSession
      }
      
      Ok("Uoi have submitted your moves")
	})
  )
 
  
}