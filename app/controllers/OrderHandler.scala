package controllers

import play.api.mvc._
import scalaz._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.queries._

object OrderHandler extends Controller with Kleislis {
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
  
  
  def processRequest(gpe: GamePlayerEmpire, 
		  username: String): Kleisli[Option, Map[String, Seq[String]], Unit] = {
    val dpuUnits = 
      DBQueries.getDiplomacyUnitsForGamePlayerEmpire(gpe)
    
    kleisli((r: Map[String, Seq[String]]) => {
      dpuUnits.foreach(dpu => {
        val unitOrderOption = r.get("%s%s" format (ORDER_PREFIX, dpu.id))
        unitOrderOption.map(_ match {
          case OrderType.HOLD => addHoldOrder(dpu)
          case OrderType.MOVE | OrderType.SUPPORT_HOLD =>
          case OrderType.SUPPORT_MOVE | OrderType.CONVOY =>
        
        })
        
        
      })
      
      
    })
  }
  
  def addHoldOrder(dpu: DiplomacyUnit): 
  
    
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      val postParametersOption = request.body.asFormUrlEncoded
      postParametersOption.map((pp: Map[String, Seq[String]]) => {
        val postEnvironment = ask[Option, Map[String, Seq[String]]]
        
        for (r <- postEnvironment;
        	gpeID <- liftKleisli(r.get(GameScreenController.GAME_PLAYER_EMPIRE_ID));
        	gamePlayerEmpire <- liftKleisli(DBQueries.getGamePlayerEmpire(gpeID.toInt));
        	username <- liftKleisli(request.session.get(Security.username));
        	_ <- kleisliPure(processRequest(gamePlayerEmpire, username)) if
        		isGamePlayeEmpireInSession(gamePlayerEmpire, username)
        )
      })
      
      
      
      for (postParameters <- )
      
      
     // fmap(postEnvironment, _ => {})
      
      /*for (postParameters <- postParametersOption;
    	 ReaderT.readerTPure
    	 gpeID <- postParameters.
    	  	get(GameScreenController.GAME_PLAYER_EMPIRE_ID);
    	 gamePlayerEmpire <- DBQueries.getGamePlayerEmpire(gpeID.toInt);
    	 username <- request.session.get(Security.username);
    	  _ <- processRequest(gamePlayerEmpire, postParameters) if 
    	  	isGamePlayerEmpireInSession(gamePlayerEmpire, username)
      ) yield {
        isGamePlayerEmpireIDInSession
      }*/
      
      Ok("Uoi have submitted your moves")
	})
  )
 
  
}