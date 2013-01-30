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
    (for (player <- DBQueries.getPlayerForGamePlayerEmpire(gpe)
    ) yield {
      player.id.equals(username)
    }) match {
      case Some(x) => x
      case None => false
    }
    
  }
  
  
  def processRequest(gpe: GamePlayerEmpire, 
		  username: String): Kleisli[Option, Map[String, Seq[String]], Unit] = {
    val dpuUnits = 
      DBQueries.getDiplomacyUnitsForGamePlayerEmpire(gpe)
    
    dpuUnits.foldLeft(kleisliPure[Option, Map[String, Seq[String]]].pure(()))((kl, dpu) => {
      kl.flatMap(_ => ask[Option, Map[String, Seq[String]]]).flatMap(r => 
        liftKleisli(r.get("%s%d" format (ORDER_PREFIX, dpu.id)).
            map(_.mkString("")))
      ).flatMap(unitOrder => unitOrder match {
      	case OrderType.HOLD => addHoldOrder(dpu)
      	case OrderType.MOVE | OrderType.SUPPORT_HOLD => addBasicOrder(dpu, unitOrder)
      	case OrderType.SUPPORT_MOVE | OrderType.CONVOY => addAdvancedOrder(dpu, unitOrder)
      })
    })
      
  }
  
  def addAdvancedOrder(dpu: DiplomacyUnit, unitOrder: String): 
	  Kleisli[Option, Map[String, Seq[String]], Unit] = {
    val postEnvironment = ask[Option, Map[String, Seq[String]]]
    val srcOrderReader = postEnvironment.flatMap(r => 
    	liftKleisli(r.get("%s%d" format (SOURCE_PREFIX, dpu.id)).
    	    map(_.mkString("").toInt))).flatMap(srcLocationId => 
    	    	liftKleisli(DBQueries.locations.find(_.id == srcLocationId))
    	    )
    val dstOrderReader = postEnvironment.flatMap(r =>
    	liftKleisli(r.get("%s%d" format (TARGET_PREFIX, dpu.id)).
    	    map(_.mkString("").toInt))).flatMap(dstLocationId => 
    	    	liftKleisli(DBQueries.locations.find(_.id == dstLocationId))
    	    )
    
    for (srcLocation <- srcOrderReader;
    	dstLocation <- dstOrderReader
    ) yield {
      val orderOption = DBQueries.getOrderForDiplomacyUnit(dpu)
      orderOption match {
        case Some(_) => UpdateStatements.updateMovementPhaseOrder(
        		dpu, unitOrder, Some(srcLocation), Some(dstLocation))
        case None => InsertStatements.insertMovementPhaseOrder(
        		dpu, unitOrder, Some(srcLocation), Some(dstLocation)
        )
      }
    }
  }
  
  def addBasicOrder(dpu: DiplomacyUnit, unitOrder: String): 
	  Kleisli[Option, Map[String, Seq[String]], Unit] = {
    def postEnvironment = ask[Option, Map[String, Seq[String]]]
    postEnvironment.flatMap(r => {
      val simpleTargetIDOption = 
        r.get("%s%d" format (SOURCE_PREFIX, dpu.id)).map(_.mkString("").toInt)
      liftKleisli(simpleTargetIDOption)
    }).flatMap(simpleTargetID => {
      val locOption = DBQueries.locations.find(_.id == simpleTargetID)
      
      val orderOption = DBQueries.getOrderForDiplomacyUnit(dpu)
      
      liftKleisli(locOption.map(loc => orderOption match {
        case Some(_) => 
          UpdateStatements.updateMovementPhaseOrder(dpu, unitOrder,
              Some(loc), None)
        case None =>
          InsertStatements.insertMovementPhaseOrder(dpu, unitOrder,
              Some(loc), None)
      }))
    })
    
  }
  
  def addHoldOrder(dpu: DiplomacyUnit): 
	  Kleisli[Option, Map[String, Seq[String]], Unit]  = 
      kleisliPure[Option, Map[String, Seq[String]]].
      	pure(DBQueries.getOrderForDiplomacyUnit(dpu) match {
	      case Some(_) => 
	        UpdateStatements.updateMovementPhaseOrder(dpu, 
	            OrderType.HOLD, None, None)
	      case None => 
	        InsertStatements.insertMovementPhaseOrder(dpu, 
	            OrderType.HOLD, None, None)
	    })
 
  
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      val postParametersOption = request.body.asFormUrlEncoded
      postParametersOption.map((pp: Map[String, Seq[String]]) => {
        val postEnvironment = ask[Option, Map[String, Seq[String]]]
        
        def liftTrueBoolean[R](b: Boolean): Kleisli[Option, R, Boolean] = 
          if (b) {
        	  liftKleisli[Option, R, Boolean](Some(true))
          } else {
            liftKleisli[Option, R, Boolean](None)
          }
        
        val monadReader = for (r <- postEnvironment;
        	gpeID <- liftKleisli(
        	    r.get(GameScreenController.GAME_PLAYER_EMPIRE_ID));
        	gamePlayerEmpire <- 
        		liftKleisli(DBQueries.getGamePlayerEmpire(
        		    gpeID.mkString("").toInt));
        	username <- liftKleisli(request.session.get(Security.username));
        	_ <- liftTrueBoolean(
        			isGamePlayerEmpireInSession(gamePlayerEmpire, 
        					username));
        	_ <- processRequest(gamePlayerEmpire, username)
        		
        ) yield { () }
        
        monadReader(pp)
      })
      
      
      Ok("You have submitted your moves")
	})
  )
 
  
}