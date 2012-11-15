package controllers
import play.api.mvc._
import scalaz._

object OrderHandler extends Controller with OptionTs {
  val GAME_PLAYER_EMPIRE_ID_NAME = "gamePlayerEmpireID"
  val UNIT_NAME = "unit"
  val ORDER_SUFFIX = "order"
  val SOURCE_SUFFIX = "source"
  val TARGET_SUFFIX = "target"
  
  implicit def embedOptionInListT[A](option: Option[A]): ListT[Option, A] =
    option match {
    	case Some(a) => a :: ListT.empty[Option, A]
    	case None => ListT.empty[Option, A]
  	}
  
  implicit def convertListToListT[A](list: List[A]): ListT[Option, A] =
    list.reverse.foldLeft(ListT.empty[Option, A])((u: ListT[Option, A], v: A) => v :: u)
    
  implicit def convertSeqToSingleString(seq: Seq[String]): String = seq.mkString("")
  
  def submitMoveOrders: ApplicationAction[AnyContent] = new ApplicationAction (
    Action ( implicit request => {
      
      val postParametersListT = embedOptionInListT(request.body.asFormUrlEncoded)
      
      val dpuAndLocsListT =
        postParametersOptionT.flatMap(getDiplomacyUnitAndLocation)
      
      
	})
  )
  
  private def allUnitOrdersExist(
      postParameters: Map[String, Seq[String]],
      dpuAndLocsListT: ListT[Option, (DiplomacyUnit, Location)]): Boolean = {
	  dpuAndLocsListT.foldLeft(true)((u, v) => {
	    val unitLocation = v._2
	    val formattedLocationName = 
	      DiplomacyQueries.getFormattedLocationName(unitLocation)
	    val orderForUnitOption = 
	      postParameters.get("%s-%s" format (formattedLocationName, ORDER_SUFFIX))
	    orderForUnitOption match {
	      case Some(_) => u && true
	      case None => u && false
	    }
	  })
  }
  
  private def ordersMatchForUnitTypeAndPhase(
		  postParameters: Map[String, Seq[String]],
		  dpuAndLocsListT: ListT[Option, (DiplomacyUnit, Location)]): Boolean = {
    dpuAndLocsListT.foldLeft(true)((u, v) => {
      val dpu = v._1
      val unitLocation = v._2
      val formattedLocationName =
        DiplomacyQueries.getFormattedLocationName(unitLocation)
      val orderForUnitOption =
        postParameters.get("%s-%s" format (formattedLocationName, ORDER_SUFFIX))
      orderForUnitOption match {
        case Some(x: Seq[String]) => x.mkString("") match {
          case OrderType.HOLD | 
          		OrderType.MOVE | 
          		OrderType.SUPPORT_HOLD | 
          		OrderType.SUPPORT_MOVE => v && true
          case OrderType.CONVOY if dpu.UNIT_TYPE.equals(UnitType.FLEET) => u && true
          case _ => u && false
        }
        case None => u && false
      }
    })
  }
  
  private def analyzeOrders(postParameters: Map[String, Seq[String]],
      dpuAndLocsListT: ListT[Option, (DiplomacyUnit, Location)]): Unit = {
    val dpuAndFormattedLocsListT =
      dpuAndLocsListT.map(u => (u._1, DiplomacyQueries.getFormattedLocationName(u._2)))
    
    
    dpuAndFormattedLocsListT.map(u: (DiplomacyUnit, String) => {
      val orderForUnitOption = postParameters.get("%s-%s" format(u._2, ORDER_SUFFIX))
      
      orderForUnitOption match {
        case Some(x: Seq[String]) => x match {
          case OrderType.HOLD => 
          case OrderType.MOVE =>
          case OrderType.SUPPORT_HOLD =>
          case OrderType.SUPPORT_MOVE =>
          case OrderType.CONVOY =>
        }
        case None => 
      }
    })
  }
  
  private def getDiplomacyUnitAndLocation(
      postParameters: Map[String, Seq[String]]): 
      ListT[Option, Tuple2[DiplomacyUnit, Location]] = {
    val gamePlayerEmpireIDOption = 
      postParameters.get(GAME_PLAYER_EMPIRE_ID_NAME).map(_.mkString("")).map(_.toInt)
    
    val gamePlayerEmpireOption = 
      gamePlayerEmpireIDOption.flatMap(DBQueries.getGamePlayerEmpire(_))
    val diplomacyUnits = 
      gamePlayerEmpireOption.flatMap(gpe =>
        convertListToListT(DBQueries.getDiplomacyUnitsForGamePlayerEmpire(gpe)))
    
    for (dpu <- diplomacyUnits;
    	loc <- DBQueries.locations.find(_.id == dpu.unitLocation)
    ) yield ((dpu, location))
  }
}