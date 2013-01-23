package controllers.json

import com.squeryl.jdip.tables._
import play.api.libs.json._
import play.api.libs.json.Writes._
import com.squeryl.jdip.queries.DBQueries
import controllers.GameScreenController

class PotentialSupportMoveOrderWrites(psmos: List[PotentialSupportMoveOrder]) extends
  Writes[Tuple3[PotentialSupportMoveOrder, Location, Location]] {
  def getJSValue: JsValue = {
      val psmosWithLocations =
        psmos.map((psmo: PotentialSupportMoveOrder) => {
          val sourceLocationOption = 
            DBQueries.locations.find(_.id == psmo.supportMoveSourceLocationID)
          val targetLocationOption =
            DBQueries.locations.find(_.id == psmo.supportMoveTargetLocationID)
        
          for (sourceLocation <- sourceLocationOption;
        	  targetLocation <- targetLocationOption
          ) yield ((psmo, sourceLocation, targetLocation))
        }).flatten
      
      Json.toJson(psmosWithLocations)(listWrites(this))
	}
  
  def writes(u: Tuple3[PotentialSupportMoveOrder, Location, Location]):
    JsValue =
      Json.toJson(Map(
        GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString,
        GameScreenController.SOURCE_LOCATION_ID -> u._2.id.toString,
        GameScreenController.SOURCE_PRESENTATION_NAME -> u._2.presentationName,
        GameScreenController.TARGET_LOCATION_ID -> u._3.id.toString,
        GameScreenController.TARGET_PRESENTATION_NAME -> u._3.presentationName
      ))
}