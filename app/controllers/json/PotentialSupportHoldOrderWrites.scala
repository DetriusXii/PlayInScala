package controllers.json
import play.api.libs.json._
import play.api.libs.json.Writes._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.queries.DBQueries
import controllers.GameScreenController

class PotentialSupportHoldOrderWrites(pshos: List[PotentialSupportHoldOrder]) extends 
  Writes[Tuple2[PotentialSupportHoldOrder, Location]] {
  def getJSValue: JsValue = {
	  val pshosWithLocations = pshos.map((psho: PotentialSupportHoldOrder) => {
	  val locationOption =
	    DBQueries.locations.find(_.id == psho.supportHoldLocationID)
	      locationOption.map((loc: Location) => (psho, loc))
	  }).flatten
	    
	  Json.toJson(pshosWithLocations)(listWrites(this))
	}
    
  def writes(u: Tuple2[PotentialSupportHoldOrder, Location]): JsValue =
    Json.toJson(Map(
      GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString,
      GameScreenController.LOCATION_ID -> u._2.id.toString,
      GameScreenController.PRESENTATION_NAME -> u._2.presentationName
    ))
}