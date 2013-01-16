package controllers.json
import com.squeryl.jdip.tables._
import play.api.libs.json._
import com.squeryl.jdip.queries.DBQueries

class PotentialMoveOrderWrites(pmos: List[PotentialMoveOrder]) extends 
  Writes[Tuple2[PotentialMoveOrder, Location]] {
  def getJSValuePromise: JsValue = {
	  val pmosWithLocations = pmos.map((pmo: PotentialMoveOrder) => {
		val locationOption = 
	      DBQueries.locations.find(_.id == pmo.moveLocationID)
	    locationOption.map((loc: Location) => (pmo, loc))
	  }).flatten
	    
	  Json.toJson(pmosWithLocations)(listWrites(this))
	})
    
  def writes(u: Tuple2[PotentialMoveOrder, Location]): JsValue =
	Json.toJson(Map(
	  GameScreenController.DIPLOMACY_UNIT_ID -> u._1.diplomacyUnitID.toString, 
	  GameScreenController.LOCATION_ID -> u._2.id.toString,
	  GameScreenController.PRESENTATION_NAME -> u._2.presentationName
	))
}

