package controllers.json
import com.squeryl.jdip.tables._
import play.api.libs.json._
import com.squeryl.jdip.queries.DBQueries

class PotentialConvoyOrderWrites(pcos: List[PotentialConvoyOrder]) extends 
  Writes[Tuple3[PotentialConvoyOrder, Location, Location]] {
  def getJSValue: JsValue = {
      val pcosWithLocations = pcos.map((pco: PotentialConvoyOrder) => {
        val sourceLocationOption = 
          DBQueries.locations.find(_.id == pco.convoySourceLocationID)
        val targetLocationOption =
          DBQueries.locations.find(_.id == pco.convoyTargetLocationID)
          
        for (sourceLocation <- sourceLocationOption;
        	targetLocation <- targetLocationOption
        ) yield ((pco, sourceLocation, targetLocation))
         
      }).flatten
      
      Json.toJson(pcosWithLocations)(listWrites(this))
    })
    
  
  def writes(u: Tuple3[PotentialConvoyOrder, Location, Location]): JsValue =
	Json.toJson(Map(
	    GameScreenController.DIPLOMACY_UNIT_ID -> 
	    	u._1.diplomacyUnitID.toString,
		GameScreenController.SOURCE_LOCATION_ID -> u._2.id.toString,
		GameScreenController.SOURCE_PRESENTATION_NAME -> 
			u._2.presentationName,
		GameScreenController.TARGET_LOCATION_ID -> u._3.id.toString,
		GameScreenController.TARGET_PRESENTATION_NAME -> u._3.presentationName
	))
}