package controllers.json

import com.squeryl.jdip.tables.UniqueProvinceName
import play.api.libs.json.Json
import play.api.libs.json._
import play.api.libs.json.Writes._
import com.squeryl.jdip.queries.DBQueries


object UniqueProvinceNameWrites extends Writes[UniqueProvinceName] {
  val PROVINCE_NAME = "provinceName"
  val ALTERNATE_NAME = "alternateName"
    
  def writes(upn: UniqueProvinceName): JsValue = Json.toJson(Map(
	PROVINCE_NAME -> upn.provinceName,
	ALTERNATE_NAME -> upn.alternateName
  ))

  def getJSValue: JsValue = {
    val uniqueProvinceNames = DBQueries.uniqueProvinceNames
    
    Json.toJson(uniqueProvinceNames)(listWrites(this))
  }
}