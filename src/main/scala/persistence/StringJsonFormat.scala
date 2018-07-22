package persistence

import play.api.libs.json.{JsValue, Json}
import utils.JsonFormat

object StringJsonFormat extends JsonFormat[String] {

  override def toJson(t: String): JsValue = {
    Json.toJson(t)
  }


  override def fromJson(jsValue: JsValue): String = {
    jsValue.as[String]
  }

}
