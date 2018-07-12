package utils

import play.api.libs.json.JsValue

trait JsonSerializer[T] {

  def toJson(t: T): JsValue
  def fromJson(jsValue: JsValue): T

}
