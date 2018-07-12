package persistence

import connectedForests.LabeledForest
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import utils.JsonSerializer

class PathToIdJsonSerializer[N](nodeValueJsonSerializer: JsonSerializer[N]) extends JsonSerializer[Map[Seq[N], Long]] {

  override def toJson(pathToId: Map[Seq[N], Long]): JsValue = {
    Json.toJson(pathToId.map(x => (x._1.map(nodeValueJsonSerializer.toJson), x._2)))
  }


  override def fromJson(jsValue: JsValue): Map[Seq[N], Long] = {
//    jsValue.as[JsObject].value.map(x => (x._2.as[JsArray].value.map(nodeValueJsonSerializer.fromJson), x._1.toLong)).asInstanceOf[Map[Seq[N], Long]]
    jsValue.as[Seq[(Seq[JsValue], Long)]].map(x => (x._1.map(nodeValueJsonSerializer.fromJson), x._2)).toMap
  }

}

object PathToIdJsonSerializer {

  def apply[N](nodeValueJsonSerializer: JsonSerializer[N]): PathToIdJsonSerializer[N] = {
    new PathToIdJsonSerializer(nodeValueJsonSerializer)
  }

}
