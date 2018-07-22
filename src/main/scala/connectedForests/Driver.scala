package connectedForests

import persistence.{PathToIdJsonFormat, StringJsonFormat}
import play.api.libs.json.Json


object Driver extends App {

//  val pathname = "taxonomy.en-US.txt"
//  println(GoogleProductTaxonomy(pathname).labeledForest.paths)
//  val a = DevelopingConnectedForests(ConnectedForests())
//  println(a)

//  val a = DevelopingConnectedForests[String, String]()
//  println(DevelopingConnectedForests(a.serialize))

  val a = LabeledForest().withPath(Seq("1","2","3")).pathToId
  val b = PathToIdJsonFormat(StringJsonFormat)
  println(b.fromJson(b.toJson(a)) == a)
}
