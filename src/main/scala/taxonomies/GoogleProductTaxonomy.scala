package taxonomies

import connectedForests.{ConnectedForests, LabeledForest}

import scala.io.Source

case class GoogleProductTaxonomy(categories: Seq[Seq[String]]) {

  def labeledForest: LabeledForest[String] = {
    LabeledForest(categories)
  }

}

object GoogleProductTaxonomy {

  def apply(pathname: String): GoogleProductTaxonomy = {
    val commentSymbol = "#"
    val subcategorySeparator = ">"
    GoogleProductTaxonomy(Source.fromFile(pathname).getLines().map(_.trim).filter(!_.startsWith(commentSymbol)).map(_.split(subcategorySeparator).map(_.trim).toList).toList)
  }

}