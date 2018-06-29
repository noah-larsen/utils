package renaming

import centralNamingsRepository.CentralNamingsRepository
import info.debatty.java.stringsimilarity.experimental.Sift4
import info.debatty.java.stringsimilarity.{Jaccard, MetricLCS, NormalizedLevenshtein}

case class NameComparator(
                           normalizedSubstringToMatchToImportance: Map[String, Map[String, Int]] = Map(),
                           distance: (String, String) => Double = NameComparator.distance
                         ) {

  def sortedDistances(name: String, possibleTargets: Iterable[String]): Seq[(String, Double)] = {
    possibleTargets.map(x => (x, nameDistance(name, x))).toSeq.sortBy(_._2)
  }


  def nameDistance(toRename: String, possibleTarget: String): Double = {
    distance(Name(toRename), Name(possibleTarget))
  }


  private def distance(toRename: Name, possibleTarget: Name): Double = {

    def combinations[T](possibleValuesForEachIndex: Seq[Seq[T]]): Seq[Seq[T]] = {
      possibleValuesForEachIndex.headOption.map(x => possibleValuesForEachIndex.tail.foldLeft(x.map(Seq(_)))((x, y) => y.flatMap(z => x.map(_ :+ z)))).getOrElse(Seq())
    }


    val possibleSubstringsForEachIndex = toRename.normalizedSubstrings.map(x => normalizedSubstringToMatchToImportance.getOrElse(x, Map[String, Int]()).keys.toSeq.+:(x))
    val conjoinedSubstringsSeparator = "_"
    combinations(possibleSubstringsForEachIndex).map(x => distance(x.mkString(conjoinedSubstringsSeparator), possibleTarget.normalizedSubstrings.mkString(conjoinedSubstringsSeparator))).min

  }

}

object NameComparator {

  def distance(x: String, y: String): Double = {
    val k = 2
    new Jaccard(k).distance(x, y)
  }

}

