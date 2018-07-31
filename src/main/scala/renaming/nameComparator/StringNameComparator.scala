package renaming.nameComparator

import info.debatty.java.stringsimilarity.Jaccard
import renaming.{TargetName, SourceName}

case class StringNameComparator(
                                 normalizedSubstringToMatchToImportance: Map[String, Map[String, Int]] = Map(),
                                 normalizedDistance: (String, String) => Double = StringNameComparator.normalizedDistance
                               ) extends IndividualNameComparator {

  override def normalizedScore(name: SourceName, approvedName: TargetName): Double = {

    def combinations[T](possibleValuesForEachIndex: Seq[Seq[T]]): Seq[Seq[T]] = {
      possibleValuesForEachIndex.headOption.map(x => possibleValuesForEachIndex.tail.foldLeft(x.map(Seq(_)))((x, y) => y.flatMap(z => x.map(_ :+ z)))).getOrElse(Seq())
    }


    val possibleSubstringsForEachIndex = name.normalizedSubstrings.map(x => normalizedSubstringToMatchToImportance.getOrElse(x, Map[String, Int]()).keys.toSeq.+:(x))
    val conjoinedSubstringsSeparator = "_"
    combinations(possibleSubstringsForEachIndex).map(x => 1 - normalizedDistance(x.mkString(conjoinedSubstringsSeparator), approvedName.normalizedSubstrings.mkString(conjoinedSubstringsSeparator))).max

  }

}

object StringNameComparator {

  def normalizedDistance(x: String, y: String): Double = {
    val k = 2
    new Jaccard(k).distance(x, y)
  }

}

