package renaming

import dataDictionary.DataDictionary
import dataDictionary.FieldEntry.IngestionStages

import scala.util.Try

case class ApprovedRenamings(originalAndRenamedNames: Seq[(String, String)]) {

  def normalizedSubstringToMatchToNObservations: Map[String, Map[String, Int]] = {

    def uniqueSubOrSuperStringMatches(substrings1: Seq[String], substrings2: Seq[String]): Seq[(String, String)] = {
      Some(substrings1.map(x => (x, substrings2.collect{case y if x.toLowerCase.contains(y.toLowerCase) || y.toLowerCase.contains(x.toLowerCase) => y}))).map(x => x.collect{case y if y._2.lengthCompare(1) == 0 && x.filter(_._2.exists(_.equalsIgnoreCase(y._2
        .head))).lengthCompare(1) == 0 => (y._1, y._2.head)}).get
    }


    def inferLastPairSubstringsMatch(substrings1: Seq[String], substrings2: Seq[String], uniqueSubOrSuperStringMatches: Seq[(String, String)]): Option[(String, String)] = {
      Some(Unit).filter(_ => substrings1.lengthCompare(substrings2.length) == 0 && uniqueSubOrSuperStringMatches.lengthCompare(substrings1.length - 1) == 0).map(_ => (substrings1.find(x => !uniqueSubOrSuperStringMatches.exists(_._1.equalsIgnoreCase(x)))
        .get, substrings2.find(x => !uniqueSubOrSuperStringMatches.exists(_._2.equalsIgnoreCase(x))).get))
    }


    def nonTrivialInferableSubstringMatches(substrings1: Seq[String], substrings2: Seq[String]): Seq[(String, String)] = {
      Some(uniqueSubOrSuperStringMatches(substrings1, substrings2)).map(x => inferLastPairSubstringsMatch(substrings1, substrings2, x).map(x :+ _).getOrElse(x)).get.filter(x => !x._1.equalsIgnoreCase(x._2))
    }


    originalAndRenamedNames.flatMap(x => nonTrivialInferableSubstringMatches(Name.normalizedSubstrings(x._1), Name.normalizedSubstrings(x._2))).groupBy(_._1).mapValues(_.map(_._2).groupBy(x => x).mapValues(_.length))

  }


  def originalToRenamedNameToNOccurences: Map[String, Map[String, Int]] = {
    originalAndRenamedNames.groupBy(_._1).mapValues(_.map(_._2).groupBy(x => x).mapValues(_.length))
  }

}

object ApprovedRenamings {

  def apply(dataDictionary: DataDictionary): Try[ApprovedRenamings] = {
    dataDictionary.fieldEntries(IngestionStages.Raw).map(x => ApprovedRenamings(x.filter(y => Seq(y.sourceField, y.physicalNameField).forall(_.isDefined)).map(y => y.sourceField.get -> y.physicalNameField.get)))
  }

}
