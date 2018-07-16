package renaming.nameComparator

import consoleApplication.ConsoleRenamer.Languages.Language
import renaming.NameSearch.Fields
import renaming.NameSearch.Fields.Field
import renaming.{ApprovedName, NameSearch, SourceName}

case class SearchNameComparator(
                                 nameSearch: NameSearch,
                                 query: SourceName => String,
                                 nTopHitsToGetNonZeroScores: Integer,
                                 fields: Seq[Field] = Fields.values
                               )(implicit language: Language) extends NameComparator {

  override def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[ApprovedName]): Map[ApprovedName, Double] = {
    nameSearch.approvedNameToNormalizedScore(query(name), nTopHitsToGetNonZeroScores, fields)
  }

}

object SearchNameComparator {

  def splitByWhitespace(query: String): Seq[String] = {
    val whitespaceRe = "\\s+"
    query.split(whitespaceRe)
  }


  def joinWithSpaces(terms: Seq[String]): String = {
    val separator = " "
    terms.mkString(separator)
  }

}