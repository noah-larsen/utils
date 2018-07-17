package renaming

import consoleApplication.ConsoleRenamer.Languages.Language
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document, TextField}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig}
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, MatchNoDocsQuery, Query}
import org.apache.lucene.store.RAMDirectory
import renaming.NameSearch.Fields
import renaming.NameSearch.Fields.Field
import utils.enumerated.{Enumerated, EnumeratedType}

import scala.util.Try

case class NameSearch(
                       approvedNames: Iterable[ApprovedName]
                     )(implicit language: Language) {

  private val ramDirectory = new RAMDirectory()
  private val analyzer = new StandardAnalyzer()


  writeIndex()


  def approvedNameToNormalizedScore(query: String, maxNTopHitsToGetNonZeroScores: Integer, fields: Seq[Field] = Fields.values): Map[ApprovedName, Double] = {
    val directoryReader = DirectoryReader.open(ramDirectory)
    try {
      val indexSearcher = new IndexSearcher(directoryReader)
      val topDocs = indexSearcher.search(parse(query, fields), maxNTopHitsToGetNonZeroScores)
      val topApprovedNameToScore = topDocs.scoreDocs.map(x => (approvedNames.find(_.name == indexSearcher.doc(x.doc).getField(Fields.Name.name).stringValue()).get, x.score.toDouble)).toMap //todo is name unique?
      val approvedNameToScore = topApprovedNameToScore ++ approvedNames.filter(!topApprovedNameToScore.contains(_)).map((_, 0.toDouble))
      val maxScore = approvedNameToScore.values.max
      approvedNameToScore.mapValues(x => if (maxScore > 0) x / maxScore else x)
    } finally directoryReader.close()
  }


  def approvedNameToNormalizedScoreFromArgs(query: Seq[String], maxNTopHitsToGetNonZeroScores: Integer, fields: Seq[Field] = Fields.values): Map[ApprovedName, Double] = {
    val separator = " "
    approvedNameToNormalizedScore(query.mkString(separator), maxNTopHitsToGetNonZeroScores, fields)
  }


  private def parse(query: String, fields: Seq[Field]): Query = {
    val remove = Seq("~", "`", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "+", "=", "{", "}", "[", "]", "|", "\\", ":", ";", "\"", "<", ",", ">", ".", "?", "/")
    val removeAllFromBeginningTerm = '-'
    val defineTermForFieldOperator = ":"
    val separator = " "
    val whitespaceRe = "\\s+"
    val orOperator = "OR"
    val queryTerms = remove.foldLeft(query)((x, y) => x.replace(y, new String)).toLowerCase.split(whitespaceRe).distinct.map(_.dropWhile(_ == removeAllFromBeginningTerm)).filter(_ != new String)
    queryTerms.length match {
      case 0 => new MatchNoDocsQuery
      case _ =>
        val fieldDefinedTerms = queryTerms.flatMap(x => fields.map(_.name + defineTermForFieldOperator + x))
        new QueryParser(new String, analyzer).parse(fieldDefinedTerms.mkString(separator + orOperator + separator))
    }
  }


  private def writeIndex(): Unit = {
    val indexWriter = new IndexWriter(ramDirectory, new IndexWriterConfig(analyzer).setOpenMode(OpenMode.CREATE))
    approvedNames.foreach{ approvedName =>
      val document = new Document()
      val separator = " "
      document.add(new TextField(Fields.Name.name, approvedName.name, Store.YES))
      document.add(new TextField(Fields.NameSubstrings.name, approvedName.normalizedSubstrings.mkString(separator), Store.NO))
      document.add(new TextField(Fields.LogicalName.name, approvedName.logicalName, Store.NO))
      document.add(new TextField(Fields.Description.name, approvedName.description, Store.NO))
      indexWriter.addDocument(document)
    }
    indexWriter.close()
  }

}

object NameSearch {

  object Fields extends Enumerated {

    override type T = Field
    sealed abstract case class Field() extends EnumeratedType

    object Name extends Field
    object NameSubstrings extends Field
    object LogicalName extends Field
    object Description extends Field


    override val values = Seq(Name, NameSubstrings, LogicalName, Description)

  }

}
