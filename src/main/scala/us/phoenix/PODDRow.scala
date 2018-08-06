package us.phoenix

import googleSpreadsheets.{RowReader, Row, SheetRange}

private[phoenix] case class PODDRow(
                                     tableName: String,
                                     columnName: String,
                                     dataType: String,
                                     nullable: String,
                                     comments: String
                                   ) extends Row {

  def description: String = {
    logicalName_description._2
  }


  def logicalName: String = {
    logicalName_description._1
  }


  private def logicalName_description: (String, String) = {
    val logicalNameDescriptionCommentSeparator = ":"
    Some(comments.indexOf(logicalNameDescriptionCommentSeparator)).filter(_ != -1).map(x => (comments.substring(0, x), comments.substring(x).tail)).orElse(Some((new String, comments))).map(x => (x._1.trim, x._2.trim)).get
  }

}

private[phoenix] object PODDRow extends RowReader[PODDRow] {

  override def sheetRange: SheetRange = {
    SheetRange("PHX Data Dictionary 20180716.csv", "E", 2)
  }


  override protected def toRow(r: Int => String): PODDRow = {
    PODDRow(r(0), r(1), r(2), r(3), r(4))
  }

}
