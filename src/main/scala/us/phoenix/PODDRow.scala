package us.phoenix

import googleSpreadsheets.{DataReader, Row, SheetRange}

private[phoenix] case class PODDRow(
                    tableName: String,
                    columnName: String,
                    nullOption: String,
                    dataType: String,
                    comment: String
                  ) extends Row {

  def description: String = {
    logicalName_description._2
  }


  def logicalName: String = {
    logicalName_description._1
  }


  private def logicalName_description: (String, String) = {
    val logicalNameDescriptionCommentSeparator = ":"
    Some(comment.indexOf(logicalNameDescriptionCommentSeparator)).filter(_ != -1).map(x => (comment.substring(0, x), comment.substring(x).tail)).orElse(Some((new String, comment))).map(x => (x._1.trim, x._2.trim)).get
  }

}

private[phoenix] object PODDRow extends DataReader[PODDRow] {

  override def sheetRange: SheetRange = {
    SheetRange("PHX Owner Data Dictionary", "E", 2)
  }


  override protected def toRow(r: Int => String): PODDRow = {
    PODDRow(r(0), r(1), r(2), r(3), r(4))
  }

}
