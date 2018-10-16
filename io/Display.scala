package utils.io

object Display {

  def indent(value: String, nTabs: Int = 1): String = {
    val indentString = "\t"
    indentString * nTabs + value
  }


  def table(rows: Seq[Seq[String]], header: Seq[String] = Seq(), reverse: Boolean = false): String = {
    val verticalDividerFillCharacter = "-"
    val verticalDividerColumnSeparator = "+"
    val newlineStrings = Seq("\n", "\r\n", "\r")
    val newlineReplacement = " "
    val unifiedRowsWithHeaderIfNonEmpty = header.headOption.map(_ => rows.+:(rows.headOption.map(x => header.padTo(x.length, new String)).getOrElse(header))).getOrElse(rows)
      .map(_.map(newlineStrings.foldLeft(_)((x, y) => x.replace(y, newlineReplacement))))
    val lengthToNStringsColumns = unifiedRowsWithHeaderIfNonEmpty.transpose.map(_.map(_.length).groupBy(x => x).mapValues(_.length))
    val maxLengths = lengthToNStringsColumns.map(x => if(x(x.keys.max) < x.filterKeys(_ != x.keys.max).values.sum) x.keys.max else x.keys.max + 1)
    val verticalDivider = maxLengths.map(verticalDividerFillCharacter * _).mkString(verticalDividerColumnSeparator, verticalDividerColumnSeparator,
      verticalDividerColumnSeparator)
    def displayRow(row: Seq[String], maxLengths: Seq[Int], columnSeparator: String = "|"): String = {
      val space = " "
      row.zip(maxLengths).map(x => x._1 + space * (x._2 - x._1.length)).mkString(columnSeparator, columnSeparator, columnSeparator)
    }
    Some(header.headOption.map(_ => Seq(verticalDivider, displayRow(unifiedRowsWithHeaderIfNonEmpty.head, maxLengths), verticalDivider)).getOrElse(Seq(verticalDivider))
      .++(header.headOption.map(_ => unifiedRowsWithHeaderIfNonEmpty.tail).getOrElse(unifiedRowsWithHeaderIfNonEmpty).map(displayRow(_, maxLengths))).:+(verticalDivider))
      .map(x => if(reverse) x.reverse else x).get.mkString(System.lineSeparator())
  }


  def withColonSpace(value: String): String = {
    val suffix = ": "
    value + suffix
  }


  def withCommaSpaces(values: Seq[String]): String = {
    val separator = ", "
    values.mkString(separator)
  }


  def withCommaSpaces(values: String*): String = {
    withCommaSpaces(values.toSeq)
  }


  def withNewlines(values: Seq[String]): String = {
    val separator = System.lineSeparator()
    values.mkString(separator)
  }


  def withNewlines(values: String*): String = {
    withNewlines(values.toSeq)
  }


  def withSemicolonSpaces(values: Seq[String]): String = {
    val separator = "; "
    values.mkString(separator)
  }


  def withSemicolonSpaces(values: String*): String = {
    withSemicolonSpaces(values.toSeq)
  }


  def withSpacedArrows(values: Seq[String]): String = {
    val separator = " -> "
    values.mkString(separator)
  }


  def withSpacedArrows(values: String*): String = {
    withSpacedArrows(values.toSeq)
  }


  def withSpacedDashes(values: Seq[String]): String = {
    val separator = " - "
    values.mkString(separator)
  }


  def withSpacedDashes(values: String*): String = {
    withSpacedDashes(values.toSeq)
  }


  def withSpacedGreaterThans(values: Seq[String]): String = {
    val separator = " > "
    values.mkString(separator)
  }


  def withSpacedGreaterThans(values: String*): String = {
    withSpacedGreaterThans(values.toSeq)
  }


  def withSpaces(values: Seq[String]): String = {
    val separator = " "
    values.mkString(separator)
  }


  def withSpaces(values: String*): String = {
    withSpaces(values.toSeq)
  }


  def withTabs(values: Seq[String]): String = {
    val separator = "\t"
    values.mkString(separator)
  }


  def withTabs(values: String*): String = {
    withTabs(values.toSeq)
  }

}
