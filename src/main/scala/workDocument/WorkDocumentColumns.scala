package workDocument

import java.time.format.DateTimeFormatter

import googleSpreadsheets.{Column, Columns}
import workDocument.WorkDocumentEntry.registrationDateFormat

private[workDocument] object WorkDocumentColumns extends Columns {

  override type RowType = WorkDocumentEntry
  override type ColumnType = WorkDocumentColumn
  sealed abstract class WorkDocumentColumn(val string: WorkDocumentEntry => String) extends Column[WorkDocumentEntry]

  object ColumnRank extends WorkDocumentColumn(_.columnRank.map(_.toString).getOrElse(new String))
  object RegistrationDate extends WorkDocumentColumn(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(registrationDateFormat))).getOrElse(new String))
  object Status extends WorkDocumentColumn(x => selfNamedOptionToString(x.status))
  object SourceOrigin extends WorkDocumentColumn(_.sourceOrigin.toString)
  object Table extends WorkDocumentColumn(_.table.toString)
  object SourceField extends WorkDocumentColumn(_.sourceField.toString)
  object LogicFormat extends WorkDocumentColumn(_.logicFormat.toString)
  object FieldDescription extends WorkDocumentColumn(_.fieldDescription.toString)
  object UsedYN extends WorkDocumentColumn(_.usedYN.toString)
  object DataModelerComments extends WorkDocumentColumn(_.dataModelerComments.toString)
  object GlobalArchitectureComments extends WorkDocumentColumn(_.globalArchitectureComments.toString)
  object LocalArchitectureComments extends WorkDocumentColumn(_.localArchitectureComments.toString)
  object ValidatedByLocalArchitecture extends WorkDocumentColumn(x => selfNamedOptionToString(x.validatedByLocalArchitecture))
  object ValidatedByGlobalArchitecture extends WorkDocumentColumn(x => selfNamedOptionToString(x.validatedByGlobalArchitecture))
  object FieldCode extends WorkDocumentColumn(_.fieldCode.toString)
  object GlobalNamingField extends WorkDocumentColumn(_.globalNamingField.toString)


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[WorkDocumentColumns.type], classOf[WorkDocumentColumn])

}