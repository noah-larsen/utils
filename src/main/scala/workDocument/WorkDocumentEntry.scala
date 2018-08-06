package workDocument

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import dataDictionary.FieldEntry
import googleSpreadsheets.RowReaderWriter.{AbstractColumn, AbstractColumns}
import googleSpreadsheets.{RowReaderWriter, Row, SheetRange}
import utils.enumerated.Enumerated
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.Custom
import workDocument.WorkDocumentEntry.Statuses.Status
import workDocument.WorkDocumentEntry.ValidatedValues.ValidatedValue
import workDocument.WorkDocumentEntry.{Statuses, ValidatedValues}

import scala.util.Try

case class WorkDocumentEntry(
                              columnRank: Option[Int],
                              registrationDate: Option[LocalDate],
                              status: Option[Status],
                              sourceOrigin: String,
                              table: String,
                              sourceField: String,
                              logicFormat: String,
                              fieldDescription: String,
                              usedYN: String,
                              dataModelerComments: String,
                              globalArchitectureComments: String,
                              localArchitectureComments: String,
                              validatedByLocalArchitecture: Option[ValidatedValue],
                              validatedByGlobalArchitecture: Option[ValidatedValue],
                              fieldCode: String,
                              globalNamingField: String,
                            //todo
                            ) extends Row {

  def toFieldEntry(preserveRegistrationDate: Boolean): FieldEntry = {
    //todo
    def toStringOption(x: String): Option[String] = Some(x).filter(_.trim != new String)
    FieldEntry(
      registrationDate = registrationDate.filter(_ => preserveRegistrationDate),
      physicalNameObject = toStringOption(table),
      sourceField = toStringOption(sourceField),
      simpleFieldDescription = toStringOption(fieldDescription),
      physicalNameField = toStringOption(globalNamingField)
    )
  }

}

object WorkDocumentEntry extends RowReaderWriter[WorkDocumentEntry] {

  override def sheetRange: SheetRange = {
    SheetRange("Namings Repository - Work Document - Avro", "P", 4)
  }


  object Statuses extends Enumerated {
    
    override type T = Status
    sealed abstract case class Status(customName: String) extends SelfNamed(Custom(customName))
    
    object RegisteredInTheCentralRepository extends Status("Registered in the Central Repository")
    object PendingDataModeler extends Status("Pending Data Modeler")
    object PendingLocalArchitecture extends Status("Pending Local Arq.")
    object PendingGlobalArchitecutre extends Status("Pending Global Arq.")
    object RuledOut extends Status("Ruled out")
    object ExistsInGlobalRepo extends Status("Exists in Global Repo")
    object NotUsed extends Status("Not Used")


    override val values = Seq(RegisteredInTheCentralRepository, PendingDataModeler, PendingLocalArchitecture, PendingGlobalArchitecutre, RuledOut, ExistsInGlobalRepo, NotUsed)

  }


  object ValidatedValues extends Enumerated {

    override type T = ValidatedValue
    sealed abstract class ValidatedValue extends SelfNamed

    object OK extends ValidatedValue
    object KO extends ValidatedValue


    override val values = Seq(OK, KO)

  }


  override protected def toRow(r: Int => String): WorkDocumentEntry = {
    WorkDocumentEntry(
      columnRank = Some(r(columns.order.indexOf(Columns.ColumnRank))).filter(x => Try(x.toInt).isSuccess).map(_.toInt), //todo reconsider whole error handling strategy here
      registrationDate = possibleRegistrationDateFormats.map(x => Try(LocalDate.parse(r(columns.order.indexOf(Columns.RegistrationDate)), DateTimeFormatter.ofPattern(x)))).maxBy(_.isSuccess).toOption,
      status = dropDown(Statuses, r(columns.order.indexOf(Columns.Status))),
      sourceOrigin = r(columns.order.indexOf(Columns.SourceOrigin)),
      table = r(columns.order.indexOf(Columns.Table)),
      sourceField = r(columns.order.indexOf(Columns.SourceField)),
      logicFormat = r(columns.order.indexOf(Columns.LogicFormat)),
      fieldDescription = r(columns.order.indexOf(Columns.FieldDescription)),
      usedYN = r(columns.order.indexOf(Columns.UsedYN)),
      dataModelerComments = r(columns.order.indexOf(Columns.DataModelerComments)),
      globalArchitectureComments = r(columns.order.indexOf(Columns.GlobalArchitectureComments)),
      localArchitectureComments = r(columns.order.indexOf(Columns.LocalArchitectureComments)),
      validatedByLocalArchitecture = dropDown(ValidatedValues, r(columns.order.indexOf(Columns.ValidatedByLocalArchitecture))),
      validatedByGlobalArchitecture = dropDown(ValidatedValues, r(columns.order.indexOf(Columns.ValidatedByGlobalArchitecture))),
      fieldCode = r(columns.order.indexOf(Columns.FieldCode)),
      globalNamingField = r(columns.order.indexOf(Columns.GlobalNamingField))
    )
  }


  override protected def columns: AbstractColumns[WorkDocumentEntry] = Columns


  private object Columns extends AbstractColumns[WorkDocumentEntry] {
    sealed class Column(val string: WorkDocumentEntry => String) extends AbstractColumn[WorkDocumentEntry]
    case object ColumnRank extends Column(_.columnRank.map(_.toString).getOrElse(new String))
    case object RegistrationDate extends Column(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(registrationDateFormat))).getOrElse(new String))
    case object Status extends Column(x => enumeratedTypeOptionToString(x.status))
    case object SourceOrigin extends Column(_.sourceOrigin.toString)
    case object Table extends Column(_.table.toString)
    case object SourceField extends Column(_.sourceField.toString)
    case object LogicFormat extends Column(_.logicFormat.toString)
    case object FieldDescription extends Column(_.fieldDescription.toString)
    case object UsedYN extends Column(_.usedYN.toString)
    case object DataModelerComments extends Column(_.dataModelerComments.toString)
    case object GlobalArchitectureComments extends Column(_.globalArchitectureComments.toString)
    case object LocalArchitectureComments extends Column(_.localArchitectureComments.toString)
    case object ValidatedByLocalArchitecture extends Column(x => enumeratedTypeOptionToString(x.validatedByLocalArchitecture))
    case object ValidatedByGlobalArchitecture extends Column(x => enumeratedTypeOptionToString(x.validatedByGlobalArchitecture))
    case object FieldCode extends Column(_.fieldCode.toString)
    case object GlobalNamingField extends Column(_.globalNamingField.toString)
    val order = Seq(ColumnRank, RegistrationDate, Status, SourceOrigin, Table, SourceField, LogicFormat, FieldDescription, UsedYN, DataModelerComments, GlobalArchitectureComments, LocalArchitectureComments, ValidatedByLocalArchitecture, ValidatedByGlobalArchitecture,
      FieldCode, GlobalNamingField
    )
  }


  private val registrationDateFormat = "MM/dd/yyyy"
  private val possibleRegistrationDateFormats = Seq(registrationDateFormat, "M/dd/yyyy", "MM/d/yyyy", "M/d/yyyy")

}
