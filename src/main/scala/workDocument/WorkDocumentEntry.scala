package workDocument

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import dataDictionary.Type
import dataDictionary.enumerations.{DataTypes, YesOrNoValues}
import dataDictionary.field.FieldEntry
import googleSpreadsheets.{Column, Columns}
import googleSpreadsheets.{Row, RowReaderWriter, SheetRange}
import utils.enumerated.Enumerated
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.Custom
import workDocument.WorkDocumentEntry.WorkDocumentColumns.WorkDocumentColumn
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
      country = None,
      physicalNameObject = toStringOption(table),
      storageType = None,
      storageZone = None,
      physicalNameField = toStringOption(globalNamingField),
      logicalNameField = None,
      simpleFieldDescription = toStringOption(fieldDescription),
      catalog = None,
      dataType = None,
      format = None,
      logicalFormat = toStringOption(logicFormat),
      key = None,
      mandatory = None,
      defaultValue = None,
      physicalNameSourceObject = None,
      sourceField = toStringOption(sourceField),
      dataTypeSourceField = None,
      formatSourceField = None,
      tags = None,
      fieldPositionInTheObject = None,
      generatedField = None,
      tokenizationType = None,
      registrationDate = None,
      countryTheConceptualEntity = None,
      conceptualEntity = None,
      operationalEntity = None,
      tds = None
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
      columnRank = Some(r(columns.indexOf(WorkDocumentColumns.ColumnRank))).filter(x => Try(x.toInt).isSuccess).map(_.toInt), //todo reconsider whole error handling strategy here
      registrationDate = possibleRegistrationDateFormats.map(x => Try(LocalDate.parse(r(columns.indexOf(WorkDocumentColumns.RegistrationDate)), DateTimeFormatter.ofPattern(x)))).maxBy(_.isSuccess).toOption,
      status = dropDown(Statuses, r(columns.indexOf(WorkDocumentColumns.Status))),
      sourceOrigin = r(columns.indexOf(WorkDocumentColumns.SourceOrigin)),
      table = r(columns.indexOf(WorkDocumentColumns.Table)),
      sourceField = r(columns.indexOf(WorkDocumentColumns.SourceField)),
      logicFormat = r(columns.indexOf(WorkDocumentColumns.LogicFormat)),
      fieldDescription = r(columns.indexOf(WorkDocumentColumns.FieldDescription)),
      usedYN = r(columns.indexOf(WorkDocumentColumns.UsedYN)),
      dataModelerComments = r(columns.indexOf(WorkDocumentColumns.DataModelerComments)),
      globalArchitectureComments = r(columns.indexOf(WorkDocumentColumns.GlobalArchitectureComments)),
      localArchitectureComments = r(columns.indexOf(WorkDocumentColumns.LocalArchitectureComments)),
      validatedByLocalArchitecture = dropDown(ValidatedValues, r(columns.indexOf(WorkDocumentColumns.ValidatedByLocalArchitecture))),
      validatedByGlobalArchitecture = dropDown(ValidatedValues, r(columns.indexOf(WorkDocumentColumns.ValidatedByGlobalArchitecture))),
      fieldCode = r(columns.indexOf(WorkDocumentColumns.FieldCode)),
      globalNamingField = r(columns.indexOf(WorkDocumentColumns.GlobalNamingField))
    )
  }


  override protected def columns: Seq[WorkDocumentColumn] = WorkDocumentColumns.values


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


    val values = Seq(ColumnRank, RegistrationDate, Status, SourceOrigin, Table, SourceField, LogicFormat, FieldDescription, UsedYN, DataModelerComments, GlobalArchitectureComments, LocalArchitectureComments, ValidatedByLocalArchitecture, ValidatedByGlobalArchitecture,
      FieldCode, GlobalNamingField
    )

  }


  private val registrationDateFormat = "MM/dd/yyyy"
  private val possibleRegistrationDateFormats = Seq(registrationDateFormat, "M/dd/yyyy", "MM/d/yyyy", "M/d/yyyy")

}
