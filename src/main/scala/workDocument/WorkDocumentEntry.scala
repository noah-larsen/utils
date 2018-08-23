package workDocument

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import dataDictionary.field.FieldEntry
import googleSpreadsheets.Column
import googleSpreadsheets.{Row, RowReaderWriter, SheetRange}
import workDocument.WorkDocumentColumns._
import workDocument.enumerations.{Statuses, ValidatedValues}
import workDocument.enumerations.Statuses.Status
import workDocument.enumerations.ValidatedValues.ValidatedValue

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


  override protected def toRow(row: Map[Column[WorkDocumentEntry], String]): WorkDocumentEntry = {
    WorkDocumentEntry(
      columnRank = Some(row(ColumnRank)).filter(x => Try(x.toInt).isSuccess).map(_.toInt), //todo reconsider whole error handling strategy here
      registrationDate = possibleRegistrationDateFormats.map(x => Try(LocalDate.parse(row(RegistrationDate), DateTimeFormatter.ofPattern(x)))).maxBy(_.isSuccess).toOption,
      status = dropDown(Statuses, row(WorkDocumentColumns.Status)),
      sourceOrigin = row(SourceOrigin),
      table = row(Table),
      sourceField = row(SourceField),
      logicFormat = row(LogicFormat),
      fieldDescription = row(FieldDescription),
      usedYN = row(UsedYN),
      dataModelerComments = row(DataModelerComments),
      globalArchitectureComments = row(GlobalArchitectureComments),
      localArchitectureComments = row(LocalArchitectureComments),
      validatedByLocalArchitecture = dropDown(ValidatedValues, row(ValidatedByLocalArchitecture)),
      validatedByGlobalArchitecture = dropDown(ValidatedValues, row(ValidatedByGlobalArchitecture)),
      fieldCode = row(FieldCode),
      globalNamingField = row(GlobalNamingField)
    )
  }


  override protected def columns: Seq[WorkDocumentColumn] = WorkDocumentColumns.values
  

  val registrationDateFormat = "MM/dd/yyyy"
  private val possibleRegistrationDateFormats = Seq(registrationDateFormat, "M/dd/yyyy", "MM/d/yyyy", "M/d/yyyy")

}
