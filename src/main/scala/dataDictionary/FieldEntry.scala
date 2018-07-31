package dataDictionary

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import dataDictionary.FieldEntry.FieldGeneratedValues.FieldGeneratedValue
import dataDictionary.FieldEntry.FieldRowBooleans.FieldRowBoolean
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns._
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns
import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.StorageTypes.StorageType
import dataDictionary.ObjectRow.StorageZones.StorageZone
import googleSpreadsheets.{DataReaderWriter, Row, SheetRange}
import utils.enumerated.Enumerated
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Uppercase
import utils.enumerated.SelfNamed.NameFormats.ObjectName

import scala.util.Try

case class FieldEntry(
                       country: Option[Country] = None,
                       physicalNameObject: Option[String] = None,
                       storageType: Option[StorageType] = None,
                       storageZone: Option[StorageZone] = None,
                       physicalNameField: Option[String] = None,
                       logicalNameField: Option[String] = None,
                       simpleFieldDescription: Option[String] = None,
                       catalog: Option[String] = None,
                       dataType: Option[String] = None,
                       format: Option[String] = None,
                       logicalFormat: Option[String] = None,
                       key: Option[FieldRowBoolean] = None,
                       mandatory: Option[FieldRowBoolean] = None,
                       defaultValue: Option[String] = None,
                       physicalNameSourceObject: Option[String] = None,
                       sourceField: Option[String] = None,
                       dataTypeSourceField: Option[String] = None,
                       formatSourceField: Option[String] = None,
                       tags: Option[Seq[String]] = None,
                       fieldPositionInTheObject: Option[Option[Int]] = None,
                       generatedField: Option[FieldGeneratedValue] = None,
                       tokenizationType: Option[String] = None,
                       registrationDate: Option[LocalDate] = None,
                       countryTheConceptualEntity: Option[Country] = None,
                       conceptualEntity: Option[String] = None,
                       operationalEntity: Option[String] = None,
                       tds: Option[FieldRowBoolean] = None
                     ) extends Row {

  def merge(fieldEntry: FieldEntry, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn] = Seq()): FieldEntry = {

    def mergedValue[T](toValue: FieldEntry => Option[T], fieldEntryColumn: FieldEntryColumn): Option[T] = {
      columnsArgumentHasPrecedence.find(_ == fieldEntryColumn).map(_ => toValue(fieldEntry).orElse(toValue(this))).getOrElse(toValue(this).orElse(toValue(fieldEntry)))
    }


    FieldEntry(
      mergedValue(_.country, FieldEntryColumns.Country),
      mergedValue(_.physicalNameObject, FieldEntryColumns.PhysicalNameObject),
      mergedValue(_.storageType, FieldEntryColumns.StorageType),
      mergedValue(_.storageZone, FieldEntryColumns.StorageZone),
      mergedValue(_.physicalNameField, PhysicalNameField),
      mergedValue(_.logicalNameField, LogicalNameField),
      mergedValue(_.simpleFieldDescription, SimpleFieldDescription),
      mergedValue(_.catalog, Catalog),
      mergedValue(_.dataType, DataType),
      mergedValue(_.format, Format),
      mergedValue(_.logicalFormat, FieldEntryColumns.LogicalFormat),
      mergedValue(_.key, Key),
      mergedValue(_.mandatory, Mandatory),
      mergedValue(_.defaultValue, DefaultValue),
      mergedValue(_.physicalNameSourceObject, PhysicalNameSourceObject),
      mergedValue(_.sourceField, SourceField),
      mergedValue(_.dataTypeSourceField, DataTypeSourceField),
      mergedValue(_.formatSourceField, FormatSourceField),
      mergedValue(_.tags, Tags),
      mergedValue(_.fieldPositionInTheObject, FieldPositionInTheObject),
      mergedValue(_.generatedField, ExcludeInclude),
      mergedValue(_.tokenizationType, TokenizationType),
      mergedValue(_.registrationDate, RegistrationDate),
      mergedValue(_.countryTheConceptualEntity, CountryTheConceptualEntity),
      mergedValue(_.conceptualEntity, ConceptualEntity),
      mergedValue(_.operationalEntity, OperationalEntity),
      mergedValue(_.tds, Tds)
    )

  }


  def isFreeField: Option[Boolean] = {
    val freeFieldPrefix = "free_field"
    physicalNameField.map(_.startsWith(freeFieldPrefix))
  }


  def sourceOrigin: Option[String] = {
    val physicalNameSeparator = "_"
    val sourceOriginIndex = 2
    physicalNameObject.flatMap(x => Some(x.split(physicalNameSeparator)).filter(_.length > sourceOriginIndex + 1).map(_(sourceOriginIndex)))
  }

}

object FieldEntry {

  object IngestionStages {
    sealed trait IngestionStage
    object Raw extends IngestionStage
    object Master extends IngestionStage
  }


  object FieldGeneratedValues extends Enumerated {

    override type T = FieldGeneratedValue
    sealed abstract class FieldGeneratedValue extends SelfNamed(ObjectName(Uppercase))

    object Yes extends FieldGeneratedValue


    override val values = Seq(Yes)

  }


  object FieldRowBooleans extends Enumerated {

    override type T = FieldRowBoolean
    sealed abstract class FieldRowBoolean extends SelfNamed(ObjectName(Uppercase))

    object Yes extends FieldRowBoolean
    object No extends FieldRowBoolean


    override val values = Seq(Yes, No)

  }


  object DataTypes {
    val string = "STRING"
  }


  object DefaultValues {
    val null_ = "NULL"
  }

}