package dataDictionary.field

import java.time.LocalDate

import dataDictionary.Type
import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.FieldGeneratedValues.FieldGeneratedValue
import dataDictionary.enumerations.StorageTypes.StorageType
import dataDictionary.enumerations.StorageZones.StorageZone
import dataDictionary.enumerations.{FieldGeneratedValues, YesOrNoValues}
import dataDictionary.enumerations.YesOrNoValues.YesOrNo
import dataDictionary.field.FieldEntryColumns._
import dataDictionary.types.LogicalFormats
import googleSpreadsheets.Row

case class FieldEntry(
                       country: Option[Country],
                       physicalNameObject: Option[String],
                       storageType: Option[StorageType],
                       storageZone: Option[StorageZone],
                       physicalNameField: Option[String],
                       logicalNameField: Option[String],
                       simpleFieldDescription: Option[String],
                       catalog: Option[String],
                       dataType: Option[String],
                       format: Option[String],
                       logicalFormat: Option[String],
                       key: Option[YesOrNo],
                       mandatory: Option[YesOrNo],
                       defaultValue: Option[String],
                       physicalNameSourceObject: Option[String],
                       sourceField: Option[String],
                       dataTypeSourceField: Option[String],
                       formatSourceField: Option[String],
                       tags: Option[Seq[String]],
                       fieldPositionInTheObject: Option[Option[Int]],
                       generatedField: Option[FieldGeneratedValue],
                       tokenizationType: Option[String],
                       registrationDate: Option[LocalDate],
                       countryTheConceptualEntity: Option[Country],
                       conceptualEntity: Option[String],
                       operationalEntity: Option[String],
                       tds: Option[YesOrNo]
                     ) extends Row {

  def merge(fieldEntry: FieldEntry, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn] = Seq()): FieldEntry = {

    def mergedValue[T](toValue: FieldEntry => Option[T], fieldEntryColumn: FieldEntryColumn): Option[T] = {
      columnsArgumentHasPrecedence.find(_ == fieldEntryColumn).map(_ => toValue(fieldEntry).orElse(toValue(this))).getOrElse(toValue(this).orElse(toValue(fieldEntry)))
    }


    FieldEntry(
      mergedValue(_.country, Country),
      mergedValue(_.physicalNameObject, PhysicalNameObject),
      mergedValue(_.storageType, FieldEntryColumns.StorageType),
      mergedValue(_.storageZone, StorageZone),
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

  def isDateOrTimestamp: Option[Boolean] = {
    logicalFormat.flatMap(Type(_, LogicalFormats).flatMap(_.logicalFormat).map(x => Seq(LogicalFormats.Date, LogicalFormats.Timestamp).contains(x.typeType)))
  }


  def isFreeField: Option[Boolean] = {
    val freeFieldPrefix = "free_field"
    physicalNameField.map(_.startsWith(freeFieldPrefix))
  }


  def isGenerated: Boolean = {
    generatedField.contains(FieldGeneratedValues.Yes)
  }


  def isKey: Option[Boolean] = {
    key.map(YesOrNoValues.toBoolean)
  }


  def isMandatory: Option[Boolean] = {
    mandatory.map(YesOrNoValues.toBoolean)
  }


  def sourceOrigin: Option[String] = {
    val physicalNameSeparator = "_"
    val sourceOriginIndex = 2
    physicalNameObject.flatMap(x => Some(x.split(physicalNameSeparator)).filter(_.length > sourceOriginIndex + 1).map(_(sourceOriginIndex)))
  }

}

object FieldEntry {

  def decimalFormat(precision: Int, scale: Option[Int]): String = {
    val prefix = "("
    val suffix = ")"
    val precisionScaleSeparator = ","
    googleSpreadsheets.asPlainTextSpreadsheetFormula(prefix + precision + scale.map(precisionScaleSeparator + _).getOrElse(new String) + suffix)
  }

}
