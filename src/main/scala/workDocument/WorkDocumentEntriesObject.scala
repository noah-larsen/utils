package workDocument

import java.time.LocalDate

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.`object`.ObjectAndFieldEntries
import dataDictionary.enumerations.IngestionStages
import dataDictionary.field.FieldEntriesObject
import dataDictionary.{DataDictionary, PhysicalNameObject}
import dataDictionary.field.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import exceptions.DataHubException
import workDocument.WorkDocumentEntry.{Statuses, ValidatedValues}

import scala.util.{Failure, Try}

case class WorkDocumentEntriesObject(entries: Seq[WorkDocumentEntry]) {

  //todo ensure entries only have one table name and are non-empty

  def table: String = {
    entries.head.table
  }


  def lowercaseSourceOrigin: Option[String] = {
    Some(entries.map(_.sourceOrigin.toLowerCase).distinct).filter(_.lengthCompare(1) == 0).map(_.head)
  }


  def allFieldsValidatedByLocalAndGlobalArchitecture: Boolean = {
    entries.forall(x => Seq(x.validatedByLocalArchitecture, x.validatedByGlobalArchitecture).forall(_.contains(ValidatedValues.OK)))
  }


  def mergeIfFromTextExtraction(intermediateDataDictionary: DataDictionary, preserveRegistrationDatesThis: Boolean, preserveRegistrationDatesThat: Boolean, columnsWorkDocumentEntriesObjectHasPrecedence: Iterable[FieldEntryColumn]): Try[ObjectAndFieldEntries] = {
    intermediateDataDictionary.objectAndFieldEntries(table).flatMap(x => merge(x.rawFieldEntriesObject, columnsWorkDocumentEntriesObjectHasPrecedence, preserveRegistrationDatesThis, preserveRegistrationDatesThat).map(x.updateFieldEntriesIfFromTextExtraction))
  }


  def withRegistrationDates: WorkDocumentEntriesObject = {
    copy(entries.map(x => if(x.registrationDate.isEmpty) x.copy(registrationDate = Some(LocalDate.now())) else x))
  }


  private def merge(rawFieldEntriesObject: FieldEntriesObject, columnsWorkDocumentEntriesObjectHasPrecedence: Iterable[FieldEntryColumn], preserveRegistrationDatesThis: Boolean, preserveRegistrationDatesThat: Boolean): Try[FieldEntriesObject] = {
    Some(rawFieldEntriesObject).filter(_ => preserveRegistrationDatesThat).getOrElse(rawFieldEntriesObject.withoutRegistrationDates).merge(toRawFieldEntriesObject(preserveRegistrationDatesThis), columnsWorkDocumentEntriesObjectHasPrecedence)
  }


  private def toRawFieldEntriesObject(preserveRegistrationDates: Boolean): FieldEntriesObject = {
    FieldEntriesObject(entries.map(_.toFieldEntry(preserveRegistrationDates)))
  }

}

object WorkDocumentEntriesObject {

  def apply(fieldEntriesObject: FieldEntriesObject, centralNamingsRepository: CentralNamingsRepository, preserveRegistrationDates: Boolean): WorkDocumentEntriesObject = {
    val logicalNameDescriptionSeparator = ". "
    val usedYN_YValue = "Y"
    val usedYN_NValue = "N"
    WorkDocumentEntriesObject(fieldEntriesObject.fieldEntries.map(fieldEntry =>
      WorkDocumentEntry(
        columnRank = Some(fieldEntriesObject.fieldEntries.indexOf(fieldEntry) + 1),
        registrationDate = fieldEntry.registrationDate.filter(_ => preserveRegistrationDates),
        status = Some(Statuses.PendingDataModeler),
        sourceOrigin = fieldEntry.sourceOrigin.getOrElse(new String).toUpperCase,
        table = fieldEntry.physicalNameObject.getOrElse(new String),
        sourceField = fieldEntry.sourceField.getOrElse(new String),
        logicFormat = fieldEntry.logicalFormat.getOrElse(new String),
        fieldDescription = fieldEntry.logicalNameField.map(_ + fieldEntry.simpleFieldDescription.map(logicalNameDescriptionSeparator + _).getOrElse(new String)).getOrElse(fieldEntry.simpleFieldDescription.getOrElse(new String)),
        usedYN = fieldEntry.isFreeField.map(if(_) usedYN_NValue else usedYN_YValue).getOrElse(new String),
        dataModelerComments = new String,
        globalArchitectureComments = new String,
        localArchitectureComments = new String,
        validatedByLocalArchitecture = None,
        validatedByGlobalArchitecture = None,
        fieldCode = new String,
        globalNamingField = fieldEntry.physicalNameField.getOrElse(new String)
      )
    ))
  }

}