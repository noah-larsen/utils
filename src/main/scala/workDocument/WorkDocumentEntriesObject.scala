package workDocument

import java.time.LocalDate

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.FieldEntry.IngestionStages
import dataDictionary.{DataDictionary, FieldEntriesObject}
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import general.DataHubException
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


  def toFieldEntriesObject(preserveRegistrationDates: Boolean): FieldEntriesObject = {
    FieldEntriesObject(entries.map(_.toFieldEntry(preserveRegistrationDates)))
  }


  def merge(fieldEntriesObject: FieldEntriesObject, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn], preserveRegistrationDatesThis: Boolean, preserveRegistrationDatesThat: Boolean): FieldEntriesObject = {
    toFieldEntriesObject(preserveRegistrationDatesThis).merge(Some(fieldEntriesObject).filter(_ => preserveRegistrationDatesThat).getOrElse(fieldEntriesObject.withoutRegistrationDates), columnsArgumentHasPrecedence)
  }


  def merge(workingDataDictionary: DataDictionary, preserveRegistrationDatesThis: Boolean, preserveRegistrationDatesThat: Boolean, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn] = Seq()): Try[FieldEntriesObject] = {
    workingDataDictionary.fieldEntriesObject(IngestionStages.Raw, table).flatMap {
      case Some(fieldEntriesObject) => Try(merge(fieldEntriesObject, columnsArgumentHasPrecedence, preserveRegistrationDatesThis, preserveRegistrationDatesThat))
      case None => Failure(DataHubException("The object does not have corresponding mergeable entries in the data dictionary."))
    }
  }


  def withRegistrationDates: WorkDocumentEntriesObject = {
    copy(entries.map(x => if(x.registrationDate.isEmpty) x.copy(registrationDate = Some(LocalDate.now())) else x))
  }

}

object WorkDocumentEntriesObject {

  def apply(fieldEntriesObject: FieldEntriesObject, centralNamingsRepository: CentralNamingsRepository, preserveRegistrationDates: Boolean): WorkDocumentEntriesObject = {
    val globalNameSet = centralNamingsRepository.globalNameSet
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