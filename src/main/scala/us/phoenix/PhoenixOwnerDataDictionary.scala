package us.phoenix

import dataDictionary.enumerations._
import dataDictionary.enumerations.YesOrNoValues.Yes
import dataDictionary.types.databases.OracleTypes
import dataDictionary.{FieldEntriesObject, FieldEntry, PhysicalNameObject, Type}
import exceptions.{DataHubException, InitialDataDictionaryNotFoundException}
import googleSpreadsheets.CheckboxValues.False
import googleSpreadsheets.{CheckboxValues, GoogleSpreadsheet}
import initialDataDictionary.{InitialDataDictionary, ObjectAndFields}
import initialDataDictionary.`object`.Object_
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes, TokenizationTypes}
import initialDataDictionary.field.Field
import initialDataDictionary.sourceSystem.SourceSystem
import us.alnova.ATLRow
import us.alnova.AlnovaTableLayouts.alnovaTableLayoutsGSId

import scala.util.matching.Regex
import scala.util.{Failure, Try}

object PhoenixOwnerDataDictionary {

  def updateInitialDataDictionary(lcSourceSystemToInitialDataDictionary: Map[String, InitialDataDictionary]): Try[Unit] = Try {
    val phoenixInitialDataDictionary = lcSourceSystemToInitialDataDictionary.getOrElse(phoenixSourceSystem, throw InitialDataDictionaryNotFoundException(phoenixSourceSystem))
    objectsAndFields.flatMap(phoenixInitialDataDictionary.appendNewObjectsAndFields)
  }



  private def objectsAndFields: Try[Seq[ObjectAndFields]] = {
    GoogleSpreadsheet(phoenixOwnerDataDictionaryGSId).flatMap(_.get(PODDRow).map(_.map(x => (x, x.tableName.toUpperCase)).groupBy(_._2).mapValues(_.map(_._1)).map { tableName_PODDFields =>
      val (tableName, poddFields) = (tableName_PODDFields._1, tableName_PODDFields._2)
      ObjectAndFields(
        Object_(
          businessEngineeringStewardComplete = CheckboxValues.toBoolean(False),
          dataOwnerComplete = CheckboxValues.toBoolean(False),
          dataArchitectureComplete = CheckboxValues.toBoolean(False),
          ingestionComplete = CheckboxValues.toBoolean(False),
          objectName = tableName,
          logicalName = new String,
          description = new String,
          loadingType = None,
          frequency = None,
          mailbox = new String,
          sourceOperational = new String,
          extractionFileType = None,
          extractionFileDelimeter = new String,
          objectType = None,
          dataSuperType = None,
          isIngestedFromFixedWidth = None,
          currentDepth = new String,
          perimeter = new String,
          informationLevel = new String,
          estimatedVolumeRecords = new String,
          technicalResponsible = new String,
          informationGroup = None,
          isCore = None,
          isTDS = None,
          countryTheDataSource = None,
          timeRequirement = new String,
          requiredDepth = new String,
          systemCodeUUAA = new String,
          dataSource = new String,
          stagingPath = new String,
          rawPath = new String,
          masterPath = new String,
          targetStorageSuperType = None,
          partitions = Seq(),
          stagingToRawSchemasPath = new String,
          rawToMasterSchemasPath = new String
        ),
        poddFields.zipWithIndex.map { poddField_index =>
          val (poddField, index) = (poddField_index._1, poddField_index._2)
          Field(
            objectName = poddField.tableName,
            fieldName = poddField.columnName,
            index = Some(index + 1),
            logicalName = poddField.logicalName,
            description = poddField.description,
            dataType = poddField.dataType,
            isKey = CheckboxValues.toBoolean(False),
            dateFormat = new String,
            length = None,
            catalog = new String,
            conceptualEntity = new String,
            meetsTokenizationCriteria = CheckboxValues.toBoolean(False),
            isTDS = None,
            countryTheConceptualEntity = None,
            operationalEntity = new String,
            isMandatoryNonKey = CheckboxValues.toBoolean(False),
            tokenizationType = None,
            defaultValue = new String
          )
        },
        SourceSystem.empty
      )
    }.toSeq.sortBy(_.obj.objectName)))
  }


  private val phoenixOwnerDataDictionaryGSId = "1n7mqabYfD7ZHDH85KIFRqQWrj5z97Vvyi1PXOtoSmU0"
  private val phoenixSourceSystem = "phx"
  private val nullableIsNullableValue = "Y"

}
