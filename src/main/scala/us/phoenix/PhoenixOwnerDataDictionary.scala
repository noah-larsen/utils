package us.phoenix

import dataDictionary.FieldEntry.DefaultValues
import dataDictionary.FieldEntry.FieldRowBooleans.Yes
import dataDictionary.ObjectRow.Countries
import dataDictionary.{FieldEntriesObject, FieldEntry, PhysicalNameObject}
import general.DataHubException
import googleSpreadsheets.GoogleSpreadsheet
import us.alnova.ATLRow
import us.alnova.AlnovaTableLayouts.alnovaTableLayoutsGSId

import scala.util.{Failure, Try}

object PhoenixOwnerDataDictionary {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    Some(Unit).filter(_ => physicalNameObject.sourceSystem.equals(phoenixSourceSystem)).map(_ =>
      GoogleSpreadsheet(phoenixOwnerDataDictionaryGSId).flatMap(_.get(PODDRow).flatMap{ poddRows =>
        Try(poddRows.filter(_.tableName.equalsIgnoreCase(physicalNameObject.dataName)).map(poddRow =>
          FieldEntry(
            physicalNameObject = Some(physicalNameObject.string),
            logicalNameField = Some(poddRow.logicalName).filter(_.nonEmpty),
            simpleFieldDescription = Some(poddRow.description).filter(_.nonEmpty),
            defaultValue = Some(poddRow.nullOption).filter(_.trim.equalsIgnoreCase(nullOptionNullValue)).map(_ => DefaultValues.null_),
            sourceField = Some(poddRow.columnName)
            //todo data type
          )
        )).filter(_.nonEmpty).recoverWith{case e => Failure(DataHubException(s"No entries for table ${physicalNameObject.dataName} in alnova table layouts"))}
          .map(FieldEntriesObject(_)
            .withRawFromTextValues
            .withCountryTheConceptualEntity(Countries.UnitedStates)
            .withTrustedDataSource(Yes) //todo ?
            .withPhysicalNameSourceObject(physicalNameObject.dataName.toUpperCase)
          )
      })

    )
  }


  private val phoenixOwnerDataDictionaryGSId = "1qf3svMM2OQNgifC43lJpRgQxhW17rk2L6qgdS_kbpVQ"
  private val phoenixSourceSystem = "phx"
  private val nullOptionNullValue = "Null"

}
