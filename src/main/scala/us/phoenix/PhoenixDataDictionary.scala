package us.phoenix

import dataDictionary.FieldEntry.DefaultValues
import dataDictionary.FieldEntry.YesOrNoValues.Yes
import dataDictionary.ObjectRow.Countries
import dataDictionary.types.databases.OracleTypes
import dataDictionary.{FieldEntriesObject, FieldEntry, PhysicalNameObject, Type}
import exceptions.DataHubException
import googleSpreadsheets.GoogleSpreadsheet
import us.alnova.ATLRow
import us.alnova.AlnovaTableLayouts.alnovaTableLayoutsGSId

import scala.util.{Failure, Try}

object PhoenixDataDictionary {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    Some(Unit).filter(_ => physicalNameObject.sourceSystem.equals(phoenixSourceSystem)).map(_ =>
      GoogleSpreadsheet(phoenixOwnerDataDictionaryGSId).flatMap(_.get(PODDRow).flatMap{ poddRows =>
        Try(poddRows.filter(_.tableName.equalsIgnoreCase(physicalNameObject.dataName)).map(poddRow =>
          FieldEntry(
            physicalNameObject = Some(physicalNameObject.string),
            logicalNameField = Some(poddRow.logicalName).filter(_.nonEmpty),
            simpleFieldDescription = Some(poddRow.description).filter(_.nonEmpty),
            logicalFormat = Type(poddRow.dataType, OracleTypes).flatMap(_.logicalFormat.map(_.string)),
            defaultValue = Some(poddRow.nullable).filter(_.trim.equalsIgnoreCase(nullableIsNullableValue)).map(_ => DefaultValues.null_),
            sourceField = Some(poddRow.columnName)
          )
        )).filter(_.nonEmpty).recoverWith{case e: Exception => Failure(DataHubException(s"No entries for table ${physicalNameObject.dataName} in alnova table layouts"))}
          .map(FieldEntriesObject(_)
            .withRawFromTextValues
            .withCountryTheConceptualEntity(Countries.UnitedStates)
            .withTrustedDataSource(Yes)
            .withPhysicalNameSourceObject(physicalNameObject.dataName.toUpperCase)
          )
      })

    )
  }


  private val phoenixOwnerDataDictionaryGSId = "1n7mqabYfD7ZHDH85KIFRqQWrj5z97Vvyi1PXOtoSmU0"
  private val phoenixSourceSystem = "phx"
  private val nullableIsNullableValue = "Y"

}
