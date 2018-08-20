package us.alnova

import dataDictionary.enumerations.DefaultValues
import dataDictionary.enumerations.YesOrNoValues.Yes
import dataDictionary.enumerations.Countries
import dataDictionary.field.FieldEntriesObject
import dataDictionary.types.databases.DB2Types
import dataDictionary.types.databases.DB2Types.DB2Type
import dataDictionary.{PhysicalNameObject, Type}
import exceptions.DataHubException
import googleSpreadsheets._
import us.alnova.AlnovaTableLayouts.AlnovaSourceSystems
import utils.enumerated.Enumerated
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Lowercase
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithUnderscoresBetweenWords

import scala.util.{Failure, Try}

object AlnovaTableLayouts {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {

    def db2Type(dB2Type: DB2Type, length: Int): Option[Type[DB2Type]] = {
      dB2Type match {
        case DB2Types.Decimal => None
        case DB2Types.Float => None
        case x if x == DB2Types.Char => Some(Type(x, Some(length)))
        case x => Some(Type(x))
      }
    }

//    AlnovaSourceSystems.withName(physicalNameObject.sourceSystem).map{ _ =>
//      GoogleSpreadsheet(alnovaTableLayoutsGSId).flatMap(_.get(ATLRow).flatMap{ atlRows =>
//        Try(atlRows.filter(_.tableName.equalsIgnoreCase(physicalNameObject.objectName)).sortBy(_.columnNumber.toInt))
//          .transform(Some(_).filter(_.nonEmpty).map(Try(_)).getOrElse(Failure(DataHubException(s"No entries for table ${physicalNameObject.objectName} in alnova table layouts"))), Failure(_))
//          .map(x => Some(x.zip(x.map(_.length.toInt).init.+:(0))).map(y => y.tail.scanLeft((y.head, 1))((z, w) => (w, z._2 + w._2))).get.map(y => (y._1._1, y._2)).map { atlRow_fieldPositionInTheObject =>
//            val (atlRow, fieldPositionInTheObject) = atlRow_fieldPositionInTheObject
//            FieldEntry(
//              physicalNameObject = Some(physicalNameObject.string),
//              logicalNameField = Some(atlRow.description).filter(_.trim.nonEmpty),
//              logicalFormat = DB2Types.withName(atlRow.type_).flatMap(db2Type(_, atlRow.length.toInt).flatMap(_.logicalFormat.map(_.string))),
//              defaultValue = Some(Unit).filter(_ => atlRow.areNullsAllowed).map(_ => DefaultValues.null_),
//              sourceField = Some(atlRow.fieldName),
//              fieldPositionInTheObject = Some(Some(fieldPositionInTheObject))
//            )
//          })
//          .map(FieldEntriesObject(_)
//            .withRawFromTextValues
//            .withCountryTheConceptualEntity(Countries.UnitedStates)
//            .withTrustedDataSource(Yes)
//            .withPhysicalNameSourceObject(physicalNameObject.objectName.toUpperCase)
//        )
//      })
//    }

    ???

  }


  private object AlnovaSourceSystems extends Enumerated {

    override type T = AlnovaSourceSystem
    sealed abstract class AlnovaSourceSystem extends SelfNamed(ObjectNameWithUnderscoresBetweenWords(Lowercase))

    object AlnCif extends AlnovaSourceSystem
    object AlnDeposits extends AlnovaSourceSystem
    object AlnLoans extends AlnovaSourceSystem


    override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[AlnovaSourceSystems.type], classOf[AlnovaSourceSystem])

  }


  private val alnovaTableLayoutsGSId = "1-wrvh6pkHAp8v6T_c7h-HCnkp_NCtA8cFDliboG7exM"

}
