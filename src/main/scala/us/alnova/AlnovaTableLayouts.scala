package us.alnova

import dataDictionary.FieldEntry.FieldRowBooleans.Yes
import dataDictionary.ObjectRow.Countries
import dataDictionary.{FieldEntriesObject, FieldEntry, PhysicalNameObject}
import general.DataHubException
import googleSpreadsheets._
import utils.Enumerated
import utils.Enumerated.EnumeratedType

import scala.util.{Failure, Try}

object AlnovaTableLayouts {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    AlnovaSourceSystems.withName(physicalNameObject.sourceSystem).map{ _ =>
      GoogleSpreadsheet(alnovaTableLayoutsGSId).flatMap(_.get(ATLRow).flatMap{ atlRows =>
        Try(atlRows.filter(_.tableName.equalsIgnoreCase(physicalNameObject.dataName)).sortBy(_.columnNumber.toInt))
          .transform(Some(_).filter(_.nonEmpty).map(Try(_)).getOrElse(Failure(DataHubException(s"No entries for table ${physicalNameObject.dataName} in alnova table layouts"))), Failure(_))
          .map(x => Some(x.zip(x.map(_.length.toInt).init.+:(0))).map(y => y.tail.scanLeft((y.head, 1))((z, w) => (w, z._2 + w._2))).get.map(y => (y._1._1, y._2)).map { atlRow_fieldPositionInTheObject =>
            val (atlRow, fieldPositionInTheObject) = atlRow_fieldPositionInTheObject
            FieldEntry(
              physicalNameObject = Some(physicalNameObject.string),
              logicalNameField = Some(atlRow.description).filter(_.trim.nonEmpty),
              sourceField = Some(atlRow.fieldName),
              fieldPositionInTheObject = Some(Some(fieldPositionInTheObject))
              //todo
            )
          })
          .map(FieldEntriesObject(_)
            .withRawFromTextValues
            .withCountryTheConceptualEntity(Countries.UnitedStates)
            .withTrustedDataSource(Yes)
            .withPhysicalNameSourceObject(physicalNameObject.dataName.toUpperCase)
        )
      })
    }
  }


  private object AlnovaSourceSystems extends Enumerated {

    override type T = AlnovaSourceSystem
    case class AlnovaSourceSystem(name: String) extends EnumeratedType

    object Cif extends AlnovaSourceSystem("cif")
    object AlnovaDeposits extends AlnovaSourceSystem("aln_deposits")
    object AlnovaLoans extends AlnovaSourceSystem("aln_loans")


    override val values = Seq(Cif, AlnovaDeposits, AlnovaLoans)

  }


  private val alnovaTableLayoutsGSId = "1-wrvh6pkHAp8v6T_c7h-HCnkp_NCtA8cFDliboG7exM"

}
