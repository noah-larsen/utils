package us

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
      val gs = GoogleSpreadsheet(alnovaTableLayoutsGSId).get
      gs.get(ATLRow).flatMap{ atlRows =>
        Try(atlRows.filter(_.tableName.equalsIgnoreCase(physicalNameObject.dataName)).map(atlRow =>
          FieldEntry(
            physicalNameObject = Some(physicalNameObject.string),
            logicalNameField = Some(atlRow.description).filter(_.trim.nonEmpty),
            sourceField = Some(atlRow.fieldName)
            //todo
          )
        )).filter(_.nonEmpty).recoverWith{case e => Failure(DataHubException(s"${physicalNameObject.dataName} entries do not exist in Alnova Table Layouts"))}
          .map(FieldEntriesObject(_)
            .withRawFromTextValues
            .withCountryTheConceptualEntity(Countries.UnitedStates)
            .withTrustedDataSource(Yes)
            .withPhysicalNameSourceObject(physicalNameObject.dataName.toUpperCase)
        )
      }
    }
  }


  object AlnovaSourceSystems extends Enumerated {

    override type T = AlnovaSourceSystem
    case class AlnovaSourceSystem(name: String) extends EnumeratedType

    object Cif extends AlnovaSourceSystem("cif")
    object AlnovaDeposits extends AlnovaSourceSystem("aln_deposits")
    object AlnovaLoans extends AlnovaSourceSystem("aln_loans")


    override val values = Seq(Cif, AlnovaDeposits, AlnovaLoans)

  }


  private val alnovaTableLayoutsGSId = "1-wrvh6pkHAp8v6T_c7h-HCnkp_NCtA8cFDliboG7exM"


  private case class ATLRow(
                           tableName: String,
                           fieldName: String,
                           columnNumber: String,
                           type_ : String,
                           length: String,
                           nullsAllowed: String,
                           description: String
                         ) extends Row

  private object ATLRow extends DataReader[ATLRow] {

    override def sheetRange: SheetRange = SheetRange("ben2", "G", 2)


    override def toRow(row: Int => String): ATLRow = {
      ATLRow(row(0), row(1), row(2), row(3), row(4), row(5), row(6))
    }

  }

}
