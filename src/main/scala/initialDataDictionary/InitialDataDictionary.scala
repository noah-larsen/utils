package initialDataDictionary

import googleSpreadsheets.{GoogleSpreadsheet, RowParametersReader, SheetRange}
import initialDataDictionary.sourceSystem.{SourceSystem, SourceSystemRowParametersReader}

import scala.util.Try

case class InitialDataDictionary(
                                  sourceSystem: SourceSystem
                                ) {

}

object InitialDataDictionary {

  def apply(googleSpreadsheetId: String): Try[InitialDataDictionary] = {
    GoogleSpreadsheet(googleSpreadsheetId).flatMap(x => x.get(SourceSystemRowParametersReader).map(InitialDataDictionary(_)))
  }

}