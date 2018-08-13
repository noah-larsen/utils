package initialDataDictionary

import googleSpreadsheets.{GoogleSpreadsheet, RowParametersReader, SheetRange}
import initialDataDictionary.`object`.{ObjectRowReader, Object_}
import initialDataDictionary.field.{Field, FieldRowReader}
import initialDataDictionary.sourceSystem.{SourceSystem, SourceSystemRowParametersReader}

import scala.util.Try

case class InitialDataDictionary(private val spreadsheet: GoogleSpreadsheet) {

  def lcObjectNameToObjectAndFields: Try[Map[String, ObjectAndFields]] = Try {
    val data_ = data.get
    val objectNameToFields = data_.fields.groupBy(_.objectName.toLowerCase).filterKeys(_.nonEmpty)
    data_.objects.collect{case x if x.objectName.nonEmpty => (x.objectName.toLowerCase, ObjectAndFields(x, objectNameToFields.getOrElse(x.objectName.toLowerCase, Seq()), data_.sourceSystem))}.toMap
  }


  def appendNewObjectsAndFields(objectsAndFields: Seq[ObjectAndFields]): Try[Unit] = Try {
    val data_ = data.get
    val lcObjectNames = (data_.objects.map(_.objectName) ++ data_.fields.map(_.objectName)).map(_.toLowerCase).toSet
    objectsAndFields.filter(x => !lcObjectNames.contains(x.obj.objectName.toLowerCase))
//    spreadsheet.append(objectsAndFields.map(_.obj), )
    ???
  }


  private def data: Try[Data] = Try {
    val sourceSystem = spreadsheet.get(SourceSystemRowParametersReader).get
    val objects = spreadsheet.get(ObjectRowReader(sourceSystem)).map(_.filter(_.objectName.nonEmpty)).get
    val fields = spreadsheet.get(FieldRowReader(sourceSystem, objects.map(x => (x.objectName, x)).toMap)).get
    Data(sourceSystem, objects, fields)
  }


  private case class Data(
                           sourceSystem: SourceSystem,
                           objects: Seq[Object_],
                           fields: Seq[Field]
                         )

}

object InitialDataDictionary {

  def apply(googleSpreadsheetId: String): Try[InitialDataDictionary] = {
    GoogleSpreadsheet(googleSpreadsheetId).map(InitialDataDictionary(_))
  }

}
