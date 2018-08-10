package initialDataDictionary

import googleSpreadsheets.{GoogleSpreadsheet, RowParametersReader, SheetRange}
import initialDataDictionary.`object`.{ObjectRowReader, Object_}
import initialDataDictionary.field.{Field, FieldRowReader}
import initialDataDictionary.sourceSystem.{SourceSystem, SourceSystemRowParametersReader}

import scala.util.Try

case class InitialDataDictionary(
                                  sourceSystem: SourceSystem,
                                  objects: Seq[Object_],
                                  fields: Seq[Field]
                                ) {

  def lcObjectNameToObjectAndFields: Map[String, ObjectAndFields] = {
    val objectNameToFields = fields.groupBy(_.objectName.toLowerCase).filterKeys(_.nonEmpty)
    objects.collect{case x if x.objectName.nonEmpty => (x.objectName.toLowerCase, ObjectAndFields(x, objectNameToFields.getOrElse(x.objectName.toLowerCase, Seq()), sourceSystem))}.toMap
  }

}

object InitialDataDictionary {

  def apply(googleSpreadsheetId: String): Try[InitialDataDictionary] = {
    Try {
      val sourceSystem = GoogleSpreadsheet(googleSpreadsheetId).flatMap(x => x.get(SourceSystemRowParametersReader)).get
      val objects = GoogleSpreadsheet(googleSpreadsheetId).flatMap(_.get(ObjectRowReader(sourceSystem))).get.filter(_.objectName.nonEmpty)
      val fields = GoogleSpreadsheet(googleSpreadsheetId).flatMap(_.get(FieldRowReader(sourceSystem, objects.map(x => (x.objectName, x)).toMap))).get
      InitialDataDictionary(sourceSystem, objects, fields)
    }
  }

}