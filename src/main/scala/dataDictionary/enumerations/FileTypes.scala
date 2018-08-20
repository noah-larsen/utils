package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.{NameFormat, ObjectName}

object FileTypes extends Enumerated {

  override type T = FileType
  sealed abstract class FileType(nameFormat: NameFormat = ObjectName()) extends SelfNamed(nameFormat)

  object Avro extends FileType
  object CSV extends FileType
  object Fixed extends FileType
  object Parquet extends FileType
  object TextExtended extends FileType
  object XML extends FileType


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[FileTypes.type], classOf[FileType])

}