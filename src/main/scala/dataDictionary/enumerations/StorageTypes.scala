package dataDictionary.enumerations

import dataDictionary.enumerations.IngestionStages.{IngestionStage, Master, Raw}
import initialDataDictionary.enumerations.TargetStorageSuperTypes.{HdfsAvroParquet, TargetStorageSuperType}
import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.Custom

object StorageTypes extends Enumerated {

  override type T = StorageType
  sealed case class StorageType(customName: String) extends SelfNamed(Custom(customName))

  object HdfsAvro extends StorageType("HDFS-Avro")
  object HdfsParquet extends StorageType("HDFS-Parquet")


  def from(targetStorageSuperType: TargetStorageSuperType, ingestionStage: IngestionStage): Option[StorageType] = {
    Some(targetStorageSuperType).filter(_ == HdfsAvroParquet).map(_ => ingestionStage match {
      case Raw => HdfsAvro
      case Master => HdfsParquet
    })
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[StorageTypes.type], classOf[StorageType])

}