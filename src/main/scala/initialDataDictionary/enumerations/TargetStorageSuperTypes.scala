package initialDataDictionary.enumerations

import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import utils.enumerated.{Enumerated, SelfNamed}

object TargetStorageSuperTypes extends Enumerated {

  override type T = TargetStorageSuperType
  sealed abstract class TargetStorageSuperType extends SelfNamed

  object DB2 extends TargetStorageSuperType
  object Elasticsearch extends TargetStorageSuperType
  object HdfsAvroParquet extends TargetStorageSuperType
  object MongoDB extends TargetStorageSuperType
  object Netezza extends TargetStorageSuperType
  object Neo4J extends TargetStorageSuperType
  object Oracle extends TargetStorageSuperType
  object Postgresql extends TargetStorageSuperType
  object SAS extends TargetStorageSuperType
  object SqlServer extends TargetStorageSuperType
  object Sybase extends TargetStorageSuperType
  object Teradata extends TargetStorageSuperType


  override val values = Seq(DB2, Elasticsearch, HdfsAvroParquet, MongoDB, Netezza, Neo4J, Oracle, Postgresql, SAS, SqlServer, Sybase, Teradata)

}