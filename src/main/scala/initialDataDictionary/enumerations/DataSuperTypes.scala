package initialDataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}

object DataSuperTypes extends Enumerated {

  override type T = DataSuperType
  sealed abstract class DataSuperType extends SelfNamed

  object DB2 extends DataSuperType
  object DB2UDB extends DataSuperType
  object Netezza extends DataSuperType
  object Oracle extends DataSuperType
  object Postgresql extends DataSuperType
  object SqlServer extends DataSuperType
  object Sybase extends DataSuperType
  object Teradata extends DataSuperType


  override val values = Seq(DB2, DB2UDB, Netezza, Oracle, Postgresql, SqlServer, Sybase, Teradata)

}
