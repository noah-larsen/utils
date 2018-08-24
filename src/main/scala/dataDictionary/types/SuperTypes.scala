package dataDictionary.types

import dataDictionary.Type
import dataDictionary.types.databases.{DB2Types, OracleTypes}
import initialDataDictionary.enumerations.DataSuperTypes._
import utils.enumerated.Enumerated

trait SuperTypes extends Enumerated {

  override type T <: SuperType

}

object SuperTypes {

  def from(dataSuperType: DataSuperType): SuperTypes = {
    dataSuperType match {
      case DB2 => DB2Types
      case DB2UDB => ???
      case Netezza => ???
      case Oracle => OracleTypes
      case Postgresql => ???
      case SqlServer => ???
      case Sybase => ???
      case Teradata => ???
    }
  }

}