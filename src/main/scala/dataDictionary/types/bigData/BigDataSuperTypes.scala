package dataDictionary.types.bigData

import dataDictionary.Type
import dataDictionary.types.LogicalFormats.LogicalFormat
import dataDictionary.types.SuperTypes

trait BigDataSuperTypes extends SuperTypes {

  def fromLogicalFormat(logicalFormat: Type[LogicalFormat], convertTimestampToString: Boolean): Type[T]

}
