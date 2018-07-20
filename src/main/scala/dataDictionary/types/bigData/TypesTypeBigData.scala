package dataDictionary.types.bigData

import dataDictionary.Type
import dataDictionary.Type.TypesType
import dataDictionary.types.LogicalFormats.LogicalFormat

trait TypesTypeBigData extends TypesType {

  def fromLogicalFormat(logicalFormat: Type[LogicalFormat]): Type[T]

}
