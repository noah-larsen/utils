package initialDataDictionary

import initialDataDictionary.field.Field
import initialDataDictionary.`object`.Object_
import initialDataDictionary.sourceSystem.SourceSystem

case class ObjectAndFields(
                            obj: Object_,
                            fields: Seq[Field],
                            sourceSystem: SourceSystem
                          ) {

}
