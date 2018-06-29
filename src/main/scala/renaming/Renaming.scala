package renaming

import dataDictionary.{FieldEntriesObject, FieldEntry}

class Renaming(fieldEntries: Seq[FieldEntry]) extends FieldEntriesObject(fieldEntries) {

  def areAllFieldEntriesNamed: Boolean = {
    unnamedFieldEntries.nonEmpty
  }


  def name(sourceField: String, physicalNameField: String): Renaming = {
    Renaming(fieldEntries.map(x => if(x.sourceField.contains(sourceField)) x.copy(physicalNameField = Some(physicalNameField)) else x))
  }


  def unnamedFieldEntries: Seq[FieldEntry] = {
    fieldEntries.filter(_.physicalNameField.isEmpty)
  }

}

object Renaming {

  def apply(fieldEntries: Seq[FieldEntry]): Renaming = {
    new Renaming(fieldEntries)
  }


  def apply(fieldEntriesObject: FieldEntriesObject): Renaming = {
    apply(fieldEntriesObject.fieldEntries)
  }

}
