package renaming

import dataDictionary.field.{FieldEntriesObject, FieldEntry}

class Renaming(fieldEntries: Seq[FieldEntry]) extends FieldEntriesObject(fieldEntries) {

  def areAllFieldEntriesNamed: Boolean = {
    unnamedFieldEntries.nonEmpty
  }


  def insert(fieldEntry: FieldEntry, index: Int): Renaming = {
    fieldEntries.splitAt(index) match {case (x, y) => Renaming(x ++ Seq(fieldEntry) ++ y)}
  }


  def name(sourceField: String, physicalNameField: String): Renaming = {
    Renaming(fieldEntries.map(x => if(x.sourceField.contains(sourceField)) x.copy(physicalNameField = Some(physicalNameField)) else x))
  }


  def moveToFront(physicalNameField: String): Renaming = {
    fieldEntries.find(_.physicalNameField.exists(_.equalsIgnoreCase(physicalNameField))).map(x => Renaming(fieldEntries.filter(_ != x).+:(x))).getOrElse(this)
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
