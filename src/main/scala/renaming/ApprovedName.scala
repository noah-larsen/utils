package renaming

import renaming.consoleApplication.ConsoleRenamer.Languages

trait ApprovedName {

  def name: String
  def logicalName(implicit language: Languages.Value): String
  def description(implicit language: Languages.Value): String

}
