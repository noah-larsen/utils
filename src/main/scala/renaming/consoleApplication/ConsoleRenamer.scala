package renaming.consoleApplication

import dataDictionary.FieldEntry
import renaming.consoleApplication.ConsoleRenamer.Languages
import renaming.{ApprovedName, NameComparator, Renaming}
import utils.Commands.CommandException
import utils.{AbstractCommand, Commands}

import scala.io.StdIn
import scala.util.Try

case class ConsoleRenamer(
                           renaming: Renaming,
                           approvedNames: Seq[ApprovedName],
                           nameComparator: NameComparator
                         )(implicit val language: Languages.Value) {

  def iterate: ConsoleRenamer = {
    copy(renaming = renaming.unnamedFieldEntries.foldLeft((renaming, true)){ (updatedRenaming_continue, unnamedFieldEntry) =>
      val (updatedRenaming, continue) = updatedRenaming_continue
      (unnamedFieldEntry.sourceField, continue) match{
        case (Some(sourceField), true) =>
          val matchResults = nameMatchingResults(unnamedFieldEntry)
          println(displayMatchResults(unnamedFieldEntry, matchResults, reverse = true) + System.lineSeparator())
          println(RenameCommands.usage)
          def process: (Renaming, Boolean) = {
            val input = StdIn.readLine()
            input match {
              case x if Try(x.toInt).filter(matchResults.indices.map(_ + 1).contains).isSuccess =>
                (updatedRenaming.name(sourceField, matchResults(x.toInt - 1).name), true)
              case x => RenameCommands.parse(x).map { commandInvocation =>
                commandInvocation.command match {
                  case RenameCommands.EnterNameManually => (updatedRenaming.name(sourceField, commandInvocation.arguments.head), continue)
                  case RenameCommands.Skip => (updatedRenaming, continue)
                  case RenameCommands.GoBackToDataMenu => (updatedRenaming, false)
                }
              }.recover{case e: CommandException =>
                println(e.message)
                process
              }.get
            }
          }
          process
        case (_, false) =>
          (updatedRenaming, continue)
        case (None, true) =>
          println("Field entry contains no source field, skipping.")
          (updatedRenaming, continue)
      }
    }._1)
  }


  def viewRenamings(): Unit = {
    val header = Seq("Source", "Renamed To")
    println(display(renaming.fieldEntries.map(x => Seq(x.sourceField.getOrElse(new String), x.physicalNameField.getOrElse(new String))), header))
  }


  private def nameMatchingResults(fieldEntry: FieldEntry): Seq[ApprovedName] = {
    approvedNames.map(x => (x, nameComparator.nameDistance(fieldEntry.sourceField.get, x.name))).sortBy(_._2).map(_._1)
  }


  private def displayMatchResults(fieldEntry: FieldEntry, matchResults: Seq[ApprovedName], reverse: Boolean = false): String = {
    val space = " "
    val defaultLogicalNameHeader = "(logical name)"
    val defaultDescriptionHeader = "(description)"
    display(matchResults.zipWithIndex.map(x => Seq((x._2 + 1).toString + space, x._1.name, x._1.logicalName, x._1.description).zipWithIndex.map(y => if(y._2 != 0) y._1.trim else y._1)), Seq(new String, fieldEntry.sourceField.get, fieldEntry.logicalNameField
      .getOrElse(defaultLogicalNameHeader), fieldEntry.simpleFieldDescription.getOrElse(defaultDescriptionHeader)).map(_ + space), reverse)
  }


  private def display(rows: Seq[Seq[String]], header: Seq[String] = Seq(), reverse: Boolean = false): String = {
    val verticalDividerFillCharacter = "-"
    val verticalDividerColumnSeparator = "+"
    val newlineStrings = Seq("\n", "\r\n", "\r")
    val newlineReplacement = " "
    val filteredRowsWithHeaderIfNonEmpty = header.headOption.map(_ => rows.+:(rows.headOption.map(x => header.padTo(x.length, new String)).getOrElse(header))).getOrElse(rows).map(_.map(newlineStrings.foldLeft(_)((x, y) => x.replace(y, newlineReplacement))))
    val maxLengths = filteredRowsWithHeaderIfNonEmpty.map(_.map(_.length)).reduce((x, y) => x.zip(y).map(z => Math.max(z._1, z._2)))
    val verticalDivider = maxLengths.map(verticalDividerFillCharacter * _).mkString(verticalDividerColumnSeparator, verticalDividerColumnSeparator, verticalDividerColumnSeparator)
    def displayRow(row: Seq[String], maxLengths: Seq[Int], columnSeparator: String = "|"): String = {
      val space = " "
      row.zip(maxLengths).map(x => x._1 + space * (x._2 - x._1.length)).mkString(columnSeparator, columnSeparator, columnSeparator)
    }
    Some(header.headOption.map(_ => Seq(verticalDivider, displayRow(filteredRowsWithHeaderIfNonEmpty.head, maxLengths), verticalDivider)).getOrElse(Seq(verticalDivider)).++(filteredRowsWithHeaderIfNonEmpty.tail.map(displayRow(_, maxLengths)))
      .:+(verticalDivider)).map(x => if(reverse) x.reverse else x).get.mkString(System.lineSeparator())
  }


  private object RenameCommands extends Commands {

    override type Command = RenameCommand
    sealed abstract class RenameCommand(val name: String, val parameters: Seq[String] = Seq()) extends AbstractCommand

    case object EnterNameManually extends RenameCommand("e", Seq("name"))
    case object Skip extends RenameCommand("s")
    case object GoBackToDataMenu extends RenameCommand("b")


    override protected def commands: Seq[Command] = Seq(EnterNameManually, Skip, GoBackToDataMenu)

  }

}

object ConsoleRenamer {

  object Languages extends Enumeration {val english, spanish = Value}

}
