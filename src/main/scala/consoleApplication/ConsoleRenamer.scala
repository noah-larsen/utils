package consoleApplication

import dataDictionary.FieldEntry
import consoleApplication.ConsoleRenamer.Languages.Language
import renaming.nameComparator.NameComparator
import renaming.{ApprovedName, NameSearch, Renaming, SourceName}
import utils.commands.{Command, Commands, Parameter}
import utils.commands.Commands.CommandException
import utils.enumerated.{Enumerated, EnumeratedType}

import scala.io.StdIn
import scala.util.Try

case class ConsoleRenamer(
                           renaming: Renaming,
                           approvedNames: Seq[ApprovedName],
                           nameComparator: NameComparator,
                           nameSearch: NameSearch,
                           nTopHitsToGetPossiblyPositiveScoresWhenSearching: Int,
                           originalToRenamedNameToNOccurences: Map[String, Map[String, Int]]
                         )(implicit val language: Language) {

  def iterate: ConsoleRenamer = {
    iterate(renaming.unnamedFieldEntries)
  }


  def viewRenamings(): ConsoleRenamer = {
    val header = Seq(" " * (renaming.fieldEntries.length.toString.length + 1), "Source", "Renamed To")
    println(display(renaming.fieldEntries.zipWithIndex.map(x => Seq((x._2 + 1).toString, x._1.sourceField.getOrElse(new String), x._1.physicalNameField.getOrElse(new String))), header))
    val commandInvocation = ViewRenamingsCommands.promptUntilParsed(renaming.fieldEntries)
    commandInvocation.command match {
      case ViewRenamingsCommands.RenameFieldWithIndex => iterate(Seq(commandInvocation.countingNumberNamedCommandN.get))
      case ViewRenamingsCommands.Back => this
    }
  }


  private def iterate(fieldEntriesToRename: Seq[FieldEntry]): ConsoleRenamer = {
    copy(renaming = fieldEntriesToRename.foldLeft((renaming, true)){ (updatedRenaming_continue, unnamedFieldEntry) =>
      val (updatedRenaming, continue) = updatedRenaming_continue
      (unnamedFieldEntry.sourceField, continue) match{
        case (Some(sourceField), true) =>
          def processMatchResults(matchResults: Seq[ApprovedName]): (Renaming, Boolean) = {
            println(displayMatchResults(unnamedFieldEntry, matchResults, reverse = true) + System.lineSeparator())
            val commandInvocation = RenameCommands.promptUntilParsed(matchResults)
            commandInvocation.command match {
              case RenameCommands.RenameToFieldWithIndex => (updatedRenaming.name(sourceField, commandInvocation.countingNumberNamedCommandN.get.name), true)
              case RenameCommands.Search => processMatchResults(withPositiveScoreOrdered(nameSearch.approvedNameToNormalizedScoreFromArgs(commandInvocation.arguments, nTopHitsToGetPossiblyPositiveScoresWhenSearching)))
              case RenameCommands.EnterNameManually => (updatedRenaming.name(sourceField, commandInvocation.arguments.head), continue)
              case RenameCommands.Pass => (updatedRenaming, continue)
              case RenameCommands.GoBackToTableMenu => (updatedRenaming, false)
            }
          }
          val reorderedMatchResults = withMatchesNameRenamedToBeforeAtFront(unnamedFieldEntry, matchResults(unnamedFieldEntry))
          processMatchResults(reorderedMatchResults)
        case (_, false) =>
          (updatedRenaming, continue)
        case (None, true) =>
          println("Field entry contains no source field, skipping.")
          (updatedRenaming, continue)
      }
    }._1)
  }


  private def matchResults(fieldEntry: FieldEntry): Seq[ApprovedName] = {
    withPositiveScoreOrdered(nameComparator.approvedNameToNormalizedScore(SourceName(fieldEntry), approvedNames))
  }


  private def withPositiveScoreOrdered(approvedNameToNormalizedScore: Map[ApprovedName, Double]): Seq[ApprovedName] = {
    approvedNameToNormalizedScore.filter(_._2 > 0).toSeq.sortBy(-_._2).map(_._1)
  }


  private def withMatchesNameRenamedToBeforeAtFront(fieldEntry: FieldEntry, matchResults: Seq[ApprovedName]): Seq[ApprovedName] = {
    Some(matchResults.partition(x => originalToRenamedNameToNOccurences.get(fieldEntry.sourceField.get).exists(_.contains(x.name)))).map(x => x._1 ++ x._2).get
  }


  private def displayMatchResults(fieldEntry: FieldEntry, matchResults: Seq[ApprovedName], reverse: Boolean = false): String = {

    def renamedToBeforeIndicator(matchResult: ApprovedName): String = {
      val renamedToBeforeTrueIndicator = "*"
      if(originalToRenamedNameToNOccurences.get(fieldEntry.sourceField.get).exists(_.contains(matchResult.name))) renamedToBeforeTrueIndicator else new String
    }


    val space = " "
    val defaultLogicalNameHeader = "(logical name)"
    val defaultDescriptionHeader = "(description)"
    display(matchResults.zipWithIndex.map(x => Seq((x._2 + 1).toString, x._1.name, renamedToBeforeIndicator(x._1), x._1.logicalName, x._1.description).zipWithIndex.map(y => if(y._2 != 0) y._1.trim else y._1)), Seq(new String, fieldEntry.sourceField.get,
      space, fieldEntry.logicalNameField.getOrElse(defaultLogicalNameHeader), fieldEntry.simpleFieldDescription.getOrElse(defaultDescriptionHeader)).map(_ + space), reverse)

  }


  private def display(rows: Seq[Seq[String]], header: Seq[String] = Seq(), reverse: Boolean = false): String = {
    val verticalDividerFillCharacter = "-"
    val verticalDividerColumnSeparator = "+"
    val newlineStrings = Seq("\n", "\r\n", "\r")
    val newlineReplacement = " "
    val filteredRowsWithHeaderIfNonEmpty = header.headOption.map(_ => rows.+:(rows.headOption.map(x => header.padTo(x.length, new String)).getOrElse(header))).getOrElse(rows).map(_.map(newlineStrings.foldLeft(_)((x, y) => x.replace(y,
      newlineReplacement))))
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

    override type CommandType = RenameCommand
    sealed abstract class RenameCommand(letterName: Option[Char], parameters: Seq[Parameter] = Seq()) extends Command(letterName, parameters)

    object RenameToFieldWithIndex extends RenameCommand(None)
    object Search extends RenameCommand(Some('s'), Seq(Parameter("terms", isList = true)))
    object EnterNameManually extends RenameCommand(Some('e'), Seq(Parameter("name")))
    object Pass extends RenameCommand(Some('p'))
    object GoBackToTableMenu extends RenameCommand(Some('b'))


    override protected def commands: Seq[CommandType] = Seq(RenameToFieldWithIndex, Search, EnterNameManually, Pass, GoBackToTableMenu)

  }


  private object ViewRenamingsCommands extends Commands {

    override type CommandType = ViewRenamingsCommand
    sealed abstract class ViewRenamingsCommand(letterName: Option[Char]) extends Command(letterName)

    object RenameFieldWithIndex extends ViewRenamingsCommand(None)
    object Back extends ViewRenamingsCommand(Some('b'))


    override protected def commands = Seq(RenameFieldWithIndex, Back)

  }

}

object ConsoleRenamer {

  object Languages extends Enumerated {

    override type T = Language
    sealed abstract class Language extends EnumeratedType

    object English extends Language
    object Spanish extends Language


    override val values = Seq(English, Spanish)

  }

}
