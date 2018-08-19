package consoleApplication

import java.io.PrintWriter

import connectedForests.{DevelopingConnectedForests, LabeledForest}
import consoleApplication.BrowseCommands.{CreateNewNode, GoTo, GoUp, MarkRelated}
import consoleApplication.IterateCommands._
import consoleApplication.IterateSelectionCommands.{SelectAll, SelectNodes}
import consoleApplication.OtherCommands.InitializeGoogleProductTaxonomy
import consoleApplication.MainCommands._
import consoleApplication.SearchResultCommands.GoToResultNumber
import org.rogach.scallop.{ScallopConf, ScallopOption}
import persistence.{ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat, PathToIdJsonFormat, StringJsonFormat}
import play.api.libs.json.Json
import taxonomies.GoogleProductTaxonomy
import utils.IO
import utils.commands.Parameter.ListParameter

import scala.io.Source
import scala.util.Try


object Driver extends App {

  case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val persistencePathname: ScallopOption[String] = opt[String](required = true)
    val fromForest: ScallopOption[String] = opt[String](required = true)
    val toForest: ScallopOption[String] = opt[String](required = true)
    verify()
  }


  private val conf = Conf(args)
  private val persistencePathname = conf.persistencePathname()
  private val fromForest = conf.fromForest()
  private val toForest = conf.toForest()
  private val jsonFormat = ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat(StringJsonFormat, StringJsonFormat)


  main(DevelopingConnectedForests(jsonFormat.fromJson(Json.parse(Source.fromFile(persistencePathname).mkString))), promptWithLeadingNewline = false)


  private def main(dcfs: DCFS, promptWithLeadingNewline: Boolean = true): Unit = {
    val commandInvocation = MainCommands.promptUntilParsed(leadWithNewline = promptWithLeadingNewline)
    commandInvocation.command match {
      case Iterate => main(iterateSelection(dcfs, finishedProportion(commandInvocation.value(MaxFinishedValue1To5))))
      case Other => main(other(dcfs))
      case Save =>
        new PrintWriter(persistencePathname){write(jsonFormat.toJson(dcfs.connectedForestsAndRelatedNodesToFinishedProportion).toString()); close()}
        main(dcfs)
      case Quit =>
    }
  }


  private def iterateSelection(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double): DCFS = {
    val header = Seq("Id", "Unfinished Subroot")
    val unfinishedSubroots = dcfs.unfinishedSubroots(fromForest, toForest, maxFinishedProportionUnfinishedNode)
    println(IO.display(unfinishedSubroots.zipWithIndex.map(x => Seq((x._2 + 1).toString, display(x._1))), header))
    val commandInvocation = IterateSelectionCommands.promptUntilParsed(unfinishedSubroots)
    commandInvocation.command match {
      case SelectNodes => iterate(dcfs, maxFinishedProportionUnfinishedNode, commandInvocation.oneBasedIndexListCommandSelection)
      case SelectAll => iterate(dcfs, maxFinishedProportionUnfinishedNode)
      case IterateSelectionCommands.Back =>
        dcfs
    }
  }


  private def iterate(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double, selectedSubroots: Option[Seq[Seq[String]]] = None): DCFS = {

    def process(dcfs_continue: (DCFS, Boolean), unfinishedSubroot: Seq[String]): (DCFS, Boolean) = {
      val (dcfs, continue) = (dcfs_continue._1, dcfs_continue._2)
      println(display(unfinishedSubroot))
      val commandInvocation = IterateCommands.promptUntilParsed()
      commandInvocation.command match {
        case Browse => (browse(dcfs, unfinishedSubroot), continue)
        case Search =>

          //todo should be toForest; for development
          val maxNResults = 100
          val keywordsSeparator = " "
          val results = dcfs.resultPathToNormalizedScore(fromForest, commandInvocation.value(Keyword).mkString(keywordsSeparator), maxNResults).toSeq.sortBy(-_._2)
            .map(_._1)
          println(IO.display(results.zipWithIndex.map(x => Seq((x._2 + 1).toString, display(x._1))), reverse = true))
          val searchResultCommandInvocation = SearchResultCommands.promptUntilParsed(results)
          searchResultCommandInvocation.command match {
            case GoToResultNumber =>
              ???
            case SearchResultCommands.Back =>
          }

          process(dcfs_continue, unfinishedSubroot)
        case RelatedNodes =>
          val indent = "\t"
          println(LabeledForest.subPaths(unfinishedSubroot).zip(dcfs.relatedNodesPath(fromForest, unfinishedSubroot, toForest)).filter(_._2.nonEmpty).map(x =>
            display(x._1) + System.lineSeparator() + x._2.map(indent + display(_)).mkString(System.lineSeparator())).mkString(System.lineSeparator()) + System.lineSeparator())
          process(dcfs_continue, unfinishedSubroot)
        case Descendants =>
          println(dcfs.pathsSubtree(fromForest, unfinishedSubroot).-(unfinishedSubroot).filter(_.length - unfinishedSubroot.length <= commandInvocation.value(MaxDepth))
            .toSeq.sortWith((x, y) => x.length < y.length && y.startsWith(x) || x.zip(y).find(z => z._1 != z._2).exists(z => z._1.compare(z._2) < 0)).map(display)
            .mkString(System.lineSeparator()))
          println()
          process(dcfs_continue, unfinishedSubroot)
        case Next => dcfs_continue
        case Finish => (dcfs.withFinishedProportion(fromForest, unfinishedSubroot, toForest, finishedProportion(commandInvocation.value(FinishedValue1To5))), continue)
        case BackToMainMenu => (dcfs, false)
      }
    }


    def browse(dcfs: DCFS, unfinishedSubroot: Seq[String], toForestNode: Option[Seq[String]] = None): DCFS = {

      //todo
//      val childrenOrRoots = toForestNode.map(dcfs.childPaths(toForest, _)).getOrElse(dcfs.rootPaths(toForest)).toSeq
      val childrenOrRoots = toForestNode.map(dcfs.childPaths(fromForest, _)).getOrElse(dcfs.rootPaths(fromForest)).toSeq
      if(childrenOrRoots.nonEmpty) println(IO.display(childrenOrRoots.zipWithIndex.map(x => Seq((x._2 + 1).toString, x._1.last))))
      toForestNode.foreach(x => println(System.lineSeparator() + display(x)))
      val commandInvocation = BrowseCommands.promptUntilParsed(childrenOrRoots)
      commandInvocation.command match {
        case GoTo => browse(dcfs, unfinishedSubroot, commandInvocation.oneBasedIndexCommandSelection)
        case GoUp => browse(dcfs, unfinishedSubroot, toForestNode.collect{case x if x.length > 1 => x.init})
        case MarkRelated => browse(dcfs, unfinishedSubroot, toForestNode) //todo
        case CreateNewNode => browse(dcfs, unfinishedSubroot, toForestNode) //todo
        case BrowseCommands.Back => dcfs
      }


    }


    val unfinishedSubroots = Some(dcfs.unfinishedSubroots(fromForest, toForest, maxFinishedProportionUnfinishedNode)).map(x => selectedSubroots.map(y => x.filter(z => y
      .exists(z.startsWith(_)))).getOrElse(x)).get
    unfinishedSubroots.foldLeft((dcfs, true)){(x, y) => if(x._2) process(x, y) else x} match {
      case x if x._2 && unfinishedSubroots.nonEmpty => iterate(x._1, maxFinishedProportionUnfinishedNode, selectedSubroots)
      case x => x._1
    }

  }


  private def other(dcfs: DCFS): DCFS = {
    val commandInvocation = OtherCommands.promptUntilParsed()
    commandInvocation.command match {
      case InitializeGoogleProductTaxonomy =>
        val forestLabel = commandInvocation.arguments.tail.head
        dcfs.withForest(forestLabel).withPaths(forestLabel, GoogleProductTaxonomy(commandInvocation.arguments.head).labeledForest.paths)
      case OtherCommands.Back => dcfs
    }
  }


  private def finishedProportion(finishedValue: Int): Double = {
    (finishedValue - finishedValues.min).toDouble / (finishedValues.max - finishedValues.min).toDouble
  }


  private def display[N](path: Seq[N]): String = {
    val separator = " -> "
    path.mkString(separator)
  }

}
