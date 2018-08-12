package consoleApplication

import java.io.PrintWriter

import connectedForests.{DevelopingConnectedForests, LabeledForest}
import consoleApplication.IterateCommands._
import consoleApplication.IterateSelectionCommands.{All, Select, UnfinishedSubrootId}
import consoleApplication.OtherCommands.{Back, InitializeGoogleProductTaxonomy}
import consoleApplication.MainCommands._
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


  main(Try(Source.fromFile(persistencePathname)).map(x => DevelopingConnectedForests(jsonFormat.fromJson(Json.parse(x.mkString)))).getOrElse(DevelopingConnectedForests[String,
    String]()), promptWithLeadingNewline = false)


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
    val listParameterToRuntimeValidationF = Map[ListParameter[_], Seq[String] => Try[Unit]](UnfinishedSubrootId -> ((x: Seq[String]) => Try().filter(_ => x.map(y => Try(y
      .toInt).filter(unfinishedSubroots.indices.map(_ + 1).contains)).forall(_.isSuccess))))
    val commandInvocation = IterateSelectionCommands.promptUntilParsed(listParameterToRuntimeValidationF = listParameterToRuntimeValidationF)
    commandInvocation.command match {
      case All => iterate(dcfs, maxFinishedProportionUnfinishedNode)
      case Select => iterate(dcfs, maxFinishedProportionUnfinishedNode, Some(unfinishedSubroots.zipWithIndex.collect{case (x, y) if commandInvocation.value(UnfinishedSubrootId)
          .contains(y + 1) => x}))
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
        case MakeNewRelatedNode =>
          process((dcfs.withPath(toForest, commandInvocation.arguments).withRelationship(fromForest, unfinishedSubroot, toForest, commandInvocation.arguments), continue),
            unfinishedSubroot)
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
      case Back => dcfs
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
