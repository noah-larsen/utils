package drivers

import java.io.File
import java.util.Locale
import java.util.concurrent.TimeUnit

import centralNamingsRepository.CentralNamingsRepository
import com.typesafe.config.ConfigFactory
import dataDictionary.DataDictionary
import googleSpreadsheets.{GoogleSpreadsheet, SheetRange}
import info.debatty.java.stringsimilarity.experimental.Sift4
import info.debatty.java.stringsimilarity._
import consoleApplication.Configuration
import consoleApplication.ConsoleRenamer.Languages
import drivers.Scratch.Asdfs.{A, Asdf, B}
import renaming.{ApprovedRenamings, Name}
import us.alnova.AlnovaTableLayouts
import utils.enumerated.{Enumerated, SelfNamed}
import workDocument.WorkDocument

import scala.io.{Source, StdIn}

object Scratch extends App {


  object Asdfs extends Enumerated {

    override type T = Asdf
    sealed abstract class Asdf extends SelfNamed

    case object A extends Asdf
    case object B extends Asdf

    override val values = Seq(A, B)
  }


  println(A == B)

  val c: Asdf = null
  c match {
    case A =>
  }

}
