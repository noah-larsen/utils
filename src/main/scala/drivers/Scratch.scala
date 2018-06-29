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
import renaming.consoleApplication.Configuration
import renaming.consoleApplication.ConsoleRenamer.Languages
import renaming.{ApprovedRenamings, Name, NameComparator}
import us.AlnovaTableLayouts
import workDocument.WorkDocument

import scala.io.{Source, StdIn}

object Scratch extends App {




  val configPathname = "config.json" //todo
  val configuration = Configuration(configPathname).get







//  val config = ConfigFactory.parseFile(new File("config.json"))
//
//
//  println(config)






//  val centralNamingsRepository = CentralNamingsRepository().get
//  val usDataDictionary = UsDataDictionary().get

//  val globalNameSet = CentralNamingsRepository().globalNameSet

//  val fieldEntriesObject = AlnovaTableLayouts.fieldEntries("pedt008")
//  println(fieldEntriesObject)
//  val sourceFields = fieldEntriesObject.fieldEntries.map(_.sourceField.get)

//  val approvedRenamings = ApprovedRenamings(usDataDictionary)

//  println(Renamings.raw(UsDataDictionary().get).normalizedSubstringToMatchSubstringsAndNObservations.mkString(System.lineSeparator()))

//  println(Renamer(CentralNamingsRepository()).possibleTargets.mkString(System.lineSeparator()))

//  NameComparator(approvedRenamings.normalizedSubstringToMatchToNObservations)





//  val dd = DataDictionary("1vZ3hohi5qw-IaV1S1Adtn3a0CFi8D_RBJTSYX_1LxLE").get

//  val fieldEntriesObjectInDictionary = dd.fieldEntriesObject(IngestionStage.raw, "t_wdco_cif_pedt008")
//  println(fieldEntriesObjectInDictionary)
//
//  StdIn.readLine()

//  println(dd.write(IngestionStage.raw, fieldEntriesObject/*.merge(fieldEntriesObject)*/))




//  println(approvedRenamings.normalizedSubstringToMatchToNObservations.toSeq.sortBy(_._1).mkString(System.lineSeparator()))
//  println()

//  val start = System.currentTimeMillis()
//  val approvedRenamingAndSortedDistances = approvedRenamings.originalAndRenamedNames/*.slice(0, 188)*/.map(x => (x, NameComparator(centralNamingsRepository, approvedRenamings.normalizedSubstringToMatchToNObservations).sortedDistances(x._1)))
//
//  println(approvedRenamingAndSortedDistances.map(x => s"${x._1._1}\t${x._1._2}\t${x._2.zipWithIndex.find(_._1._1.equalsIgnoreCase(x._1._2)).map(_._2).getOrElse(-1)}").mkString(System.lineSeparator()))
//  println(approvedRenamingAndSortedDistances.map(x => x._2.zipWithIndex.find(_._1._1.equalsIgnoreCase(x._1._2)).map(_._2).getOrElse(-1)).filter(x => x >= 0 && x < 10).sorted)
////
//  println()
//  println(TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis() - start) + " seconds")


}
