package us

import consoleApplication.Configuration
import consoleApplication.ConsoleRenamer.Languages.Language
import consoleApplication.Driver.args
import initialDataDictionary.InitialDataDictionary
import org.rogach.scallop.{ScallopConf, ScallopOption}
import us.phoenix.PhoenixOwnerDataDictionary

import scala.io.Source
import scala.util.Try

object USSourceSystems extends App {

  //todo

//  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
//    val test: ScallopOption[Boolean] = toggle(default = Some(false), hidden = true)
//    verify()
//  }
//
//
//  val conf = new Conf(args)
//  System.exit(0)

  implicit val isTest: Boolean = true//conf.test()


  val configPathname = "config.json"
  val testConfigPathname = "testConfig.json"
  val configuration = Configuration(Source.fromResource(if(isTest) testConfigPathname else configPathname)).get


  val lcSourceSystemToInitialDataDictionary = configuration.lcSourceSystemToInitialDataDictionaryId.filter(_._2.trim.nonEmpty).map(x => (x._1.toLowerCase, InitialDataDictionary(x._2).get))
  PhoenixOwnerDataDictionary.updateInitialDataDictionary(lcSourceSystemToInitialDataDictionary)

}
