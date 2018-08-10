package consoleApplication

import com.typesafe.config.ConfigFactory
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.enumerations.Countries
import dataDictionary.enumerations.Countries.Country
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.Try
import collection.JavaConverters._
import scala.io.Source

case class Configuration(
                          applicationId: String,
                          country: Country,
                          language: Language,
                          intermediateDataDictionaryId: String,
                          lcSourceSystemToDataDictionaryId: Map[String, String],
                          lcSourceSystemToInitialDataDictionaryId: Map[String, String],
                          workDocumentId: String,
                        ) {

}

object Configuration {

  def apply(source: Source): Try[Configuration] = {
    val config = ConfigFactory.parseString(source.mkString)
    Try(Configuration(
      config.getString(ConfigParameters.ApplicationId.name),
      Countries.withName(config.getString(ConfigParameters.Country.name)).get, //todo error handling
      Languages.withName(config.getString(ConfigParameters.Language.name)).get, //todo error handling
      config.getString(ConfigParameters.IntermediateDataDictionaryId.name),
      config.getObject(ConfigParameters.SourceSystemToDataDictionaryId.name).unwrapped().asScala.toMap.asInstanceOf[Map[String, String]].map(x => (x._1.toLowerCase, x._2)),
      config.getObject(ConfigParameters.SourceSystemToInitialDataDictionaryId.name).unwrapped().asScala.toMap.asInstanceOf[Map[String, String]].map(x => (x._1.toLowerCase, x._2)),
      config.getString(ConfigParameters.WorkDocumentId.name)
    ))
  }


  object ConfigParameters extends Enumerated {

    override type T = ConfigParameter
    sealed abstract class ConfigParameter extends SelfNamed

    object ApplicationId extends ConfigParameter
    object Country extends ConfigParameter
    object Language extends ConfigParameter
    object IntermediateDataDictionaryId extends ConfigParameter
    object SourceSystemToDataDictionaryId extends ConfigParameter
    object SourceSystemToInitialDataDictionaryId extends ConfigParameter
    object WorkDocumentId extends ConfigParameter


    override val values = Seq(ApplicationId, Country, Language, IntermediateDataDictionaryId, SourceSystemToDataDictionaryId, SourceSystemToInitialDataDictionaryId, WorkDocumentId)

  }

}
