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
                          initialDataDictionaryId: String,
                          intermediateDataDictionaryId: String,
                          workDocumentId: String,
                          sourceSystemToDataDictionaryId: Map[String, String],
                          applicationId: String,
                          language: Language,
                          country: Country
                        ) {

}

object Configuration {

  def apply(source: Source): Try[Configuration] = {
    val config = ConfigFactory.parseString(source.mkString)
    Try(Configuration(
      config.getString(ConfigParameters.InitialDataDictionaryId.name),
      config.getString(ConfigParameters.IntermediateDataDictionaryId.name),
      config.getString(ConfigParameters.WorkDocumentId.name),
      config.getObject(ConfigParameters.SourceSystemToDataDictionaryId.name).unwrapped().asScala.toMap.asInstanceOf[Map[String, String]],
      config.getString(ConfigParameters.ApplicationId.name),
      Languages.withName(config.getString(ConfigParameters.Language.name)).get, //todo error handling
      Countries.withName(config.getString(ConfigParameters.Country.name)).get //todo error handling
    ))
  }


  object ConfigParameters extends Enumerated {

    override type T = ConfigParameter
    sealed abstract class ConfigParameter extends SelfNamed

    object InitialDataDictionaryId extends ConfigParameter
    object IntermediateDataDictionaryId extends ConfigParameter
    object WorkDocumentId extends ConfigParameter
    object SourceSystemToDataDictionaryId extends ConfigParameter
    object ApplicationId extends ConfigParameter
    object Language extends ConfigParameter
    object Country extends ConfigParameter


    override val values = Seq(InitialDataDictionaryId, IntermediateDataDictionaryId, WorkDocumentId, SourceSystemToDataDictionaryId, ApplicationId, Language, Country)

  }

}
