package consoleApplication

import java.io.File

import com.typesafe.config.ConfigFactory
import dataDictionary.ObjectRow.Countries
import dataDictionary.ObjectRow.Countries.Country
import consoleApplication.Configuration.ConfigParameters._
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language

import scala.util.Try
import collection.JavaConverters._
import scala.io.Source

case class Configuration(
                          workDataDictionaryId: String,
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
      config.getString(WorkDataDictionaryId.name),
      config.getString(WorkDocumentId.name),
      config.getObject(SourceSystemToDataDictionaryId.name).unwrapped().asScala.toMap.asInstanceOf[Map[String, String]],
      config.getString(ApplicationId.name),
      Languages.withName(config.getString(ConfigParameters.Language.name)).get, //todo error handling
      Countries.withName(config.getString(ConfigParameters.Country.name)).get //todo error handling
    ))
  }


  object ConfigParameters {
    sealed abstract class ConfigParameter(val name: String)
    case object WorkDataDictionaryId extends ConfigParameter("workDataDictionaryId")
    case object WorkDocumentId extends ConfigParameter("workDocumentId")
    case object SourceSystemToDataDictionaryId extends ConfigParameter("sourceSystemToDataDictionaryId")
    case object ApplicationId extends ConfigParameter("applicationId")
    case object Language extends ConfigParameter("language")
    case object Country extends ConfigParameter("country")
  }

}
