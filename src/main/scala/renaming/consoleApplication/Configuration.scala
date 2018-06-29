package renaming.consoleApplication

import java.io.File

import com.typesafe.config.ConfigFactory
import dataDictionary.ObjectRow.Countries
import dataDictionary.ObjectRow.Countries.Country
import renaming.consoleApplication.Configuration.ConfigParameters._
import renaming.consoleApplication.ConsoleRenamer.Languages

import scala.util.Try
import collection.JavaConverters._

case class Configuration(
                          workDataDictionaryId: String,
                          workDocumentId: String,
                          sourceSystemToDataDictionaryId: Map[String, String],
                          applicationId: String,
                          language: Languages.Value,
                          country: Country
                        ) {

}

object Configuration {

  def apply(pathname: String): Try[Configuration] = {
    val config = ConfigFactory.parseFile(new File(pathname))
    Try(Configuration(
      config.getString(WorkDataDictionaryId.name),
      config.getString(WorkDocumentId.name),
      config.getObject(SourceSystemToDataDictionaryId.name).unwrapped().asScala.toMap.asInstanceOf[Map[String, String]],
      config.getString(ApplicationId.name),
      Languages.withName(config.getString(Language.name)),
      Countries.withName(config.getString(ConfigParameters.Country.name)).get
    ))
  }


  object ConfigParameters {
    sealed class ConfigParameter(val name: String)
    case object WorkDataDictionaryId extends ConfigParameter("workDataDictionaryId")
    case object WorkDocumentId extends ConfigParameter("workDocumentId")
    case object SourceSystemToDataDictionaryId extends ConfigParameter("sourceSystemToDataDictionaryId")
    case object ApplicationId extends ConfigParameter("applicationId")
    case object Language extends ConfigParameter("language")
    case object Country extends ConfigParameter("country")
  }

}
