package consoleApplication

import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{Config, ConfigFactory}
import consoleApplication.Configuration.ConfigParameters
import consoleApplication.Configuration.ConfigParameters.{GeneratedFields, PrimaryDateField}
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.enumerations.Countries
import dataDictionary.enumerations.Countries.Country
import dataDictionary.field.GeneratedField
import utils.RichConfig
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.Try
import collection.convert.ImplicitConversionsToScala._
import scala.io.Source

case class Configuration(
                          applicationId: String,
                          country: Country,
                          generatedFields: Seq[GeneratedField],
                          language: Language,
                          intermediateDataDictionaryId: String,
                          lcSourceSystemToDataDictionaryId: Map[String, String],
                          lcSourceSystemToInitialDataDictionaryId: Map[String, String],
                          primaryDateField: Option[GeneratedField],
                          workDocumentId: String,
                        ) {

}

object Configuration extends RichConfig {

  def apply(source: Source): Try[Configuration] = {
    val config = ConfigFactory.parseString(source.mkString)
    Try(Configuration(
      applicationId = config.getString(ConfigParameters.ApplicationId.name),
      country = Countries.withName(config.getString(ConfigParameters.Country.name)).get, //todo error handling
      generatedFields = config.get(GeneratedFields.name, _.getConfigList).map(x => x.map(GeneratedField(_).get)).getOrElse(Seq()),
      language = Languages.withName(config.getString(ConfigParameters.Language.name)).get, //todo error handling
      intermediateDataDictionaryId = config.getString(ConfigParameters.IntermediateDataDictionaryId.name),
      lcSourceSystemToDataDictionaryId = config.getObject(ConfigParameters.SourceSystemToDataDictionaryId.name).unwrapped().toMap.asInstanceOf[Map[String, String]].map(x => (x._1.toLowerCase, x._2)),
      lcSourceSystemToInitialDataDictionaryId = config.getObject(ConfigParameters.SourceSystemToInitialDataDictionaryId.name).unwrapped().toMap.asInstanceOf[Map[String, String]].map(x => (x._1.toLowerCase, x._2)),
      primaryDateField = config.get(PrimaryDateField.name, _.getConfig).map(GeneratedField(_).get),
      workDocumentId = config.getString(ConfigParameters.WorkDocumentId.name)
    ))
  }


  object ConfigParameters extends Enumerated {

    override type T = ConfigParameter
    sealed abstract class ConfigParameter extends SelfNamed

    object ApplicationId extends ConfigParameter
    object Country extends ConfigParameter
    object GeneratedFields extends ConfigParameter
    object Language extends ConfigParameter
    object IntermediateDataDictionaryId extends ConfigParameter
    object PrimaryDateField extends ConfigParameter
    object SourceSystemToDataDictionaryId extends ConfigParameter
    object SourceSystemToInitialDataDictionaryId extends ConfigParameter
    object WorkDocumentId extends ConfigParameter


    override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConfigParameters.type], classOf[ConfigParameter])

  }

}
