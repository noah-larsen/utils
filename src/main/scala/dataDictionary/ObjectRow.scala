package dataDictionary

import java.nio.file.Path
import java.time.LocalDate

import dataDictionary.ObjectRow.CoreValues.CoreValue
import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.Frequencies.Frequency
import dataDictionary.ObjectRow.LoadingTypes.LoadingType
import dataDictionary.ObjectRow.StorageTypes.StorageType
import dataDictionary.ObjectRow.StorageZones.StorageZone
import utils.enumerated.EnumeratedType.NameFormats.CaseFormats.Uppercase
import utils.enumerated.EnumeratedType.NameFormats.{Custom, ObjectName, ObjectNameWithSpacesBetweenWords}
import utils.enumerated.{Enumerated, EnumeratedType}

case class ObjectRow(
                      countryTheDataSource: Country,
                      physicalNameObject: String,
                      logicalNameObject: String,
                      descriptionObject: String,
                      informationGroupLevel1: String,
                      informationGroupLevel2: String,
                      informationGroupLevel3: String,
                      core: CoreValue,
                      perimeter: String,
                      informationLevel: String,
                      dataSource: String,
                      technicalResponsible: String,
                      storageType: StorageType,
                      storageZone: StorageZone,
                      objectType: String,
                      physicalPath: Path,
                      schema: String,
                      systemCodeUuaa: String,
                      partitions: Seq[String],
                      frequency: Frequency,
                      timeRequirement: String,
                      loadingType: LoadingType,
                      currentDepth: String,
                      requiredDepth: String,
                      estimatedVolumeRecords: String,
                      sourceOperational: String,
                      sourceSystem: String,
                      physicalNameSourceObject: String,
                      mailboxSourceTable: String,
                      sourcePath: String,
                      schemaPath: Path,
                      sourceFileType: StorageType,
                      sourceFileDelimeter : Option[String],
                      targetFileType: StorageType,
                      targetFileDelimeter: Option[String],
                      tags: Seq[String],
                      registrationDate: LocalDate
                    ) {

}

object ObjectRow {

  object CoreValues extends Enumerated {

    override type T = CoreValue
    sealed abstract class CoreValue extends EnumeratedType(ObjectName(Uppercase))

    object Yes extends CoreValue
    object No extends CoreValue


    override val values = Seq(Yes, No)

  }


  object Countries extends Enumerated {
    
    override type T = Country
    sealed abstract class Country extends EnumeratedType(ObjectNameWithSpacesBetweenWords())
    
    object Argentina extends Country
    object Colombia extends Country
    object Chile extends Country
    object Holding extends Country
    object Mexico extends Country
    object Paraguay extends Country
    object Peru extends Country
    object Spain extends Country
    object Turkey extends Country
    object UnitedStates extends Country
    object Uruguay extends Country
    object Venezuela extends Country
    
    
    override val values = Seq(Argentina, Colombia, Chile, Holding, Mexico, Paraguay, Peru, Spain, Turkey, UnitedStates, Uruguay, Venezuela)
    
  }
  
  
  object Frequencies extends Enumerated {

    override type T = Frequency
    sealed abstract class Frequency extends EnumeratedType(ObjectNameWithSpacesBetweenWords())

    object Annually extends Frequency
    object Bimonthly extends Frequency
    object ByRequest extends Frequency
    object Daily extends Frequency
    object Monthly extends Frequency
    object Punctual extends Frequency
    object Quarterly extends Frequency
    object RealTime extends Frequency
    object Semiannual extends Frequency
    object Weekly extends Frequency


    override val values = Seq(Annually, Bimonthly, ByRequest, Daily, Monthly, Punctual, Quarterly, RealTime, Semiannual, Weekly)

  }


  object LoadingTypes extends Enumerated {

    override type T = LoadingType
    sealed abstract class LoadingType extends EnumeratedType

    object Complete extends LoadingType
    object Incremental extends LoadingType


    override val values = Seq(Complete, Incremental)

  }


  object StorageTypes extends Enumerated {

    override type T = StorageType
    sealed case class StorageType(customName: String) extends EnumeratedType(Custom(customName))

    object HdfsAvro extends StorageType("HDFS-Avro")
    object HdfsParquet extends StorageType("HDFS-Parquet")


    override val values = Seq(HdfsAvro, HdfsParquet)

  }


  object StorageZones extends Enumerated {

    override type T = StorageZone
    sealed abstract class StorageZone(customName: String) extends EnumeratedType(Custom(customName))

    object MasterData extends StorageZone("MASTERDATA")
    object Production extends StorageZone("Production")
    object RawData extends StorageZone("RAWDATA")
    object Sandbox extends StorageZone("SANDBOX")


    override val values = Seq(MasterData, Production, RawData, Sandbox)

  }


  val partitionsSeparator = ";"

}
