package dataDictionary

import java.nio.file.Path
import java.time.LocalDate

import dataDictionary.ObjectRow.CoreValues.CoreValue
import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.Frequencies.Frequency
import dataDictionary.ObjectRow.LoadingTypes.LoadingType
import dataDictionary.ObjectRow.StorageTypes.StorageType
import dataDictionary.ObjectRow.StorageZones.StorageZone
import dataDictionary.ObjectRow._
import utils.Enumerated
import utils.Enumerated.EnumeratedType

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
    sealed case class CoreValue(name: String) extends EnumeratedType

    object Yes extends CoreValue("YES")
    object No extends CoreValue("NO")


    override val values = Seq(Yes, No)

  }


  object Countries extends Enumerated {
    
    override type T = Country
    sealed case class Country(name: String) extends EnumeratedType
    
    object Argentina extends Country("Argentina")
    object Colombia extends Country("Colombia")
    object Chile extends Country("Chile")
    object Holding extends Country("Holding")
    object Mexico extends Country("Mexico")
    object Paraguay extends Country("Paraguay")
    object Peru extends Country("Peru")
    object Spain extends Country("Spain")
    object Turkey extends Country("Turkey")
    object UnitedStates extends Country("United States")
    object Uruguay extends Country("Uruguay")
    object Venezuela extends Country("Venezuela")
    
    
    override val values = Seq(Argentina, Colombia, Chile, Holding, Mexico, Paraguay, Peru, Spain, Turkey, UnitedStates, Uruguay, Venezuela)
    
  }
  
  
  object Frequencies extends Enumerated {

    override type T = Frequency
    sealed case class Frequency(name: String) extends EnumeratedType

    object Annually extends Frequency("Annually")
    object BiMonthly extends Frequency("Bimonthly")
    object ByRequest extends Frequency("By Request")
    object Daily extends Frequency("Daily")
    object Monthly extends Frequency("Monthly")
    object Punctual extends Frequency("Punctual")
    object Quarterly extends Frequency("Quarterly")
    object RealTime extends Frequency("Real Time")
    object SemiAnnual extends Frequency("Semiannual")
    object Weekly extends Frequency("Weekly")


    override val values = Seq(Annually, BiMonthly, ByRequest, Daily, Monthly, Punctual, Quarterly, RealTime, SemiAnnual, Weekly)

  }


  object LoadingTypes extends Enumerated {

    override type T = LoadingType
    sealed case class LoadingType(name: String) extends EnumeratedType

    object Complete extends LoadingType("Complete")
    object Incremental extends LoadingType("Incremental")


    override val values = Seq(Complete, Incremental)

  }


  object StorageTypes extends Enumerated {

    override type T = StorageType
    sealed case class StorageType(name: String) extends EnumeratedType

    object HdfsAvro extends StorageType("HDFS-Avro")
    object HdfsParquet extends StorageType("HDFS-Parquet")


    override val values = Seq(HdfsAvro, HdfsParquet)

  }


  object StorageZones extends Enumerated {

    override type T = StorageZone
    sealed case class StorageZone(name: String) extends EnumeratedType

    object MasterData extends StorageZone("MASTERDATA")
    object Production extends StorageZone("Production")
    object RawData extends StorageZone("RAWDATA")
    object Sandbox extends StorageZone("SANDBOX")


    override val values = Seq(MasterData, Production, RawData, Sandbox)

  }


  val partitionsSeparator = ";"

}
