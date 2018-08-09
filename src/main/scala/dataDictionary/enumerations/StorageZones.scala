package dataDictionary.enumerations

import dataDictionary.enumerations.IngestionStages.{IngestionStage, Master, Raw}
import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.Custom

object StorageZones extends Enumerated {

  override type T = StorageZone
  sealed abstract class StorageZone(customName: String) extends SelfNamed(Custom(customName))

  object MasterData extends StorageZone("MASTERDATA")
  object Production extends StorageZone("Production")
  object RawData extends StorageZone("RAWDATA")
  object Sandbox extends StorageZone("SANDBOX")


  def from(ingestionStage: IngestionStage): StorageZone = {
    ingestionStage match {
      case Raw => RawData
      case Master => MasterData
    }
  }


  override val values = Seq(MasterData, Production, RawData, Sandbox)

}