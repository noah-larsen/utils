package workDocument.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.Custom

object Statuses extends Enumerated {

  override type T = Status
  sealed abstract case class Status(customName: String) extends SelfNamed(Custom(customName))

  object RegisteredInTheCentralRepository extends Status("Registered in the Central Repository")
  object PendingDataModeler extends Status("Pending Data Modeler")
  object PendingLocalArchitecture extends Status("Pending Local Arq.")
  object PendingGlobalArchitecutre extends Status("Pending Global Arq.")
  object RuledOut extends Status("Ruled out")
  object ExistsInGlobalRepo extends Status("Exists in Global Repo")
  object NotUsed extends Status("Not Used")


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[Statuses.type], classOf[Status])

}