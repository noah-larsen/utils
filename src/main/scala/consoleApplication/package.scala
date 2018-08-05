import connectedForests.DevelopingConnectedForests

package object consoleApplication {
  type DCFS = DevelopingConnectedForests[String, String]
  val finishedValues: Range = 1 to 5
}
