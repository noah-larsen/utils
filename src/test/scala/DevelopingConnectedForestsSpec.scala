import ConnectedForestsSpec.ConnectedForestsTestData
import connectedForests.DevelopingConnectedForests
import org.scalatest.FunSpec

class DevelopingConnectedForestsSpec extends FunSpec with ConnectedForestsTestData {

  describe("DevelopingConnectedForests") {

    val developingConnectedForests = DevelopingConnectedForests(connectedForests)


    describe("finishedProportion") {
      //todo
    }


    describe("unfinishedSubroots") {

      it("should return an empty iterable if all nodes are finished") {
        assert(developingConnectedForests.unfinishedSubroots(forest1Label, forest2Label, 0).isEmpty)
      }


      it("should return the root paths of fromForest if all nodes are unfinished") {
        assert(developingConnectedForests.unfinishedSubroots(forest1Label, forest2Label, 1) == developingConnectedForests.roots(forest1Label).map(Seq(_)))
      }

    }


    describe("withFinishedProportion") {
      //todo
    }

  }

}
