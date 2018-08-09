package connectedForests

import connectedForests.ConnectedForestsSpec.ConnectedForestsTestData
import connectedForests.DevelopingConnectedForestsSpec.DevelopingConnectedForestsTestData
import org.scalatest.FunSpec

class DevelopingConnectedForestsSpec extends FunSpec with DevelopingConnectedForestsTestData {

  describe("DevelopingConnectedForests") {

    describe("finishedProportion") {
      //todo
    }


    describe("unfinishedSubroots") {

      it("should return an empty iterable if all nodes are finished") {
        val dcfAllNodesF1ToF2MinimallyFinished = dcf.paths(forest1Label).foldLeft(dcf)((x, y) => x.withFinishedProportion(forest1Label, y, forest2Label, Double
          .MinPositiveValue))
        assert(dcfAllNodesF1ToF2MinimallyFinished.unfinishedSubroots(forest1Label, forest2Label, 0).isEmpty)
      }


      it("should return the root paths of fromForest if all nodes are unfinished") {
        assert(dcf.unfinishedSubroots(forest1Label, forest2Label, 1).toSet == dcf.roots(forest1Label).map(Seq(_)))
      }

    }


    describe("withFinishedProportion") {
      //todo
    }

  }

}

object DevelopingConnectedForestsSpec {

  trait DevelopingConnectedForestsTestData extends ConnectedForestsTestData {

    val dcf = DevelopingConnectedForests(connectedForests)

  }

}
