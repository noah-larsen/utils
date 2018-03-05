import connectedForests.LabeledForest
import org.scalatest.FunSpec

class LabeledForestSpec extends FunSpec {

  describe("LabeledForest"){

    val emptyLF = LabeledForest[Int]()
    val pathsToLeaves = Set(Seq(0), Seq(1, 2), Seq(3, 4, 5), Seq(3, 4, 6), Seq(3, 7))
    val labeledForest = LabeledForest(pathsToLeaves)
    val pathTo__parentLabel_childrenLabels = Map(
      Seq(0) -> (None, Set()),
      Seq(1) -> (None, Set(2)),
      Seq(1, 2) -> (Some(1), Set()),
      Seq(3) -> (None, Set(4, 7)),
      Seq(3, 4) -> (Some(3), Set(5, 6)),
      Seq(3, 4, 5) -> (Some(4), Set()),
      Seq(3, 4, 6) -> (Some(4), Set()),
      Seq(3, 7) -> (Some(3), Set()),
    )
    val paths = pathTo__parentLabel_childrenLabels.keys
    val rootLabels = Set(0, 1, 3)
    val validNonExistentPaths = Seq(Seq(8), Seq(0, 0), Seq(0, 1), Seq(2), Seq(1, 1), Seq(2, 1), Seq(2, 2), Seq(1, 2, 1), Seq(1, 2, 2), Seq(1, 2, 0))
    val nonExistentPaths = validNonExistentPaths.++(Seq())
    val nonExistentLabel = -1


    describe("children"){
      it("should return the node's children if the node exists, and None otherwise"){
        pathTo__parentLabel_childrenLabels.foreach(x => assert(labeledForest.children(x._1).contains(x._2._2)))
        nonExistentPaths.foreach(x => assert(labeledForest.children(x).isEmpty))
      }
    }


    describe("id"){
      it("should return an id uniquely identifying the node if the node exists, and None otherwise"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          assert(labeledForest.path(labeledForest.id(path).get).get == path)
        }
        nonExistentPaths.foreach(x => assert(labeledForest.id(x).isEmpty))
      }
    }


    describe("path"){
      it("should return the path of a node given a uniquely identifying id of a node if the id is valid, and None otherwise"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          assert(labeledForest.path(labeledForest.id(path).get).get == path)
        }
        val invalidId = pathTo__parentLabel_childrenLabels.keySet.map(x => labeledForest.id(x).get).max + 1
        nonExistentPaths.foreach(x => assert(labeledForest.path(invalidId).isEmpty))
      }
    }


    describe("paths"){
      it("should return the paths of all nodes in the forest"){
        assert(labeledForest.paths == pathTo__parentLabel_childrenLabels.keys)
      }
    }


    describe("roots"){
      it("should return the root nodes' labels"){
        assert(labeledForest.roots == rootLabels)
      }
    }


    describe("withLabel"){
      it("should relabel a node if both the node exists and a sibling with the label does not exist"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          val relabeledLF = labeledForest.withLabel(path, nonExistentLabel)
          assert(relabeledLF.get.children(path.slice(0, path.length - 1).:+(nonExistentLabel)).isDefined)
          assert(relabeledLF.get.children(path).isEmpty)
        }
      }
      it("should transfer a node's children to the sibling and remove the node if both the node and a sibling with the label exist"){
        pathTo__parentLabel_childrenLabels.foreach { path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          pathTo__parentLabel_childrenLabels.keys.find(x => x != path && x.slice(0, x.length - 1) == path.slice(0, path.length - 1)).foreach { siblingPath =>
            val relabeledLF = labeledForest.withLabel(path, siblingPath.last)
            val childrenLabelsOriginalPath = pathTo__parentLabel_childrenLabels(path)._2
            val childrenLabelsOriginialSiblingPath = pathTo__parentLabel_childrenLabels(siblingPath)._2
            assert(relabeledLF.get.children(siblingPath).contains(childrenLabelsOriginialSiblingPath ++ childrenLabelsOriginalPath))
            assert(relabeledLF.get.children(path).isEmpty)
          }
        }
      }
      it("should return None if the node does not exist"){
        nonExistentPaths.foreach(x => assert(labeledForest.withLabel(x, nonExistentLabel).isEmpty))
      }
    }


    describe("withPath"){
      it("should return a copy of the forest with the path and its subpaths"){
        paths.foreach { path =>
          val newLfPaths = emptyLF.withPath(path).paths
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == subpaths.toSet)
        }
        nonExistentPaths.foreach { path =>
          val newLfPaths = labeledForest.withPath(path).paths
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == labeledForest.paths.++(subpaths.toSet))
        }
        paths.foreach(x => assert(labeledForest.withPath(x).paths == labeledForest.paths))
      }
    }


    describe("withPaths"){
      it("should return a copy of the forest with the paths and their subpaths"){
        assert(emptyLF.withPaths(pathsToLeaves).paths == labeledForest.paths)
        assert(emptyLF.withPaths(paths).paths == labeledForest.paths)
        assert(labeledForest.withPaths(pathsToLeaves).paths == labeledForest.paths)
        assert(labeledForest.withPaths(paths).paths == labeledForest.paths)
      }
    }


    describe("withoutSubtree"){
      it("should return a copy of the forest without the subtree"){
        paths.foreach{path =>
          val pathsSubtree = paths.filter(_.startsWith(path))
          assert(labeledForest.withoutSubtree(path).paths == paths.toSet.--(pathsSubtree))
        }
      }
    }

  }
}
