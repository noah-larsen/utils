package connectedForests

import connectedForests.LabeledForest.LabeledTreeNode

class LabeledForest[N] private(
                                private val idToNode: Map[Long, LabeledTreeNode[N]],
                                private val labelToRootId: Map[N, Long]
                              ) {

  def this(){
    this(Map(), Map())
  }


  def children(path: Seq[N]): Option[Set[N]] = {
    id(path).map(idToNode(_).labelToChildId.keySet)
  }


  def id(path: Seq[N]): Option[Long] = {
    path.headOption.map(x => path.tail.foldLeft(labelToRootId.get(x))((y, z) => y.map(idToNode(_).labelToChildId.get(z)).getOrElse(None))).getOrElse(None)
  }


  def path(id: Long): Option[Seq[N]] = {
    idToNode.get(id).map(x => x.parentId.map(y => path(y).get.:+(x.label)).getOrElse(Seq(x.label)))
  }


  def paths: Set[Seq[N]] = {
    idToNode.keys.map(path(_).get).toSet
  }


  def roots: Set[N] = {
    labelToRootId.keySet
  }


  def withLabel(path: Seq[N], label: N): Option[LabeledForest[N]] = {

    def copyChildren(idToNode: Map[Long, LabeledTreeNode[N]], from: Long, to: Long): Map[Long, LabeledTreeNode[N]] = {
      idToNode.+(to -> idToNode(to).addChildren(idToNode(from).labelToChildId))
    }


    def relabelIdToNode(idToNode: Map[Long, LabeledTreeNode[N]], id: Long, label: N): Map[Long, LabeledTreeNode[N]] = {
      val withParentUpdated = idToNode(id).parentId
        .map(x => idToNode.+(x -> idToNode(x).removeChild(idToNode(id).label).addChild(label, id)))
        .getOrElse(idToNode)
      withParentUpdated.+(id -> idToNode(id).withLabel(label))
    }


    def relabelLabelToRootId(labelToRootId: Map[N, Long], id: Long, label: N): Map[N, Long] = {
      labelToRootId.find(_._2 == id)
        .map(x => labelToRootId.-(x._1).+(label -> id))
        .getOrElse(labelToRootId)
    }


    def siblingId(path: Seq[N], label: N): Option[Long] = {
      id(path.slice(0, path.length - 1).:+(label))
    }

    id(path).map(idNodeToRelabel =>
      siblingId(path, label)
        .map(x => new LabeledForest[N](
          LabeledForest.withoutNode(copyChildren(idToNode, idNodeToRelabel, x), idNodeToRelabel),
          LabeledForest.withoutNode(labelToRootId, idToNode(idNodeToRelabel))
        ))
        .getOrElse(new LabeledForest[N](relabelIdToNode(idToNode, idNodeToRelabel, label), relabelLabelToRootId(labelToRootId, idNodeToRelabel, label))))

  }


  def withPath(path: Seq[N]): LabeledForest[N] = {
    def subpaths(path: Seq[N]): Seq[Seq[N]] = {
      path.zipWithIndex.map(x => path.take(x._2 + 1))
    }
    subpaths(path).foldLeft((this, None: Option[Long])){(labeledForest_parentId, subPath) =>
      val labeledForest = labeledForest_parentId._1
      val parentId = labeledForest_parentId._2
      val id = parentId.map(labeledForest.idToNode(_).labelToChildId).getOrElse(labeledForest.labelToRootId).get(subPath.last)
      val labeledForestWithSubpath = id.map(_ => labeledForest).getOrElse(
        new LabeledForest[N](
          parentId.map(x => labeledForest.idToNode.+(x -> labeledForest.idToNode(x).addChild(subPath.last, labeledForest.nextId))).getOrElse(labeledForest.idToNode)
            .+(labeledForest.nextId -> LabeledTreeNode(subPath.last, parentId, Map())),
          parentId.map(_ => labeledForest.labelToRootId).getOrElse(labeledForest.labelToRootId.+(subPath.last -> id.getOrElse(labeledForest.nextId)))
        )
      )
      (labeledForestWithSubpath, id.orElse(Some(labeledForest.nextId)))
    }._1
  }


  def withPaths(paths: Traversable[Seq[N]]): LabeledForest[N] = {
    paths.foldLeft(this)((x, y) => x.withPath(y))
  }


  def withoutSubtree(path: Seq[N]): LabeledForest[N] = {
    def idsSubtree(idToNode: Map[Long, LabeledTreeNode[N]], rootId: Long): Set[Long] = {
      idToNode.get(rootId).map(_.labelToChildId.values.foldLeft(Set(rootId))((x, y) => x.++(idsSubtree(idToNode, y)))).getOrElse(Set())
    }
    id(path).map { rootId =>
      new LabeledForest[N](
        LabeledForest.withoutNode(idToNode, rootId).--(idsSubtree(idToNode, rootId)),
        LabeledForest.withoutNode(labelToRootId, idToNode(rootId))
      )
    }.getOrElse(this)
  }


  private def nextId: Long = {
    Some(idToNode.keys).filter(_.nonEmpty)
      .map(_.max + 1)
      .getOrElse(0)
  }

}

object LabeledForest {

  def apply[N](paths: Set[Seq[N]] = Set[Seq[N]]()): LabeledForest[N] = {
    new LabeledForest().withPaths(paths)
  }


  private def withoutNode[N](idToNode: Map[Long, LabeledTreeNode[N]], id: Long): Map[Long, LabeledTreeNode[N]] = {
    idToNode(id).parentId.map(x => idToNode.+(x -> idToNode(x).removeChild(idToNode(id).label))).getOrElse(idToNode).-(id)
  }


  private def withoutNode[N](labelToRootId: Map[N, Long], node: LabeledTreeNode[N]): Map[N, Long] = {
    node.parentId.map(_ => labelToRootId).getOrElse(labelToRootId.-(node.label))
  }


  private case class LabeledTreeNode[N](
                                         label: N,
                                         parentId: Option[Long],
                                         labelToChildId: Map[N, Long]
                                       ){

    def addChild(label: N, id: Long): LabeledTreeNode[N] = {
      LabeledTreeNode(this.label, parentId, labelToChildId.+(label -> id))
    }


    def addChildren(labelToChildId: Map[N, Long]): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, this.labelToChildId.++(labelToChildId))
    }


    def removeChild(label: N): LabeledTreeNode[N] = {
      LabeledTreeNode(this.label, parentId, labelToChildId.-(label))
    }


    def removeChildren(labels: Set[N]): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, labelToChildId.--(labels))
    }


    def withLabel(label: N): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, labelToChildId)
    }

  }

}




