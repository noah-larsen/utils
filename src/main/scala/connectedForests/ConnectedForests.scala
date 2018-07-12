package connectedForests

import connectedForests.ConnectedForests.RelatedNodesKey

class ConnectedForests[F, N] private (
                                       private val labelToForest: Map[F, LabeledForest[N]],
                                       private val relatedNodes: Map[RelatedNodesKey[F], Set[Long]]
                                     ) extends AbstractConnectedForests[F, N] {

  override def children(forestLabel: F, path: Seq[N]): Set[N] = {
    labelToForest(forestLabel).children(path)
  }


  override def forestLabels: Set[F] = {
    labelToForest.keySet
  }


  def id(forestLabel: F, path: Seq[N]): Long = {
    labelToForest(forestLabel).id(path)
  }


  override def paths(forestLabel: F): Set[Seq[N]] = {
    labelToForest(forestLabel).paths
  }


  override def pathsSubtree(forestLabel: F, path: Seq[N]): Set[Seq[N]] = {
    labelToForest(forestLabel).pathsSubtree(path)
  }


  override def relatedNodes(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Set[Seq[N]] = {
    relatedNodes.get(RelatedNodesKey(fromForestLabel, labelToForest(fromForestLabel).id(fromForestPath), toForestLabel))
      .map(_.map(labelToForest(toForestLabel).path))
      .getOrElse(Set())
  }


  override def roots(forestLabel: F): Set[N] = {
    labelToForest(forestLabel).roots
  }


  override def withForest(label: F): ConnectedForests[F, N] = {
    if(labelToForest.contains(label)){
      this
    }
    else{
      ConnectedForests(
        labelToForest.+(label -> LabeledForest()),
        relatedNodes
      )
    }
  }


  override def withLabel(forestLabel: F, path: Seq[N], label: N): ConnectedForests[F, N] = {
    ConnectedForests[F, N](labelToForest.+(forestLabel -> labelToForest(forestLabel).withLabel(path, label)), relatedNodes)
  }


  override def withPath(forestLabel: F, path: Seq[N]): ConnectedForests[F, N] = {
    ConnectedForests(labelToForest.+(forestLabel -> labelToForest(forestLabel).withPath(path)), relatedNodes)
  }


  override def withPaths(forestLabel: F, paths: Iterable[Seq[N]]): ConnectedForests[F, N] = {
    ConnectedForests(labelToForest.+(forestLabel -> labelToForest(forestLabel).withPaths(paths)), relatedNodes)
  }


  override def withRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): ConnectedForests[F, N] = {
    val forest1NodeId = labelToForest(forest1Label).id(forest1Path)
    val forest2NodeId = labelToForest(forest2Label).id(forest2Path)
    val relatedNodesKey1 = RelatedNodesKey(forest1Label, forest1NodeId, forest2Label)
    val relatedNodesKey2 = RelatedNodesKey(forest2Label, forest2NodeId, forest1Label)
    ConnectedForests(labelToForest,
      relatedNodes +
        (relatedNodesKey1 -> (relatedNodes.getOrElse(relatedNodesKey1, Set()) + forest2NodeId)) +
        (relatedNodesKey2 -> (relatedNodes.getOrElse(relatedNodesKey2, Set()) + forest1NodeId))
    )
  }


  override def withoutRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): ConnectedForests[F, N] = {
    val forest1NodeId = labelToForest(forest1Label).id(forest1Path)
    val forest2NodeId = labelToForest(forest2Label).id(forest2Path)
    val relatedNodesKey1 = RelatedNodesKey(forest1Label, forest1NodeId, forest2Label)
    val relatedNodesKey2 = RelatedNodesKey(forest2Label, forest2NodeId, forest1Label)
    ConnectedForests(labelToForest,
      relatedNodes +
        (relatedNodesKey1 -> (relatedNodes.getOrElse(relatedNodesKey1, Set()) - forest2NodeId)) +
        (relatedNodesKey2 -> (relatedNodes.getOrElse(relatedNodesKey2, Set()) - forest1NodeId))
    )
  }


  override def withoutForest(forestLabel: F): ConnectedForests[F, N] = {
    ConnectedForests(labelToForest - forestLabel, relatedNodes.filterKeys(x => !Seq(x.fromForestLabel, x.toForestLabel).contains(forestLabel)))
  }


  override def withoutSubtree(forestLabel: F, path: Seq[N]): ConnectedForests[F, N] = {

    def withoutRelationshipsNode(relatedNodes: Map[RelatedNodesKey[F], Set[Long]], labelForest: F, nodeId: Long): Map[RelatedNodesKey[F], Set[Long]] = {
      relatedNodes.filterKeys(x => !(x.fromForestLabel == labelForest && x.fromForestNodeId == nodeId)).map{
        case x if x._1.toForestLabel == labelForest => (x._1, x._2 - nodeId)
        case x => x
      }
    }


    ConnectedForests(
      labelToForest.+(forestLabel -> labelToForest(forestLabel).withoutSubtree(path)),
      labelToForest(forestLabel).idsSubtree(path).foldLeft(relatedNodes)((y, z) => withoutRelationshipsNode(y, forestLabel, z))
    )

  }


  override protected type Self = ConnectedForests[F, N]

}

object ConnectedForests {

  def apply[F, N](
                   labelToForest: Map[F, LabeledForest[N]] = Map[F, LabeledForest[N]](),
                   relatedNodes: Map[RelatedNodesKey[F], Set[Long]] = Map[RelatedNodesKey[F], Set[Long]]()
                 ): ConnectedForests[F, N] = {
    new ConnectedForests(labelToForest, relatedNodes)
  }


  private[connectedForests] case class RelatedNodesKey[F](fromForestLabel: F, fromForestNodeId: Long, toForestLabel: F)

}
