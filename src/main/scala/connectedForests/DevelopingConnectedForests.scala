package connectedForests

import connectedForests.ConnectedForests.RelatedNodesKey

class DevelopingConnectedForests[F, N] private (
                                                 connectedForests: ConnectedForests[F, N],
                                                 relatedNodesKeyToFinishedProportion: Map[RelatedNodesKey[F], Double]
                                               ) extends AbstractConnectedForests[F, N] {

  override def children(forestLabel: F, path: Seq[N]): Set[N] = {
    connectedForests.children(forestLabel, path)
  }


  def finishedProportion(fromForestLabel: F, path: Seq[N], toForestLabel: F): Double = {
    relatedNodesKeyToFinishedProportion.getOrElse(relatedNodesKey(fromForestLabel, path, toForestLabel), 0)
  }


  override def forestLabels: Set[F] = {
    connectedForests.forestLabels
  }


  override def paths(forestLabel: F): Set[Seq[N]] = {
    connectedForests.paths(forestLabel)
  }


  override def pathsSubtree(forestLabel: F, path: Seq[N]): Set[Seq[N]] = {
    connectedForests.pathsSubtree(forestLabel, path)
  }


  override def relatedNodes(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Set[Seq[N]] = {
    connectedForests.relatedNodes(fromForestLabel, fromForestPath, toForestLabel)
  }


  override def roots(forestLabel: F): Set[N] = {
    connectedForests.roots(forestLabel)
  }


  def unfinishedSubroots(fromForestLabel: F, toForestLabel: F, minFinishedProportionFinishedNode: Double): Iterable[Seq[N]] = {

    def unfinishedSubroots(subrootPaths: Iterable[Seq[N]] = connectedForests.roots(fromForestLabel).map(Seq(_))): Iterable[Seq[N]] = {
      subrootPaths.flatMap{
        case x if finishedProportion(fromForestLabel, x, toForestLabel) >= minFinishedProportionFinishedNode && connectedForests.children(fromForestLabel, x).isEmpty => Seq()
        case x if finishedProportion(fromForestLabel, x, toForestLabel) >= minFinishedProportionFinishedNode => unfinishedSubroots(connectedForests.children(fromForestLabel, x)
          .map(x :+ _))
        case x => Seq(x)
      }
    }


    unfinishedSubroots()

  }


  def withFinishedProportion(fromForestLabel: F, path: Seq[N], toForestLabel: F, finishedProportion: Double): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests, relatedNodesKeyToFinishedProportion.+((relatedNodesKey(fromForestLabel, path, toForestLabel), finishedProportion)))
  }


  override def withForest(label: F): Self = {
    DevelopingConnectedForests(connectedForests.withForest(label), relatedNodesKeyToFinishedProportion)
  }


  override def withLabel(forestLabel: F, path: Seq[N], label: N): Self = {
    DevelopingConnectedForests(connectedForests.withLabel(forestLabel, path, label), relatedNodesKeyToFinishedProportion)
  }


  override def withPath(forestLabel: F, path: Seq[N]): Self = {
    DevelopingConnectedForests(connectedForests.withPath(forestLabel, path), relatedNodesKeyToFinishedProportion)
  }


  override def withPaths(forestLabel: F, paths: Iterable[Seq[N]]): Self = {
    DevelopingConnectedForests(connectedForests.withPaths(forestLabel, paths), relatedNodesKeyToFinishedProportion)
  }


  override def withRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): Self = {
    DevelopingConnectedForests(connectedForests.withRelationship(forest1Label, forest1Path, forest2Label, forest2Path), relatedNodesKeyToFinishedProportion)
  }


  override def withoutRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): Self = {
    DevelopingConnectedForests(connectedForests.withoutRelationship(forest1Label, forest1Path, forest2Label, forest2Path), relatedNodesKeyToFinishedProportion)
  }


  override def withoutForest(forestLabel: F): Self = {
    DevelopingConnectedForests(connectedForests.withoutForest(forestLabel), relatedNodesKeyToFinishedProportion.filterKeys(x => !Seq(x.fromForestLabel, x.toForestLabel)
      .contains(forestLabel)))
  }


  override def withoutSubtree(forestLabel: F, path: Seq[N]): Self = {
    DevelopingConnectedForests(connectedForests.withoutSubtree(forestLabel, path), relatedNodesKeyToFinishedProportion.filterKeys(x => connectedForests.pathsSubtree(forestLabel,
      path).exists(y => x.fromForestLabel == forestLabel && x.fromForestNodeId == connectedForests.id(forestLabel, y))))
  }


  override protected type Self = DevelopingConnectedForests[F, N]


  private def relatedNodesKey(fromForestLabel: F, path: Seq[N], toForestLabel: F): RelatedNodesKey[F] = {
    RelatedNodesKey(fromForestLabel, connectedForests.id(fromForestLabel, path), toForestLabel)
  }

}

object DevelopingConnectedForests {

  def apply[F, N](
                   connectedForests: ConnectedForests[F, N] = ConnectedForests[F, N](),
                   relatedNodesKeyToFinishedProportion: Map[RelatedNodesKey[F], Double] = Map[RelatedNodesKey[F], Double]()
                 ): DevelopingConnectedForests[F, N] = {
    new DevelopingConnectedForests(connectedForests, relatedNodesKeyToFinishedProportion)
  }

}