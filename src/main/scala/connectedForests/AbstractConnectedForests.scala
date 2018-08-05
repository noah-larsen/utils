package connectedForests

trait AbstractConnectedForests[F, N] {

  protected type Self <: AbstractConnectedForests[F, N]


  def children(forestLabel: F, path: Seq[N]): Set[N]


  def forestLabels: Set[F]


  def paths(forestLabel: F): Set[Seq[N]]


  def pathsSubtree(forestLabel: F, path: Seq[N]): Set[Seq[N]]


  def relatedNodes(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Set[Seq[N]]


  def relatedNodesOfPath(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Seq[Set[Seq[N]]]


  def roots(forestLabel: F): Set[N]


  def withForest(label: F): Self


  def withLabel(forestLabel: F, path: Seq[N], label: N): Self


  def withPath(forestLabel: F, path: Seq[N]): Self


  def withPaths(forestLabel: F, paths: Iterable[Seq[N]]): Self


  def withRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): Self


  def withoutRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): Self


  def withoutForest(forestLabel: F): Self


  def withoutSubtree(forestLabel: F, path: Seq[N]): Self


  def subPaths(path: Seq[N]): Seq[Seq[N]] = {
    path.inits.toSeq.reverse.tail
  }

}
