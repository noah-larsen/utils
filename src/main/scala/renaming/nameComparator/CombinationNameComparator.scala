package renaming.nameComparator
import renaming.{ApprovedName, SourceName}

case class CombinationNameComparator(
                                      nameComparatorToWeight: Map[NameComparator, Double]
                                    ) extends NameComparator {

  override def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[ApprovedName]): Map[ApprovedName, Double] = {
    val sumWeights = nameComparatorToWeight.values.sum
    nameComparatorToWeight.map(x => normalizeToUseEntireRange(x._1.approvedNameToNormalizedScore(name, approvedNames)).mapValues(_ * x._2)).reduce((x, y) => x.map(z => (z._1, z._2 + y(z._1)))).mapValues(_ / sumWeights)
  }


  private def normalizeToUseEntireRange(approvedNameToNormalizedScore: Map[ApprovedName, Double]): Map[ApprovedName, Double] = {
    Some(approvedNameToNormalizedScore.values.max).filter(_ > 0).map(x => approvedNameToNormalizedScore.mapValues(_ * (1 / x))).getOrElse(approvedNameToNormalizedScore)
  }

}
