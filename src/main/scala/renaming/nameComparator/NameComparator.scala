package renaming.nameComparator

import renaming.{TargetName, SourceName}

trait NameComparator {

  def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[TargetName]): Map[TargetName, Double]

}
