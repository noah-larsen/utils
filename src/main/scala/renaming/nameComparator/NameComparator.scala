package renaming.nameComparator

import renaming.{ApprovedName, SourceName}

trait NameComparator {

  def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[ApprovedName]): Map[ApprovedName, Double]

}
