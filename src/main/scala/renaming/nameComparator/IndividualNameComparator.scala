package renaming.nameComparator

import renaming.{TargetName, SourceName}

trait IndividualNameComparator extends NameComparator {

  def normalizedScore(name: SourceName, approvedName: TargetName): Double


  override def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[TargetName]): Map[TargetName, Double] = {
    approvedNames.map(x => (x, normalizedScore(name, x))).toMap
  }

}
