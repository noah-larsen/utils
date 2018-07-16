package renaming.nameComparator

import renaming.{ApprovedName, SourceName}

trait IndividualNameComparator extends NameComparator {

  def normalizedScore(name: SourceName, approvedName: ApprovedName): Double


  override def approvedNameToNormalizedScore(name: SourceName, approvedNames: Iterable[ApprovedName]): Map[ApprovedName, Double] = {
    approvedNames.map(x => (x, normalizedScore(name, x))).toMap
  }

}
