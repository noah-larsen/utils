package utils.enumerated

import utils.enumerated.Enumerated.EnumeratedType

abstract class SelfNamed(addSpacesBetweenWords: Boolean = false) extends EnumeratedType {

  def name: String = {
    val multipleClassNameAddedDisambiguationSymbol = "$"
    val classNameSeparatorRE = "[.$]"
    val space = " "
    Some(getClass.getName.split(classNameSeparatorRE).filter(_.nonEmpty).last)
      .map(x => if(addSpacesBetweenWords) words(x).mkString(space) else x)
      .get
  }


  private def words(camelCase: String): Seq[String] = {
    Some(camelCase.zipWithIndex.filter(x => x._1.isUpper || x._2 == 0).map(_._2)).map(x => x.zip(x.tail.+:(camelCase.length)).map(y => camelCase.substring(y._1, y._2))).get
  }

}
