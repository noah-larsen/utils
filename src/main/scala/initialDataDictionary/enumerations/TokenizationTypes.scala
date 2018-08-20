package initialDataDictionary.enumerations

import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Lowercase
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithUnderscoresBetweenWords
import utils.enumerated.{Enumerated, SelfNamed}

object TokenizationTypes extends Enumerated {

  override type T = TokenizationType
  sealed abstract class TokenizationType extends SelfNamed(ObjectNameWithUnderscoresBetweenWords(Lowercase))

  object Alphanumeric extends TokenizationType
  object AlphanumericExtended extends TokenizationType
  object Cclient extends TokenizationType
  object DateExtended extends TokenizationType
  object Generic extends TokenizationType
  object Mail extends TokenizationType
  object Nif extends TokenizationType
  object Numeric extends TokenizationType
  object NumericExtended extends TokenizationType
  object Pan extends TokenizationType
  object Phone extends TokenizationType


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[TokenizationTypes.type], classOf[TokenizationType])

}
