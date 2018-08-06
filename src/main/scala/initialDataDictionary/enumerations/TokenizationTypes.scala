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


  override val values = Seq(Alphanumeric, AlphanumericExtended, Cclient, DateExtended, Generic, Mail, Nif, Numeric, NumericExtended, Pan, Phone)

}
