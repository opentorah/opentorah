package org.podval.judaica.metadata

sealed class Language(code: String) extends Named {
  final override def names: Names = Language.toNames(this)

  final override def name: String = code

  final def toSpec: LanguageSpec = LanguageSpec(this)

  def toString(number: Int): String = number.toString
}

object Language extends NamedCompanion {
  override type Key = Language

  override val values: Seq[Language] = Seq(English, Hebrew, Russian)

  case object English extends Language("en")
  case object Russian extends Language("ru")

  case object Hebrew extends Language("he") {
    val MAQAF: Char       = '־'
    val PASEQ: Char       = '׀'
    val SOF_PASUQ: Char   = '׃'

    private val units: List[Char] = "אבגדהוזחט".toList
    private val decades: List[Char] = "יכלמנסעפצ".toList
    private val hundreds: List[Char] = "קרשת".toList

    override def toString(number: Int): String = {
      require(number > 0)
      require(number <= 10000)

      val result = new StringBuilder
      var remainder = number

      if (remainder >= 1000) {
        result.append(units((remainder / 1000) - 1))
        result.append("׳")
        remainder = remainder % 1000
      }

      if (remainder >= 900) {
        result.append("תת")
        remainder = remainder - 800
      }

      if (remainder >= 500) {
        result.append("ת")
        remainder = remainder - 400
      }

      if (remainder >= 100) {
        result.append(hundreds((remainder / 100) - 1))
        remainder = remainder % 100
      }

      if (remainder == 15) result.append("טו") else
      if (remainder == 16) result.append("טז") else {
        if (remainder >= 10) {
          result.append(decades((remainder / 10) - 1))
          remainder = remainder % 10
        }

        if (remainder >= 1) result.append(units(remainder - 1))
      }

      result.toString
    }
  }
}
