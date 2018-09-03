package org.podval.calendar.generate.tanach

sealed trait Parsha

object Parsha {
  case object Bereishis extends Parsha
  case object Noach extends Parsha
  case object LechLecha extends Parsha
  case object Vayeira extends Parsha
  case object ChayeiSarah extends Parsha
  case object Toldos extends Parsha
  case object Vayeitzei extends Parsha
  case object Vayishlach extends Parsha
  case object Vayeishev extends Parsha
  case object Mikeitz extends Parsha
  case object Vayigash extends Parsha
  case object Vayechi extends Parsha
  case object Shemos extends Parsha
  case object Va_eira extends Parsha
  case object Bo extends Parsha
  case object Beshalach extends Parsha
  case object Yisro extends Parsha
  case object Mishpatim extends Parsha
  case object Terumah extends Parsha
  case object Tetzaveh extends Parsha
  case object KiSisa extends Parsha
  case object Vayakhel extends Parsha
  case object Pekudei extends Parsha
  case object Vayikra extends Parsha
  case object Tzav extends Parsha
  case object Shemini extends Parsha
  case object Tazria extends Parsha
  case object Metzora extends Parsha
  case object Acharei extends Parsha
  case object Kedoshim extends Parsha
  case object Emor extends Parsha
  case object Behar extends Parsha
  case object Bechukosai extends Parsha
  case object Bemidbar extends Parsha
  case object Nasso extends Parsha
  case object Beha_aloscha extends Parsha
  case object Shelach extends Parsha
  case object Korach extends Parsha
  case object Chukas extends Parsha
  case object Balak extends Parsha
  case object Pinchas extends Parsha
  case object Mattos extends Parsha
  case object Masei extends Parsha
  case object Devarim extends Parsha
  case object Va_eschanan extends Parsha
  case object Eikev extends Parsha
  case object Re_eh extends Parsha
  case object Shoftim extends Parsha
  case object KiSeitzei extends Parsha
  case object KiSavo extends Parsha
  case object Nitzavim extends Parsha
  case object Vayeilech extends Parsha
  case object Haazinu extends Parsha
  case object VezosHaberacha extends Parsha

  // TODO add half-parshiot for the Dardaki custom

  final val all: Seq[Parsha] = Seq(
    Bereishis, Noach, LechLecha, Vayeira, ChayeiSarah, Toldos,
    Vayeitzei, Vayishlach, Vayeishev, Mikeitz, Vayigash, Vayechi,
    Shemos, Va_eira, Bo, Beshalach, Yisro, Mishpatim, Terumah, Tetzaveh, KiSisa, Vayakhel, Pekudei,
    Vayikra, Tzav, Shemini, Tazria, Metzora, Acharei, Kedoshim, Emor, Behar, Bechukosai,
    Bemidbar, Nasso, Beha_aloscha, Shelach, Korach, Chukas, Balak, Pinchas, Mattos, Masei,
    Devarim, Va_eschanan, Eikev, Re_eh, Shoftim, KiSeitzei, KiSavo, Nitzavim, Vayeilech, Haazinu, VezosHaberacha
  )

  def forIndex(index: Int): Parsha = all(index)

  def indexOf(parsha: Parsha): Int = all.indexOf(parsha)

  def distance(from: Parsha, to: Parsha): Int = indexOf(to) - indexOf(from)
}
