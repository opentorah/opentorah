package org.podval.calendar.generate.chumash

// TODO load the names in various languages from the XML file

sealed trait Parsha

object Parsha {
  case object Bereshit extends Parsha
  case object Noach extends Parsha
  case object LechLecha extends Parsha
  case object Vayeira extends Parsha
  case object ChayeiSarah extends Parsha
  case object Toledot extends Parsha
  case object Vayetze extends Parsha
  case object Vayishlach extends Parsha
  case object Vayeshev extends Parsha
  case object Miketz extends Parsha
  case object Vayigash extends Parsha
  case object Vayechi extends Parsha
  case object Shemot extends Parsha
  case object Vaeira extends Parsha
  case object Bo extends Parsha
  case object Beshalach extends Parsha
  case object Yitro extends Parsha
  case object Mishpatim extends Parsha
  case object Terumah extends Parsha
  case object Tetzaveh extends Parsha
  case object KiTisa extends Parsha
  case object Vayakhel extends Parsha
  case object Pekudei extends Parsha
  case object Vayikra extends Parsha
  case object Tzav extends Parsha
  case object Shemini extends Parsha
  case object Tazria extends Parsha
  case object Metzora extends Parsha
  case object AchareiMot extends Parsha
  case object Kedoshim extends Parsha
  case object Emor extends Parsha
  case object Behar extends Parsha
  case object Bechukotai extends Parsha
  case object Bemidbar extends Parsha
  case object Naso extends Parsha
  case object Behaalotecha extends Parsha
  case object Shlach extends Parsha
  case object Korach extends Parsha
  case object Chukat extends Parsha
  case object Balak extends Parsha
  case object Pinchas extends Parsha
  case object Matot extends Parsha
  case object Masei extends Parsha
  case object Devarim extends Parsha
  case object Vaetchanan extends Parsha
  case object Eikev extends Parsha
  case object Reeh extends Parsha
  case object Shoftim extends Parsha
  case object KiTeitzei extends Parsha
  case object KiTavo extends Parsha
  case object Nitzavim extends Parsha
  case object Vayelech extends Parsha
  case object Haazinu extends Parsha
  case object VZotHaBerachah extends Parsha
}
