package org.podval.calendar.generate.tanach

import org.podval.calendar.generate.tanach.Tanach.ChumashBook

sealed trait Parsha {
  def book: ChumashBook

  def name: String = getClass.getSimpleName.replace("$", "")

  final def structure: Parsha.Structure = TanachParser.forParsha(this)
}

object Parsha {
  final class Structure(
    val names: Names,
    val fromChapter: Int,
    val fromVerse: Int,
    val toChapter: Int,
    val toVerse: Int,
    val days: Array[Day], // length 7 :)
    val maftir: Maftir
  ) {
    require(fromChapter > 0)
    require(fromVerse > 0)
  }

  final class Day(
    val fromChapter: Int,
    val fromVerse: Int,
    val toChapter: Int,
    val toVerse: Int
  )

  // TODO special Maftir has reference to the book; generalize?
  final class Maftir(
    val fromChapter: Int,
    val fromVerse: Int,
    val toChapter: Int,
    val toVerse: Int
  )

  final class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  trait GenesisParsha extends Parsha {
    final override def book: ChumashBook = Tanach.Genesis
  }

  trait ExodusParsha extends Parsha {
    final override def book: ChumashBook = Tanach.Exodus
  }

  trait LeviticusParsha extends Parsha {
    final override def book: ChumashBook = Tanach.Leviticus
  }

  trait NumbersParsha extends Parsha {
    final override def book: ChumashBook = Tanach.Numbers
  }

  trait DeutoronomyParsha extends Parsha {
    final override def book: ChumashBook = Tanach.Deuteronomy
  }

  case object Bereishis extends GenesisParsha
  case object Noach extends GenesisParsha
  case object LechLecha extends GenesisParsha { override def name: String = "Lech Lecha" }
  case object Vayeira extends GenesisParsha
  case object ChayeiSarah extends GenesisParsha { override def name: String = "Chayei Sarah" }
  case object Toldos extends GenesisParsha
  case object Vayeitzei extends GenesisParsha
  case object Vayishlach extends GenesisParsha
  case object Vayeishev extends GenesisParsha
  case object Mikeitz extends GenesisParsha
  case object Vayigash extends GenesisParsha
  case object Vayechi extends GenesisParsha

  case object Shemos extends ExodusParsha
  case object Va_eira extends ExodusParsha { override def name: String = "Va'eira" }
  case object Bo extends ExodusParsha
  case object Beshalach extends ExodusParsha
  case object Yisro extends ExodusParsha
  case object Mishpatim extends ExodusParsha
  case object Terumah extends ExodusParsha
  case object Tetzaveh extends ExodusParsha
  case object KiSisa extends ExodusParsha { override def name: String = "Ki Sisa" }
  case object Vayakhel extends ExodusParsha
  case object Pekudei extends ExodusParsha

  case object Vayikra extends LeviticusParsha
  case object Tzav extends LeviticusParsha
  case object Shemini extends LeviticusParsha
  case object Tazria extends LeviticusParsha
  case object Metzora extends LeviticusParsha
  case object Acharei extends LeviticusParsha
  case object Kedoshim extends LeviticusParsha
  case object Emor extends LeviticusParsha
  case object Behar extends LeviticusParsha
  case object Bechukosai extends LeviticusParsha

  case object Bemidbar extends NumbersParsha
  case object Nasso extends NumbersParsha
  case object Beha_aloscha extends NumbersParsha { override def name: String = "Beha'aloscha" }
  case object Shelach extends NumbersParsha
  case object Korach extends NumbersParsha
  case object Chukas extends NumbersParsha
  case object Balak extends NumbersParsha
  case object Pinchas extends NumbersParsha
  case object Mattos extends NumbersParsha
  case object Masei extends NumbersParsha

  case object Devarim extends DeutoronomyParsha
  case object Va_eschanan extends DeutoronomyParsha { override def name: String = "Va'eschanan" }
  case object Eikev extends DeutoronomyParsha
  case object Re_eh extends DeutoronomyParsha { override def name: String = "Re'eh" }
  case object Shoftim extends DeutoronomyParsha
  case object KiSeitzei extends DeutoronomyParsha { override def name: String = "Ki Seitzei" }
  case object KiSavo extends DeutoronomyParsha { override def name: String = "Ki Savo" }
  case object Nitzavim extends DeutoronomyParsha
  case object Vayeilech extends DeutoronomyParsha
  case object Haazinu extends DeutoronomyParsha
  case object VezosHaberachah extends DeutoronomyParsha { override def name: String = "Vezos Haberachah" }

  // TODO add half-parshiot for the Dardaki custom

  final val all: Seq[Parsha] = Seq(
    Bereishis, Noach, LechLecha, Vayeira, ChayeiSarah, Toldos,
    Vayeitzei, Vayishlach, Vayeishev, Mikeitz, Vayigash, Vayechi,
    Shemos, Va_eira, Bo, Beshalach, Yisro, Mishpatim, Terumah, Tetzaveh, KiSisa, Vayakhel, Pekudei,
    Vayikra, Tzav, Shemini, Tazria, Metzora, Acharei, Kedoshim, Emor, Behar, Bechukosai,
    Bemidbar, Nasso, Beha_aloscha, Shelach, Korach, Chukas, Balak, Pinchas, Mattos, Masei,
    Devarim, Va_eschanan, Eikev, Re_eh, Shoftim, KiSeitzei, KiSavo, Nitzavim, Vayeilech, Haazinu, VezosHaberachah
  )

  def forBook(book: ChumashBook): Seq[Parsha] = all.filter(_.book == book)

  def forIndex(index: Int): Parsha = all(index)

  def indexOf(parsha: Parsha): Int = all.indexOf(parsha)

  def distance(from: Parsha, to: Parsha): Int = indexOf(to) - indexOf(from)

  def forName(name: String): Option[Parsha] = TanachParser.forParshaName(name)
}
