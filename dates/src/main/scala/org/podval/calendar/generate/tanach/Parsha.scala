package org.podval.calendar.generate.tanach

import Tanach.ChumashBook
import org.podval.calendar.metadata.{HasNames, Names, WithMetadata}

sealed trait Parsha extends WithMetadata[Parsha, Parsha.Structure] {
  def book: ChumashBook

  final override def toMetadata: Map[Parsha, Parsha.Structure] = book.metadata.weeks

  final def combines: Boolean = Parsha.combinableAll.contains(this)
}

object Parsha {
  final class Structure(
    val parsha: Parsha,
    override val names: Names,
    val span: Span,
    val days: Seq[Span], // length 7 :)
    val daysCustom: Map[String, Seq[Span]],
    val daysCombined: Seq[Span],
    val daysCombinedCustom: Map[String, Seq[Span]],
    val maftir: Span,
    val aliyot: Seq[Span] // length 3
  ) extends HasNames

  final class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  trait GenesisParsha extends Parsha { final override def book: ChumashBook = Tanach.Genesis }

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

  trait ExodusParsha extends Parsha { final override def book: ChumashBook = Tanach.Exodus }

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

  trait LeviticusParsha extends Parsha { final override def book: ChumashBook = Tanach.Leviticus }

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

  trait NumbersParsha extends Parsha { final override def book: ChumashBook = Tanach.Numbers }

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

  trait DeutoronomyParsha extends Parsha { final override def book: ChumashBook = Tanach.Deuteronomy }

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

  def forName(name: String): Option[Parsha] = all.find(_.names.has(name))

  // Rules of combining; affect the WeeklyReading.
  // TODO deal with alternative customs of what and in what sequence combines?
  final val combinableFromBereishisToVayikra: Seq[Parsha] = Seq(Vayakhel)
  // TODO see #56; Magen Avraham 428:4 (6);
  // Reversing the priorities here currently affects only non-leap regular years with Rosh
  // Hashanah on Thursday (and Pesach on Shabbat).
  final val combinableFromVayikraToBemidbar: Seq[Parsha] = Seq(Tazria, Acharei, Behar)
  final val combinableFromBemidbarToVa_eschanan: Seq[Parsha] = Seq(Mattos, Chukas)
  final val combinableFromVa_eschanan: Seq[Parsha] = Seq(Nitzavim)

  final val combinableAll: Set[Parsha] = (combinableFromBereishisToVayikra ++ combinableFromVayikraToBemidbar ++
    combinableFromBemidbarToVa_eschanan ++ combinableFromVa_eschanan).toSet
}
