package org.podval.calendar.generate.tanach

sealed trait Parsha {
  def name: String = getClass.getSimpleName.replace("$", "")

  lazy val structure: Parsha.Structure = Parsha.structureForName(name)
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

  final class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

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

  case object Bereishis extends Parsha
  case object Noach extends Parsha
  case object LechLecha extends Parsha { override def name: String = "Lech Lecha" }
  case object Vayeira extends Parsha
  case object ChayeiSarah extends Parsha { override def name: String = "Chayei Sarah" }
  case object Toldos extends Parsha
  case object Vayeitzei extends Parsha
  case object Vayishlach extends Parsha
  case object Vayeishev extends Parsha
  case object Mikeitz extends Parsha
  case object Vayigash extends Parsha
  case object Vayechi extends Parsha
  case object Shemos extends Parsha
  case object Va_eira extends Parsha { override def name: String = "Va'eira" }
  case object Bo extends Parsha
  case object Beshalach extends Parsha
  case object Yisro extends Parsha
  case object Mishpatim extends Parsha
  case object Terumah extends Parsha
  case object Tetzaveh extends Parsha
  case object KiSisa extends Parsha { override def name: String = "Ki Sisa" }
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
  case object Beha_aloscha extends Parsha { override def name: String = "Beha'aloscha" }
  case object Shelach extends Parsha
  case object Korach extends Parsha
  case object Chukas extends Parsha
  case object Balak extends Parsha
  case object Pinchas extends Parsha
  case object Mattos extends Parsha
  case object Masei extends Parsha
  case object Devarim extends Parsha
  case object Va_eschanan extends Parsha { override def name: String = "Va'eschanan" }
  case object Eikev extends Parsha
  case object Re_eh extends Parsha { override def name: String = "Re'eh" }
  case object Shoftim extends Parsha
  case object KiSeitzei extends Parsha { override def name: String = "Ki Seitzei" }
  case object KiSavo extends Parsha { override def name: String = "Ki Savo" }
  case object Nitzavim extends Parsha
  case object Vayeilech extends Parsha
  case object Haazinu extends Parsha
  case object VezosHaberachah extends Parsha { override def name: String = "Vezos Haberachah" }

  // TODO add half-parshiot for the Dardaki custom

  final val all: Seq[Parsha] = Seq(
    Bereishis, Noach, LechLecha, Vayeira, ChayeiSarah, Toldos,
    Vayeitzei, Vayishlach, Vayeishev, Mikeitz, Vayigash, Vayechi,
    Shemos, Va_eira, Bo, Beshalach, Yisro, Mishpatim, Terumah, Tetzaveh, KiSisa, Vayakhel, Pekudei,
    Vayikra, Tzav, Shemini, Tazria, Metzora, Acharei, Kedoshim, Emor, Behar, Bechukosai,
    Bemidbar, Nasso, Beha_aloscha, Shelach, Korach, Chukas, Balak, Pinchas, Mattos, Masei,
    Devarim, Va_eschanan, Eikev, Re_eh, Shoftim, KiSeitzei, KiSavo, Nitzavim, Vayeilech, Haazinu, VezosHaberachah
  )

  def forIndex(index: Int): Parsha = all(index)

  def indexOf(parsha: Parsha): Int = all.indexOf(parsha)

  def distance(from: Parsha, to: Parsha): Int = indexOf(to) - indexOf(from)

  def forName(name: String): Parsha = structure2parsha(structureForName(name))

  private val structures: Seq[Parsha.Structure] = Tanach.chumash.flatMap(_.structure.weeks)
  private val structure2parsha: Map[Parsha.Structure, Parsha] = all.map(parsha => parsha.structure -> parsha).toMap
  require(structures.toSet == structure2parsha.keySet)

  private def structureForName(name: String): Parsha.Structure = {
    val result = structures.filter(_.names.has(name))
    require(result.nonEmpty, s"No structure for $name")
    require(result.length == 1)
    result.head
  }
}
