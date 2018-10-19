package org.podval.calendar.jewish

import Jewish.{Year, Day}

final case class YearType
(
  isLeap: Boolean,
  name: String,
  roshHashanah: Day.Name,
  kind: Year.Kind,
  pesach: Day.Name
)

// This table of unknown origin was submitted by @michaelko58; it gives all occurring year types.
object YearType {
  val N2S: YearType = YearType(isLeap = false, "בחג")
  val N2F: YearType = YearType(isLeap = false, "בשה")
  val N3R: YearType = YearType(isLeap = false, "גכה")
  val N5R: YearType = YearType(isLeap = false, "הכז")
  val N5F: YearType = YearType(isLeap = false, "השא")
  val N7S: YearType = YearType(isLeap = false, "זחא")
  val N7F: YearType = YearType(isLeap = false, "זשג")

  val L2S: YearType = YearType(isLeap = true, "בחה")
  val L2F: YearType = YearType(isLeap = true, "בשז")
  val L3R: YearType = YearType(isLeap = true, "גכז")
  val L4S: YearType = YearType(isLeap = true, "החא")
  val L5F: YearType = YearType(isLeap = true, "השג")
  val L7S: YearType = YearType(isLeap = true, "זחג")
  val L7F: YearType = YearType(isLeap = true, "זשה")

  private val types: Seq[YearType] = Seq(N2S, N2F, N3R, N5R, N5F, N7S, N7F, L2S, L2F, L3R, L4S, L5F, L7S, L7F)

  def get(year: Year): YearType = {
    val (isLeap, kind) = year.character
    types.find { yearType =>
      (yearType.isLeap == isLeap) &&
      (yearType.roshHashanah == SpecialDay.RoshHashanah1(year).name) &&
      (yearType.kind == kind)
    }.get
  }

  def apply(isLeap: Boolean, name: String): YearType = {
    require(name.length == 3)

    def dayOfTheWeek(char: Char): Day.Name = char match {
      case 'א' => Day.Name.Rishon
      case 'ב' => Day.Name.Sheni
      case 'ג' => Day.Name.Shlishi
      case 'ה' => Day.Name.Chamishi
      case 'ז' => Day.Name.Shabbos
    }

    new YearType(
      isLeap,
      name,
      roshHashanah = dayOfTheWeek(name.charAt(0)),
      kind = name.charAt(1) match {
        case 'ח' => Year.Kind.Short
        case 'כ' => Year.Kind.Regular
        case 'ש' => Year.Kind.Full
      },
      pesach = dayOfTheWeek(name.charAt(2))
    )
  }
}
