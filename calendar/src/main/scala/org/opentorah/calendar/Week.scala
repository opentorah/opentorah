package org.opentorah.calendar

import org.opentorah.metadata.{Named, NamedCompanion, Names}

object Week:

  sealed trait Day extends Named:
    final override def names: Names = Day.toNames(this)

  object Day extends NamedCompanion:
    override type Key = Day

    case object Sunday    extends Day
    case object Monday    extends Day
    case object Tuesday   extends Day
    case object Wednesday extends Day
    case object Thursday  extends Day
    case object Friday    extends Day
    case object Saturday  extends Day

    val Rishon  : Day = Sunday
    val Sheni   : Day = Monday
    val Shlishi : Day = Tuesday
    val Rvii    : Day = Wednesday
    val Chamishi: Day = Thursday
    val Shishi  : Day = Friday
    val Shabbos : Day = Saturday

    override val values: Seq[Day] = Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi, Shabbos)

    require(values.length == length)

    def forNumber(numberInWeek: Int): Day = values(numberInWeek - 1)

  final val length: Int = 7
