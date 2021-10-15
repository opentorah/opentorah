package org.opentorah.metadata

object Week:

  enum Day extends Named.ByLoader[Day](loader = Day, nameOverride = None), HasName.Enum derives CanEqual :
    case Sunday extends Day
    case Monday extends Day
    case Tuesday extends Day
    case Wednesday extends Day
    case Thursday extends Day
    case Friday extends Day
    case Saturday extends Day

  object Day extends Names.Loader[Day], HasValues.FindByDefaultName[Day]:
    val Rishon: Day = Sunday
    val Sheni: Day = Monday
    val Shlishi: Day = Tuesday
    val Rvii: Day = Wednesday
    val Chamishi: Day = Thursday
    val Shishi: Day = Friday
    val Shabbos: Day = Saturday

    require(values.length == length)

    def forNumber(numberInWeek: Int): Day = fromOrdinal(numberInWeek - 1)

    override val valuesSeq: Seq[Day] = values.toIndexedSeq

  final val length: Int = 7
