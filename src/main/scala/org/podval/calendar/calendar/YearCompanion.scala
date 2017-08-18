package org.podval.calendar.calendar

/**
  *
  */
abstract class YearCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(number: Int): C#Year = calendar.createYear(number)

  final def apply(month: C#Month): C#Year =
    apply(calendar.Month.yearNumber(month.number))

  final def apply(day: C#Day): C#Year = {
    var result = apply(yearsForSureBefore(day.number))
    require(result.firstDayNumber <= day.number)
    while (result.next.firstDayNumber <= day.number) result = result.next
    result
  }

  val monthDescriptors: Map[C#YearCharacter, List[C#MonthDescriptor]] =
    Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)

  protected def characters: Seq[C#YearCharacter]

  private[this] def monthsGenerator(character: C#YearCharacter): List[C#MonthDescriptor] = {
    val namesAndLengths = monthNamesAndLengths(character)
    // TODO dayses?
    val daysesBefore = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
    namesAndLengths zip daysesBefore map { case (nameAndLength, daysBefore) =>
      new MonthDescriptorBase(nameAndLength.name, nameAndLength.length, daysBefore)
    }
  }

  protected def monthNamesAndLengths(character: C#YearCharacter): List[C#MonthNameAndLength]

  // TODO scope inside MonthCompanion
  protected final def createMonthNameAndLength(name: C#MonthName, length: Int):
    C#MonthNameAndLength = new MonthNameAndLengthBase(name, length)

  protected def areYearsPositive: Boolean

  // TODO centralize the constants (in a Gregorian companion object?).
  private[this] final def yearsForSureBefore(dayNumber: Int): Int =  {
    val result = (4 * dayNumber / (4 * 365 + 1)) - 1
    if (areYearsPositive) scala.math.max(1, result) else result
  }

  def isLeap(yearNumber: Int): Boolean

  def firstMonth(yearNumber: Int): Int

  def lengthInMonths(yearNumber: Int): Int
}
