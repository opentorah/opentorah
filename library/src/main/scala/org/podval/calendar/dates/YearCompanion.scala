package org.podval.calendar.dates

/**
  *
  */
abstract class YearCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  def apply(number: Int): C#Year

  final def apply(month: C#Month): C#Year =
    apply(calendar.Month.yearNumber(month.number))

  final def apply(day: C#Day): C#Year = {
    var result = apply(yearsForSureBefore(day.number))
    require(result.firstDayNumber <= day.number)
    while (result.next.firstDayNumber <= day.number) result = result.next
    result
  }

  // lazy to make initialization work
  lazy val monthDescriptors: Map[C#YearCharacter, Seq[C#MonthDescriptor]] =
    Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)

  protected def characters: Seq[C#YearCharacter]

  private[this] def monthsGenerator(character: C#YearCharacter): Seq[C#MonthDescriptor] = {
    val namesAndLengths = monthNamesAndLengths(character)
    val daysBeforeForMonth: Seq[Int] = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
    namesAndLengths zip daysBeforeForMonth map { case (nameAndLength, daysBefore) =>
      new MonthDescriptorBase[C](nameAndLength.name, nameAndLength.length, daysBefore)
    }
  }

  protected def monthNamesAndLengths(character: C#YearCharacter): Seq[C#MonthNameAndLength]

  protected final def createMonthNameAndLength(name: C#MonthName, length: Int):
    C#MonthNameAndLength = new MonthNameAndLengthBase[C](name, length)

  protected final def yearLength(character: C#YearCharacter): Int = {
    val lastMonth: C#MonthDescriptor = monthDescriptors(character).last
    lastMonth.daysBefore + lastMonth.length
  }

  protected def areYearsPositive: Boolean

  private[this] final def yearsForSureBefore(dayNumber: Int): Int =  {
    val result = (4 * dayNumber / (4 * 365 + 1)) - 1
    if (areYearsPositive) scala.math.max(1, result) else result
  }

  def isLeap(yearNumber: Int): Boolean

  def firstMonth(yearNumber: Int): Int

  def lengthInMonths(yearNumber: Int): Int
}
