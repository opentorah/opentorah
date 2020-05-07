package org.opentorah.dates

import org.opentorah.util.Cache

/**
  *
  */
abstract class YearCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  private final val yearsCache: Cache[Int, C#Year] = new Cache[Int, C#Year] {
    override def calculate(number: Int): C#Year = newYear(number)
  }

  final def apply(number: Int): C#Year =
    yearsCache.get(number, calendar.cacheYears)

  protected def newYear(number: Int): C#Year

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

  final def yearsForSureBefore(dayNumber: Int): Int =  {
    val result: Int = (4 * dayNumber / (4 * 365 + 1)) - 1
    if (areYearsPositive) scala.math.max(1, result) else result
  }

  def isLeap(yearNumber: Int): Boolean

  def firstMonth(yearNumber: Int): Int

  def lengthInMonths(yearNumber: Int): Int
}
