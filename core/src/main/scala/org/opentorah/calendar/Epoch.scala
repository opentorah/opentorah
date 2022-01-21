package org.opentorah.calendar

trait Epoch:
  // days before the start of the calendar counting from the start of the Jewish calendar
  def epoch: Int

  protected final def epochDifference(that: Epoch): Int = epoch - that.epoch

  // hours offset; for example:
  //  Jewish:   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Roman :  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  def epochHours: Int

  protected final def epochHoursDifference(that: Epoch): Int = epochHours - that.epochHours

object Epoch:

  object JulianDay extends Epoch:
    // first Julian Day in the Jewish calendar
    val epoch: Int = -347997

    // Creation epoch starts at 6pm, Julian Days start at noon
    val epochHours: Int = -6
