package org.podval.calendar.dates.jewish

import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class YearTest extends FlatSpec {

  private val years = (1 to 6000) map (Jewish.Year(_))

  "A Jewish year" should "have a valid kind" in {
    years foreach (_.kind)
  }

  it should "belong to the year it was retrieved from" in {
    for (year <- years; month <- 1 to year.lengthInMonths)
      assertResult(year)(year.month(month).year)
  }
}
