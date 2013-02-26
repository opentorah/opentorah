package org.podval.calendar.dates;

import org.junit.{Test, Assert}

import JewishCalendar.{Month => jm, Day => jd}
import GregorianCalendar.{Month => gm}


final class NewMoonTest {

  val x = JewishCalendar
  val y = GregorianCalendar


  @Test
  def newMoons {
    // From http://www.chabad.org/library/article_cdo/aid/216238/jewish/Molad-Times.htm
    //
    newMoon(5769, jm.Tishrei   , jd.Shlishi, 5769, jm.Tishrei,  1, 2008, gm.September, 30,  1, 58, 13)
    newMoon(5769, jm.Marheshvan, jd.Rvii   , 5769, jm.Tishrei, 30, 2008, gm.October  , 29,  2, 42, 14)
    newMoon(5769, jm.Kislev    , jd.Shishi , 5769, jm.Kislev ,  1, 2008, gm.November , 28,  3, 26, 15)
    newMoon(5771, jm.Elul      , jd.Sheni  , 5771, jm.Av     , 28, 2011, gm.September, 28, 17,  8, 14) // XXX day of the week?
    newMoon(5772, jm.Tishrei   , jd.Shlishi, 5772, jm.Tishrei, 28, 2011, gm.October  , 28,  5, 52, 15) // XXX day of the week?
  }



  def newMoon(moladYear: Int, moladMonth: jm.Name, dayOfWeek: jd.Name,
              year: Int, month: jm.Name, day: Int,
              yearG: Int, monthG: gm.Name, dayG: Int,
              hours: Int, minutes: Int, parts: Int)
  {
    val molad = JewishCalendar.Year(moladYear).month(moladMonth).newMoon
    Assert.assertEquals(dayOfWeek, molad.day.name)

    Assert.assertEquals(JewishCalendar.Day(year, month, day), molad.day)
    // XXX conversion of Moments!!!
//    val moladG = Conversions.fromJewish(molad)
    // XXX time with minutes!
    val dateG = GregorianCalendar.Day(yearG, monthG, dayG).time(hours, minutes*18+parts)
  }
}
