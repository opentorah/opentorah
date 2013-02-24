package org.podval.calendar.dates;

import org.junit.{Test, Assert, Before}

import JewishCalendar.Month

final class NewMoonTest {

  @Before
  def triggerInitializationOfTheJewishCalendar {
    val x = JewishCalendar
  }

    @Test
    def dummy = {}


    @Test
    def when2011() {
        // TODO
        println(Month(   1, 1).newMoon.toFullString)
        println(Month(5772, 2).newMoon.toFullString)
        println(Month(5772, 3).newMoon.toFullString)
        println(Month(5772, 4).newMoon.toFullString)
    }

  /*
   * New Moons
   * 5771 Elul     28   23: 8:14   2011 Sep 28 17: 8:14
   * 5772 Tishrei   1   11:52:15   2011 Oct 28  5:52:15
   * 5772 Cheshvan 29   
   */

//    @Test
//    public void table5769() {
//        // From http://www.chabad.org/library/article_cdo/aid/216238/jewish/Molad-Times.htm
//
//        molad(5769, JewishMonth.Tishri, 3, JewishMonth.Tishri, 1, 2008, GregorianMonth.September, 30, 1, true, 58, 13);
//        molad(5769, JewishMonth.MarHeshvan, 4, JewishMonth.Tishri, 30, 2008, GregorianMonth.October, 29, 2, false, 42, 14);
//        molad(5769, JewishMonth.Kislev, 6, JewishMonth.Kislev, 1, 2008, GregorianMonth.November, 28, 3, true, 26, 15);
//    }
//
//
//        moladDate(year, month);
//        Assert.assertEquals(dayOfTheWeek, mDate.getDayOfTheWeek());
//
//        Assert.assertEquals(jMonth, mDate.getMonth().month);
//        Assert.assertEquals(jDay, mDate.getDay());
//
//        final GregorianDate gDate = GregorianDate.create(gYear, gMonth, gDay)
//            .setTime(hour + (am ? 0 : 12), minute, parts);
//
//        Assert.assertEquals(gDate, mDate.toGregorian());
//    }
}
