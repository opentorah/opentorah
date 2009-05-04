package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;


public class BirkatHahamaTest {

    @Test
    public void birkatHahamaWikipedia() {
        // From http://en.wikipedia.org/wiki/Birkat_HaHammah#Occurrences_of_Birkat_Hachama

        cycle(202, 5657, JewishMonth.Nissan, 5, 1897, GregorianMonth.April, 7);
        cycle(203, 5685, JewishMonth.Nissan, 14, 1925, GregorianMonth.April, 8);
        cycle(204, 5713, JewishMonth.Nissan, 23, 1953, GregorianMonth.April, 8);
        cycle(205, 5741, JewishMonth.Nissan, 4, 1981, GregorianMonth.April, 8);
        cycle(206, 5769, JewishMonth.Nissan, 14, 2009, GregorianMonth.April, 8);
        cycle(207, 5797, JewishMonth.Nissan, 23, 2037, GregorianMonth.April, 8);
        cycle(208, 5825, JewishMonth.Nissan, 2, 2065, GregorianMonth.April, 8);
        cycle(209, 5853, JewishMonth.Nissan, 12, 2093, GregorianMonth.April, 8);
        cycle(210, 5881, JewishMonth.Nissan, 21, 2121, GregorianMonth.April, 9);
        // ...
        cycle(214, 5993, JewishMonth.AdarII, 29, 2233, GregorianMonth.April, 10);
    }


    private void cycle(
        final int number,
        final int jYear,
        final JewishMonth jMonth,
        final int jDay,
        final int gYear,
        final GregorianMonth gMonth,
        final int gDay)
    {
        final Date<JewishMonth> tDate = Calendar.getJewish().birkasHachama(number);
        final Date<JewishMonth> jDate = Calendar.getJewish().dateFromDate(jYear, jMonth, jDay);
        final Date<JewishMonth> gDate = Calendar.getGregorian().dateFromDate(gYear, gMonth, gDay).setTime(18, 0, 0);
        Assert.assertEquals(4, Calendar.dayOfTheWeek(tDate.getDays()));
        Assert.assertEquals(jDate, tDate);
        Assert.assertEquals(gDate, tDate.toGregorian());
    }
}
