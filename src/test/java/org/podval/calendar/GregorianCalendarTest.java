package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;



public class GregorianCalendarTest {

    @Test
    public void gregorian2jewish() {
        gregorian2jewish(1964, GregorianMonth.October, 30, 5725, JewishMonth.MarHeshvan, 24);
    }


    private void gregorian2jewish(
        final int gYear,
        final GregorianMonth gMonth,
        final int gDay,
        final int jYear,
        final JewishMonth jMonth,
        final int jDay)
    {
        final Date gDate = Calendar.getGregorian().dateFromDate(gYear, gMonth, gDay);
        final Date jDate = Calendar.getJewish().dateFromDate(jYear, jMonth, jDay);
        Assert.assertEquals(gDate, jDate);
    }
}
