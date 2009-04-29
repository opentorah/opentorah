package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;


public class JewishCalendarTest {

    private static final JewishCalendar jewishCalendar = Calendar.getJewish();


    @Test
    public void date2days2date() {
        date2days2date(1, JewishMonth.Tishri, 1);
        date2days2date(5768, JewishMonth.AdarII, 28);
        date2days2date(5769, JewishMonth.Nissan, 14);
    }


    private void date2days2date(final int year, final JewishMonth month, final int day) {
        final Date<JewishMonth> fromDate = jewishCalendar.dateFromDate(year, month, day);
        final Date<JewishMonth> fromDays = jewishCalendar.dateFromDays(fromDate.getDays());
        Assert.assertEquals(fromDate, fromDays);
    }


    @Test
    public void date2days() {
        date2days(6, 1, JewishMonth.Tishri, 1);
    }


    private void date2days(final int days, final int year, final JewishMonth month, final int day) {
        final Date<JewishMonth> fromDays = jewishCalendar.dateFromDays(days);
        final Date<JewishMonth> fromDate = jewishCalendar.dateFromDate(year, month, day);
        Assert.assertEquals(days, fromDate.getDays());
        Assert.assertEquals(fromDays, fromDate);
    }
}
