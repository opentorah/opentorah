package org.podval.calendar.dates;

import org.junit.Test;
import org.junit.Assert;


public class JewishCalendarTest {

    @Test
    public void date2days2date() {
        date2days2date(1, JewishMonth.Tishri, 1);
        date2days2date(5768, JewishMonth.AdarII, 28);
        date2days2date(5769, JewishMonth.Nissan, 14);
    }


    private void date2days2date(final int year, final JewishMonth month, final int day) {
        final JewishDate fromDate = JewishDate.create(year, month, day);
        final JewishDate fromDays = JewishCalendar.getInstance().dateFromDays(fromDate.getDays());
        Assert.assertEquals(fromDate, fromDays);
    }


    @Test
    public void date2days() {
        // @todo xxx
//        date2days(6, 1, JewishMonth.Tishri, 1);
    }


    private void date2days(final int days, final int year, final JewishMonth month, final int day) {
        final JewishDate fromDays = JewishCalendar.getInstance().dateFromDays(days);
        final JewishDate fromDate = JewishDate.create(year, month, day);
        Assert.assertEquals(days, fromDate.getDays());
        Assert.assertEquals(fromDays, fromDate);
    }
}
