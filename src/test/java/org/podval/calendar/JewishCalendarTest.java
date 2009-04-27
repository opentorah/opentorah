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
        final Date<JewishMonth> date = jewishCalendar.dateFromDate(year, month, day);
        Assert.assertEquals(date, jewishCalendar.dateFromDays(date.getDays()));
    }


    @Test
    public void days2date2days() {
        days2date2days(6);
        days2date2days(28);
        days2date2days(29);
        days2date2days(30);
        days2date2days(31);
        days2date2days(280);
        days2date2days(2800);
        days2date2days(28000);
    }


    private void days2date2days(final int days) {
        Assert.assertEquals(days, jewishCalendar.dateFromDays(days).getDays());
    }


    @Test
    public void date2days() {
        date2days(6, 1, JewishMonth.Tishri, 1);
    }


    private void date2days(final int days, final int year, final JewishMonth month, final int day) {
        Assert.assertEquals(days, jewishCalendar.dateFromDate(year, month, day).getDays());
    }


    @Test
    public void days2date() {
        days2date(6, 1, JewishMonth.Tishri, 1);
    }


    private void days2date(final int days, final int year, final JewishMonth month, final int day) {
        Assert.assertEquals(jewishCalendar.dateFromDays(days), jewishCalendar.dateFromDate(year, month, day));
    }
}
