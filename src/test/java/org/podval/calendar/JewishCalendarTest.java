package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;


public class JewishCalendarTest {

    @Test
    public void conversionFromDate() {
        date2days2date(1, JewishMonth.Tishri, 1);
        date2days2date(5768, JewishMonth.AdarII, 28);
        date2days2date(5769, JewishMonth.Nissan, 14);
    }


    private void date2days2date(final int year, final JewishMonth month, final int day) {
        final JewishDate date = JewishCalendar.dateFromDate(year, month, day);
        final int days = JewishCalendar.daysFromDate(date);
        final JewishDate date_ = JewishCalendar.dateFromDays(days);
        Assert.assertEquals(date, date_);
    }
}
