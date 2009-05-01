package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;


public class MoladTest {

    @Test
    public void cycleNumbers() {
        cycleNumber(1, 1);
        cycleNumber(2, 1);
        cycleNumber(18, 1);
        cycleNumber(19, 1);
        cycleNumber(20, 2);
        cycleNumber(37, 2);
        cycleNumber(38, 2);
        cycleNumber(39, 3);

        cycleNumber(0, 0);
        cycleNumber(-1, 0);
        cycleNumber(-2, 0);
        cycleNumber(-18, 0);

        cycleNumber(-19, -1);
        cycleNumber(-20, -1);
        cycleNumber(-37, -1);
        cycleNumber(-38, -2);
        cycleNumber(-39, -2);
    }


    private void cycleNumber(final int year, final int cycle) {
        Assert.assertEquals(cycle, Calendar.getJewish().cycleNumber(year));
    }


    @Test
    public void moladNumbers() {
        moladNumber(1, 2, 1);
        moladNumber(1, 1, 0);
        moladNumber(0, 13, -1);
        moladNumber(0, 12, -2);
    }


    private void moladNumber(final int year, final int month, final int number) {
        Assert.assertEquals(number, Calendar.getJewish().moladNumber(year, month));
    }


    @Test
    public void table5769() {
        molad(5769, 1, 3, true , 1, 58, 13);
        molad(5769, 2, 4, false, 2, 42, 14);
        molad(5769, 3, 6, true, 3, 26, 15);
    }


    private void moladTable(final int year) {
        for (int m = 1; m <= Calendar.getJewish().monthsInYear(year); m++) {
            System.out.println("Molad " + year + " " + m + " = " + Calendar.getJewish().moladDate(year, m));
        }
    }


    public void molad(
        final int year,
        final int month,
        final int dayOfTheWeek,
        final boolean am,
        final int hour,
        final int minute,
        final int parts)
    {
        final long molad = Calendar.getJewish().molad(year, month);
        final int days = JewishCalendar.daysFromParts(molad);
        final int cDayOfTheWeek = JewishCalendar.dayOfTheWeek(days);
        final int cHour = JewishCalendar.hoursFromParts(molad);
        final int cMinute = JewishCalendar.minutesFromParts(molad);
        final int cParts = JewishCalendar.partsFromParts(molad);

        Assert.assertEquals(dayOfTheWeek, cDayOfTheWeek);
        Assert.assertEquals(hour + (am ? 6 : 18), cHour);
        Assert.assertEquals(minute, cMinute);
        Assert.assertEquals(parts, cParts);
    }
}
