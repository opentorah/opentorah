package org.podval.calendar.dates;

import org.junit.{Test, Assert}

import MonthName._


final class DatesTest {

    @Test
    def dayOfWeek {
        val day = Year(5772).month(MonthName.Marheshvan).day(24)
        Assert.assertEquals(2, day.dayOfWeek)
    }


    @Test
    def date2days2date() {
        date2days2date(1   , Tishrei,  1);
        date2days2date(2   , Tishrei,  1);
        date2days2date(5768, AdarII , 28);
        date2days2date(5769, Nisan  , 14);
    }


    private def date2days2date(yearNumber: Int, monthName: MonthName, dayNumber: Int) {
        val year = Year(yearNumber)
        Assert.assertEquals(yearNumber, year.number)

        val month = year.month(monthName)
        Assert.assertEquals(monthName, month.name)

        val day = month.day(dayNumber)
        Assert.assertEquals(year, day.year)
        Assert.assertEquals(month, day.month)
        Assert.assertEquals(dayNumber, day.dayOfMonth)
    }
}
