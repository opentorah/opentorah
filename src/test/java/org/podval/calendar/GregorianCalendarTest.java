package org.podval.calendar;

import org.junit.Test;
import org.junit.Assert;

import java.util.Date;


public class GregorianCalendarTest {

    @Test
    public void gregorian2jewish() {
        gregorian2jewish(1964, 10, 30, 5725, JewishMonth.MarHeshvan, 24);
    }


    private void gregorian2jewish(
        final int year,
        final int month,
        final int day,
        final int jYear,
        final JewishMonth jMonth,
        final int jDay)
    {
    }
}
