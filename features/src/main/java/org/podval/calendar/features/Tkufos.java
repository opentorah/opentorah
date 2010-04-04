package org.podval.calendar.features;

import org.podval.calendar.dates.JewishCalendar;
import org.podval.calendar.dates.JewishDate;
import org.podval.calendar.dates.JewishMonth;


public final class Tkufos {

    // KH 9:3
// @todo if this is uncommented, I get NPE during construction...
//    public final long FIRST_TKUFAS_NISSAN = molad(1, JewishMonth.Nissan) - partsFromDate(7, 9, 642);
    private static long firstTkufasNissan() {
        return JewishCalendar.getInstance().molad(1, JewishMonth.Nissan) - JewishCalendar.partsFromDate(7, 9, 642);
    }


    public static final long YEAR_OF_SHMUEL = JewishCalendar.partsFromDate(365, 6, 0);


    public static final long YEAR_OF_RAV_ADA = JewishCalendar.MONTHS_IN_CYCLE*JewishCalendar.LUNAR_MONTH/19;


    public static JewishDate birkasHachama(final int cycle) {
        // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox
        return JewishCalendar.getInstance().dateFromParts(firstTkufasNissan() + 28*cycle*YEAR_OF_SHMUEL + 12*JewishCalendar.PARTS_IN_HOUR);
    }


    public static JewishDate tkufasNissan(final int year) {
        return JewishCalendar.getInstance(). dateFromParts(firstTkufasNissan() + (year-1)*YEAR_OF_RAV_ADA);
    }
}
