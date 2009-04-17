/* $Id: HebrewCalendar.java,v 1.1 2006/07/20 15:06:29 dub Exp $ */
package org.podval.moon;

public final class HebrewCalendar {

    public static final long DAY = 24 * 1080;


    public static long HOUR = 1080;


    public static final long MINUTE = HOUR / 60;


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    public static final long LUNAR_MONTH = 29 * DAY + 12 * HOUR + 793;


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KN 6:8)
    public static final long FIRST_MOLAD = -5*DAY + (1*DAY + 5*HOUR + 204);


    public static final boolean[] IS_LEAP = new boolean[19];


    static {
        IS_LEAP[3] = true;
        IS_LEAP[6] = true;
        IS_LEAP[8] = true;
        IS_LEAP[11] = true;
        IS_LEAP[14] = true;
        IS_LEAP[17] = true;
        IS_LEAP[0] = true;
    }


    public static boolean isLeap(int year) {
        // Leap years: 3,6,8,11,14,17 and 19 of the 19-year Metonic cycle
        return IS_LEAP[year % 19];
    }


    public static int numberOfMonths(int year) {
        return isLeap(year) ? 13 : 12;
    }


    public static final int[] MONTH_BEFORE_YEAR = new int[21];


    static {
        MONTH_BEFORE_YEAR[0] = 0;
        MONTH_BEFORE_YEAR[1] = 0;
        for (int year = 2; year <= 20; year++) {
            MONTH_BEFORE_YEAR[year] = MONTH_BEFORE_YEAR[year-1] + numberOfMonths(year-1);
        }
    }



    public static final int MONTH_IN_19_YEAR_CYCLE = MONTH_BEFORE_YEAR[20];


    public static long moladTime(int year, int month) {
        int moladNumber = (year / 19) * MONTH_IN_19_YEAR_CYCLE + MONTH_BEFORE_YEAR[year % 19] + (month - 1);
        return FIRST_MOLAD + LUNAR_MONTH * moladNumber;
    }


    public static int dayOfWeek(final long when) {
        return (int) (((when / DAY) + 5) % 7 + 1);
    }


    
    public static void main(final String[] args) {
        final long molad = moladTime(5769, 1);
//        final long molad = moladTime(1, 1);
        final long minutesAndParts = (molad % DAY) % HOUR;
        final long minutes = minutesAndParts / MINUTE;
        final long parts = minutesAndParts % MINUTE;
        System.out.println("Molad on " + dayOfWeek(molad) + " at " + minutes + " min " + parts + " parts (" + minutesAndParts + ")");
    }
}
