package org.podval.calendar;


public final class Years {

    public static final int YEARS_IN_CYCLE = 19;


    private static final boolean[] IS_LEAP = new boolean[YEARS_IN_CYCLE];


    static {
        IS_LEAP[3] = true;
        IS_LEAP[6] = true;
        IS_LEAP[8] = true;
        IS_LEAP[11] = true;
        IS_LEAP[14] = true;
        IS_LEAP[17] = true;
        IS_LEAP[0] = true; // 19th year
    }


    public static boolean isLeap(final int year) {
        return IS_LEAP[yearInCycle(year)];
    }


    public static int cycleNumber(final int year) {
        return year / YEARS_IN_CYCLE + 1;
    }


    public static int yearInCycle(final int year) {
        return year % YEARS_IN_CYCLE;
    }


    public static int monthsInYear(final int year) {
        return isLeap(year) ? 13 : 12;
    }


    private static final int[] MONTHS_BEFORE_YEAR_IN_CYCLE = new int[YEARS_IN_CYCLE+2];


    static {
        MONTHS_BEFORE_YEAR_IN_CYCLE[0] = 0;
        MONTHS_BEFORE_YEAR_IN_CYCLE[1] = 0;
        for (int year = 2; year <= YEARS_IN_CYCLE+1; year++) {
            MONTHS_BEFORE_YEAR_IN_CYCLE[year] = MONTHS_BEFORE_YEAR_IN_CYCLE[year-1] + monthsInYear(year-1);
        }
    }


    public static int monthsBeforeYearInCycle(final int yearInCycle) {
        return MONTHS_BEFORE_YEAR_IN_CYCLE[yearInCycle];
    }


    public static final int MONTHS_IN_CYCLE = monthsBeforeYearInCycle(YEARS_IN_CYCLE+1);
}
