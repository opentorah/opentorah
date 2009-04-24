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


    public static long molad(final int year, final int month) {
        final int monthsInPreviousCycles = (cycleNumber(year)-1) * MONTHS_IN_CYCLE;
        final int monthInPreviousYears = monthsBeforeYearInCycle(yearInCycle(year));
        final int moladNumber = monthsInPreviousCycles + monthInPreviousYears + (month - 1);
        return Days.FIRST_MOLAD + Days.LUNAR_MONTH * moladNumber;
    }


    public static int dayOfRoshHaShono(final int year) {
        if (year == 1) {
            return 1;
        }

        final long molad = molad(year, 1);
        int result = Days.daysFromParts(molad);

        final int moladDayOfTheWeek = Days.dayOfTheWeek(result);
        if (isADU(moladDayOfTheWeek)) {
            // KH 7:1
            result++;
        } else if (Days.hoursFromParts(molad) >= 18) {
            // KH 7:2
            result++;
            if (isADU(moladDayOfTheWeek % 7 + 1)) {
                // KH 7:3
                result++;
            }
        } else if ((moladDayOfTheWeek == 3) && !Years.isLeap(year) && Days.notEarlierInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && Years.isLeap(year-1) && Days.notEarlierInTheDayThan(molad, 15, 589)) {
            // KH 7:5
            result += 1;
        }

        return result;
    }


    private static boolean isADU(final int dayOfTheWeek) {
        return (dayOfTheWeek == 1) || (dayOfTheWeek == 4) || (dayOfTheWeek == 6);
    }


    public static int yearLength(final int year) {
        return dayOfRoshHaShono(year+1) - dayOfRoshHaShono(year);
    }


    public static int yearDayIsIn(final int days) {
        int result = (4 * days / (4 * 365 + 1));

        while (true) {
            if (dayOfRoshHaShono(result + 1) > days) {
                break;
            }
            result++;
        }

        return result;
    }
}
