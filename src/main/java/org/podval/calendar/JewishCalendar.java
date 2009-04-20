package org.podval.calendar;


public final class JewishCalendar {

    public static long PARTS_IN_HOUR = 1080;


    public static int MINUTES_IN_HOUR = 60;


    public static final long PARTS_IN_MINUTE = PARTS_IN_HOUR / MINUTES_IN_HOUR;


    public static final int HOURS_IN_DAY = 24;


    public static final long PARTS_IN_DAY = HOURS_IN_DAY * PARTS_IN_HOUR;


    public static int daysFromParts(final long parts) {
        return (int) (parts / PARTS_IN_DAY);
    }


    public static int hoursFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_DAY) / PARTS_IN_HOUR);
    }


    public static int minutesFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_HOUR) / PARTS_IN_MINUTE);
    }


    public static int partsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_MINUTE);
    }


    public static int hoursMinutesAndPartsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_DAY);
    }


    public static int dayOfTheWeek(final int days) {
        return (int) ((days + 5) % 7 + 1);
    }


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


    private static final int[] MONTHS_BEFORE_YEAR_IN_CYCLE = new int[YEARS_IN_CYCLE+1];


    static {
        MONTHS_BEFORE_YEAR_IN_CYCLE[0] = 0;
        for (int year = 1; year <= YEARS_IN_CYCLE; year++) {
            MONTHS_BEFORE_YEAR_IN_CYCLE[year] = MONTHS_BEFORE_YEAR_IN_CYCLE[year-1] + monthsInYear(year-1);
        }
    }


    public static int monthsBeforeYearInCycle(final int yearInCycle) {
        return MONTHS_BEFORE_YEAR_IN_CYCLE[yearInCycle-1];
    }


    public static final int MONTHS_IN_CYCLE = monthsBeforeYearInCycle(YEARS_IN_CYCLE+1);


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    public static final long LUNAR_MONTH = 29 * PARTS_IN_DAY + 12 * PARTS_IN_HOUR + 793;


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    // Our epoch is the 6th day, creation of Man, the first Rosh HaShono
    public static final long FIRST_MOLAD = -5*PARTS_IN_DAY + (1*PARTS_IN_DAY + 5*PARTS_IN_HOUR + 204);


    public static long molad(final int year, final int month) {
        final int monthsInPreviousCycles = (cycleNumber(year)-1) * MONTHS_IN_CYCLE;
        final int monthInPreviousYears = monthsBeforeYearInCycle(yearInCycle(year));
        final int moladNumber = monthsInPreviousCycles + monthInPreviousYears + (month - 1);
        return FIRST_MOLAD + LUNAR_MONTH * moladNumber;
    }


    public static int dayOfRoshHaShono(final int year) {
        final long molad = molad(year, 1);
        int result = daysFromParts(molad);

        final int moladDayOfTheWeek = dayOfTheWeek(result);
        if (isADU(moladDayOfTheWeek)) {
            // KH 7:1
            result++;
        } else if (hoursFromParts(molad) >= 18) {
            // KH 7:2
            result++;
            if (isADU(moladDayOfTheWeek % 7 + 1)) {
                // KH 7:3
                result++;
            }
        } else if ((moladDayOfTheWeek == 3) && !isLeap(year) && notEarlierInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && isLeap(year-1) && notEarlierInTheDayThan(molad, 15, 589)) {
            // KH 7:5
            result += 1;
        }

        return result;
    }


    private static boolean isADU(final int dayOfTheWeek) {
        return (dayOfTheWeek == 1) || (dayOfTheWeek == 4) || (dayOfTheWeek == 6);
    }


    private static boolean notEarlierInTheDayThan(final long molad, final int hour, final int parts) {
        return hoursMinutesAndPartsFromParts(molad) >= hour * PARTS_IN_HOUR + parts;
    }


    public enum YearKind { LACKING, ORDERLY, FULL }


    // KH 8:5,6
    private static final int[] DAYS_IN_MONTH = {
        30, // Tishri
        29, // MarHeshvan // or 30
        30, // Kislev     // or 29
        29, // Tevet
        30, // Shevat
        30, // Adar I
        29, // Adar [II]
        30, // Nissan
        29, // Iyyar
        30, // Sivan
        29, // Tammuz
        30, // Av
        29  // Elul
    };


    public static int daysInMonth(final int year, final YearKind yearKind, final int number) {
        int correctedNumber = number;

        if (!isLeap(year) && (correctedNumber >= 5)) {
            correctedNumber++;
        }
        int result = DAYS_IN_MONTH[correctedNumber-1];
        if ((correctedNumber == 2) && (yearKind == YearKind.LACKING)) {
            result--;
        } else if ((correctedNumber == 1) && (yearKind == YearKind.FULL)) {
            result++;
        }

        return result;
    }


    public static int daysBeforeMonth(final int year, final int month) {
        int result = 0;
        final YearKind yearKind = yearKind(year);
        for (int m = 1; m <month; m++) {
            result += daysInMonth(year, yearKind, m);
        }
        return result;
    }


    // KH 8:7,8
    public static YearKind yearKind(final int year) {
        final YearKind result;

        final int roshHaShono = dayOfRoshHaShono(year);
        final int nextRoshHaShono = dayOfRoshHaShono(year+1);
        final int difference = dayOfTheWeek(nextRoshHaShono) - dayOfTheWeek(roshHaShono) - 1;

        if (!isLeap(year)) {
            if (difference == 2) {
                result = YearKind.LACKING;
            } else if (difference == 3) {
                result = YearKind.ORDERLY;
            } else if (difference == 4) {
                result = YearKind.FULL;
            } else {
                throw new Error();
            }
        } else {
            if (difference == 4) {
                result = YearKind.LACKING;
            } else if (difference == 5) {
                result = YearKind.ORDERLY;
            } else if (difference == 6) {
                result = YearKind.FULL;
            } else {
                throw new Error();
            }
        }

        return result;
    }


    public static int daysFromYearMonthDay(final int year, final int month, final int day) {
        return dayOfRoshHaShono(year) + daysBeforeMonth(year, month) + day - 1;
    }


    public static void main(final String[] args) {
        final long molad = molad(5769, 1);
        final int days = daysFromParts(molad);
        System.out.println(
            "Molad on " + dayOfTheWeek(days) +
            " at " + minutesFromParts(molad) + " min " +
            " and " + partsFromParts(molad) + " parts");
    }
}
