package org.podval.calendar;


public final class JewishCalendar {

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


    public static long molad(final int year, final int month) {
        final int monthsInPreviousCycles = (cycleNumber(year)-1) * MONTHS_IN_CYCLE;
        final int monthInPreviousYears = monthsBeforeYearInCycle(yearInCycle(year));
        final int moladNumber = monthsInPreviousCycles + monthInPreviousYears + (month - 1);
        return Units.FIRST_MOLAD + Units.LUNAR_MONTH * moladNumber;
    }


    public static int dayOfRoshHaShono(final int year) {
        final long molad = molad(year, 1);
        int result = Units.daysFromParts(molad);

        final int moladDayOfTheWeek = dayOfTheWeek(result);
        if (isADU(moladDayOfTheWeek)) {
            // KH 7:1
            result++;
        } else if (Units.hoursFromParts(molad) >= 18) {
            // KH 7:2
            result++;
            if (isADU(moladDayOfTheWeek % 7 + 1)) {
                // KH 7:3
                result++;
            }
        } else if ((moladDayOfTheWeek == 3) && !isLeap(year) && Units.notEarlierInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && isLeap(year-1) && Units.notEarlierInTheDayThan(molad, 15, 589)) {
            // KH 7:5
            result += 1;
        }

        return result;
    }


    private static boolean isADU(final int dayOfTheWeek) {
        return (dayOfTheWeek == 1) || (dayOfTheWeek == 4) || (dayOfTheWeek == 6);
    }


    // KH 8:7,8
    public static YearKind yearKind(final int year) {
        final YearKind result;

        final int roshHaShono = dayOfRoshHaShono(year);
        final int nextRoshHaShono = dayOfRoshHaShono(year+1);
        final int difference = dayOfTheWeek(nextRoshHaShono) - dayOfTheWeek(roshHaShono) - 1;

        if (!isLeap(year)) {
            if (difference == 2) {
                result = YearKind.SHORT;
            } else if (difference == 3) {
                result = YearKind.REGULAR;
            } else if (difference == 4) {
                result = YearKind.FULL;
            } else {
                throw new Error();
            }
        } else {
            if (difference == 4) {
                result = YearKind.SHORT;
            } else if (difference == 5) {
                result = YearKind.REGULAR;
            } else if (difference == 6) {
                result = YearKind.FULL;
            } else {
                throw new Error();
            }
        }

        return result;
    }


    public static int daysFromDate(final int year, final JewishMonth month, final int day) {
        int daysBeforeMonth = 0;
        for (final Month m : getMonths(year)) {
            if (m.month == month) {
                break;
            }
            daysBeforeMonth += m.days;
        }
        return dayOfRoshHaShono(year) + daysBeforeMonth + day - 1;
    }


    public static int daysFromDate(final JewishDate date) {
        return daysFromDate(date.getYear(), date.getMonth(), date.getDay());
    }


    public static JewishDate dateFromDays(final int days) {
        int year = (4 * days / (4 * 365 + 1));
        while (true) {
            if (dayOfRoshHaShono(year+1) > days) {
                break;
            }
            year++;
        }

        int daysInYear = days - dayOfRoshHaShono(year);

        Month month = null;
        for (final Month m : getMonths(year)) {
            final int daysInMonth = m.days;
            if (daysInYear < daysInMonth) {
                month = m;
                break;
            }
            daysInYear -= daysInMonth;
        }

        return new JewishDate(year, month.month, daysInYear);
    }


    private static Month[] getMonths(final int year) {
        return Months.getMonths(isLeap(year), yearKind(year));
    }


    public static void main(final String[] args) {
//        final long molad = molad(5769, 1);
//        final int days = daysFromParts(molad);
//        System.out.println(
//            "Molad on " + dayOfTheWeek(days) +
//            " at " + minutesFromParts(molad) + " min " +
//            " and " + partsFromParts(molad) + " parts");
        System.out.println(new JewishDate(1, JewishMonth.Tishri, 1));
    }
}
