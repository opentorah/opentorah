package org.podval.calendar;


public final class JewishCalendar {

    public static int dayOfTheWeek(final int days) {
        return (int) ((days + 5) % 7 + 1);
    }


    public static long molad(final int year, final int month) {
        final int monthsInPreviousCycles = (Years.cycleNumber(year)-1) * Years.MONTHS_IN_CYCLE;
        final int monthInPreviousYears = Years.monthsBeforeYearInCycle(Years.yearInCycle(year));
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
        } else if ((moladDayOfTheWeek == 3) && !Years.isLeap(year) && Units.notEarlierInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && Years.isLeap(year-1) && Units.notEarlierInTheDayThan(molad, 15, 589)) {
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

        if (!Years.isLeap(year)) {
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


    public static JewishDate dateFromDate(final int year, final JewishMonth monthName, final int day) {
        final boolean isLeap = Years.isLeap(year);

        if (isLeap) {
            if (monthName == JewishMonth.Adar) {
                throw new IllegalArgumentException("No Adar in a leap year");
            }
        } else {
            if ((monthName == JewishMonth.AdarI) || (monthName == JewishMonth.AdarII)) {
                throw new IllegalArgumentException("No numbered Adar in a non-leap year");
            }
        }

        Month month = null;
        for (final Month m : getMonths(year)) {
            if (m.month == monthName) {
                month = m;
                break;
            }
        }

        if (day < 1) {
            throw new IllegalArgumentException("Day number must be no less than 1");
        }

        if (day > month.days) {
            throw new IllegalArgumentException("No such day in the month");
        }

        return new JewishDate(year, month, day);
    }


    public static int daysFromDate(final int year, final Month month, final int day) {
        int daysBeforeMonth = 0;
        for (final Month m : getMonths(year)) {
            if (m == month) {
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

        return new JewishDate(year, month, daysInYear);
    }


    private static Month[] getMonths(final int year) {
        return Months.getMonths(Years.isLeap(year), yearKind(year));
    }


    public static void main(final String[] args) {
//        final long molad = molad(5769, 1);
//        final int days = daysFromParts(molad);
//        System.out.println(
//            "Molad on " + dayOfTheWeek(days) +
//            " at " + minutesFromParts(molad) + " min " +
//            " and " + partsFromParts(molad) + " parts");
        System.out.println(dateFromDate(1, JewishMonth.Tishri, 1));
    }
}
