package org.podval.calendar;

import java.util.List;


public abstract class Calendar<M, D extends Date<M,D>> {

    public final D dateFromDate(final int year, final M monthName, final int day) {
        Month<M> month = null;
        int daysBeforeMonth = 0;
        for (final Month<M> m : getMonths(year)) {
            if (m.month == monthName) {
                month = m;
                break;
            }
            daysBeforeMonth += m.days;
        }

        if (month == null) {
            monthNotFound(year, monthName);
        }

        if (day < 1) {
            throw new IllegalArgumentException("Day number must be no less than 1");
        }

        if (day > month.days) {
            throw new IllegalArgumentException("No such day in the month");
        }

        final int days = epoch() + daysInYearsBeforeYear(year) + daysBeforeMonth + day - 1;

        return createDate(days, year, month, day);
    }


    protected abstract D createDate(final int days, final int year, final Month<M> month, final int day);


    public final D dateFromDays(final int days) {
        final int year = yearDayIsIn(days-epoch());

        int daysInYear = days - daysInYearsBeforeYear(year) - epoch();

        Month<M> month = null;
        for (final Month<M> m : getMonths(year)) {
            final int daysInMonth = m.days;
            if (daysInYear < daysInMonth) {
                month = m;
                break;
            }
            daysInYear -= daysInMonth;
        }

        return createDate(days, year, month, daysInYear+1);
    }


    public final int monthNumber(final int year, final M month) {
        boolean found = false;
        int result = 1;
        for (final Month<M> m : getMonthsSimple(year)) {
            if (m.month == month) {
                found = true;
                break;
            }
            result++;
        }

        if (!found) {
            monthNotFound(year, month);
        }

        return result;
    }


    public final D dateFromParts(final long parts) {
        final int days = daysFromParts(parts);
        return dateFromDays(days).setTime(
            hoursFromParts(parts),
            minutesFromParts(parts),
            partsFromParts(parts)
        );
    }


    private int yearDayIsIn(final int days) {
        int result = (4 * days / (4 * 365 + 1)) - 1;

        while (true) {
            final int daysBeforeNextYear = daysInYearsBeforeYear(result + 1);
            if (daysBeforeNextYear > days) {
                break;
            }
            result++;
        }

        return result;
    }


    public static final long PARTS_IN_HOUR = 1080;


    public static final int MINUTES_IN_HOUR = 60;


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


    public static int dayOfTheWeek(final int days) {
        return (int) (days % 7 + 1);
    }


    public abstract int epoch();


    protected abstract int daysInYearsBeforeYear(final int year);


    public abstract List<Month<M>> getMonths(final int year);


    public abstract List<Month<M>> getMonthsSimple(final int year);


    protected abstract void monthNotFound(final int year, final M monthName);
}
