package org.podval.calendar;

import java.util.List;


public abstract class Calendar<M> {

    public final Date dateFromDate(final int year, final M monthName, final int day) {
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

        return new Date<M>(days, year, month, day);
    }


    public final Date<M> dateFromDays(final int days) {
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

        return new Date(days, year, month, daysInYear+1);
    }


    public final Date<M> dateFromParts(final long parts) {
        final int days = JewishCalendar.daysFromParts(parts);
        return dateFromDays(days).setTime(
            JewishCalendar.hoursFromParts(parts),
            JewishCalendar.minutesFromParts(parts),
            JewishCalendar.partsFromParts(parts)
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


    public static final int dayOfTheWeek(final int days) {
        return (int) (days % 7 + 1);
    }


    public abstract int epoch();


    protected abstract int daysInYearsBeforeYear(final int year);


    public abstract List<Month<M>> getMonths(final int year);


    protected abstract void monthNotFound(final int year, final M monthName);
}
