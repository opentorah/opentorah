package org.podval.calendar;

import java.util.List;


public abstract class Calendar<M> {

    private static final JewishCalendar jewishCalendar = new JewishCalendar();


    private static final GregorianCalendar gregorianCalendar = new GregorianCalendar();


    public static JewishCalendar getJewish() {
        return jewishCalendar;
    }


    public static GregorianCalendar getGregorian() {
        return gregorianCalendar;
    }


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

        final int days = epoch() + daysInYearsBeforeYear(year) + daysBeforeMonth + day;

        return new Date<M>(this, days, year, month, day);
    }


    public final Date<M> dateFromDays(final int days) {
        final int daysOfEpoch = days - epoch();

        final int year = yearDayIsIn(daysOfEpoch);

        int daysInYear = daysOfEpoch - daysInYearsBeforeYear(year);

        Month<M> month = null;
        for (final Month<M> m : getMonths(year)) {
            final int daysInMonth = m.days;
            if (daysInYear < daysInMonth) {
                month = m;
                break;
            }
            daysInYear -= daysInMonth;
        }

        return new Date(this, days, year, month, daysInYear);
    }


    public final int dayOfTheWeek(final int days) {
        return (int) (days % 7 + 1);
    }


    public abstract int epoch();


    protected abstract int daysInYearsBeforeYear(final int year);


    protected abstract int yearDayIsIn(final int days);


    public abstract List<Month<M>> getMonths(final int year);


    protected abstract void monthNotFound(final int year, final M monthName);
}
