package org.podval.calendar;


public abstract class Calendar<M> {

    private static final JewishCalendar jewishCalendar = new JewishCalendar();


    public static JewishCalendar getJewish() {
        return jewishCalendar;
    }


    public final Date dateFromDate(final int year, final M monthName, final int day) {
        Month<M> month = null;
        for (final Month<M> m : getMonths(year)) {
            if (m.month == monthName) {
                month = m;
                break;
            }
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

        return new Date<M>(year, month, day);
    }


    public final int daysFromDate(final Date<M> date) {
        final int year = date.getYear();
        return epoch() + daysInYearsBeforeYear(year) + daysBeforeMonth(year, date.getMonth()) + date.getDay() - 1;
    }


    public final int daysBeforeMonth(final int year, final Month<M> month) {
        int result = 0;

        for (final Month<M> m : getMonths(year)) {
            if (m == month) {
                break;
            }
            result += m.days;
        }

        return result;
    }


    public final Date<M> dateFromDays(final int days) {
        final int year = yearDayIsIn(days);

        int daysInYear = days - daysInYearsBeforeYear(year);

        Month<M> month = null;
        for (final Month<M> m : getMonths(year)) {
            final int daysInMonth = m.days;
            if (daysInYear < daysInMonth) {
                month = m;
                break;
            }
            daysInYear -= daysInMonth;
        }

        return new Date(year, month, daysInYear+1);
    }


    public final int dayOfTheWeek(final int days) {
        return (int) ((days + epochDayOfTheWeek()) % 7 + 1);
    }


    public abstract int epoch();


    public abstract int epochDayOfTheWeek();


    protected abstract int daysInYearsBeforeYear(final int year);


    protected abstract int yearDayIsIn(final int days);


    public abstract Month<M>[] getMonths(final int year);


    protected abstract void monthNotFound(final int year, final M monthName);
}
