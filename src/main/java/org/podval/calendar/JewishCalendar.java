package org.podval.calendar;


public final class JewishCalendar {

    public static JewishDate dateFromDate(final int year, final JewishMonth monthName, final int day) {
        Month month = null;
        for (final Month m : Months.getMonths(year)) {
            if (m.month == monthName) {
                month = m;
                break;
            }
        }

        if (month == null) {
            throw new IllegalArgumentException((monthName == JewishMonth.Adar) ?
                "No Adar in a leap year" :
                "No numbered Adar in a non-leap year");
        }

        if (day < 1) {
            throw new IllegalArgumentException("Day number must be no less than 1");
        }

        if (day > month.days) {
            throw new IllegalArgumentException("No such day in the month");
        }

        return new JewishDate(year, month, day);
    }


    public static int daysFromDate(final JewishDate date) {
        final int year = date.getYear();
        final Month month = date.getMonth();

        int daysBeforeMonth = 0;
        for (final Month m : Months.getMonths(year)) {
            if (m == month) {
                break;
            }
            daysBeforeMonth += m.days;
        }

        return Years.dayOfRoshHaShono(year) + daysBeforeMonth + date.getDay() - 1;
    }


    public static JewishDate dateFromDays(final int days) {
        final int year = Years.yearDayIsIn(days);

        int daysInYear = days - Years.dayOfRoshHaShono(year);

        Month month = null;
        for (final Month m : Months.getMonths(year)) {
            final int daysInMonth = m.days;
            if (daysInYear < daysInMonth) {
                month = m;
                break;
            }
            daysInYear -= daysInMonth;
        }

        return new JewishDate(year, month, daysInYear+1);
    }
}
