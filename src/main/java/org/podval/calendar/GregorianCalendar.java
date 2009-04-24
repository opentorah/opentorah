package org.podval.calendar;

import java.util.Date;


public final class GregorianCalendar {

    public static final int FIRST_DAY = 1373427;


    public static int daysFromDate(final Date date) {
        final int year = date.getYear();
        final int daysBeforeYear = ((year-1)/4) - ((year-1)/100) + ((year-1)/400);
        final int daysBeforeMonth = (367*date.getMonth() - 362) / 12;
        return FIRST_DAY-1 + daysBeforeYear + daysBeforeMonth + date.getDay();
    }
}
