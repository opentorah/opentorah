package org.podval.calendar;


public final class GregorianCalendar extends Calendar<GregorianMonth> {

    @Override
    public int epoch() {
        return 1373427;
    }


    @Override
    public int epochDayOfTheWeek() {
        return 3; // @todo
    }


    @Override
    protected int daysInYearsBeforeYear(final int year) {
        final int y = year-1;
        return 365 * y + y/4 - y/100 + y/400;
    }


    @Override
    protected int yearDayIsIn(final int days) {
        throw new UnsupportedOperationException();
    }


    @Override
    public Month<GregorianMonth>[] getMonths(final int year) {
        throw new UnsupportedOperationException();
    }


    @Override
    protected void monthNotFound(final int year, final GregorianMonth monthName) {
        throw new UnsupportedOperationException();
    }


//    public int daysFromDate(final Date date) {
//        final int year = date.getYear();
//        final int daysBeforeMonth = (367*date.getMonth() - 362) / 12;
//        return FIRST_DAY-1 + daysBeforeYear(year) + daysBeforeMonth + date.getDay();
//    }
}
