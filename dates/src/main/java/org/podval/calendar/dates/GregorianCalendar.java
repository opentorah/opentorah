package org.podval.calendar.dates;

import java.util.LinkedList;
import java.util.List;


public final class GregorianCalendar extends Calendar<GregorianMonth, GregorianDate> {

    private static final GregorianCalendar INSTANCE = new GregorianCalendar();


    public static GregorianCalendar getInstance() {
        return INSTANCE;
    }


    public GregorianCalendar() {
        addMonth(new Month<GregorianMonth>(GregorianMonth.January, "January", 31));

        normalMonths.add(new Month<GregorianMonth>(GregorianMonth.February, "February", 28));
        leapMonths.add(new Month<GregorianMonth>(GregorianMonth.February, "February", 29));

        addMonth(new Month<GregorianMonth>(GregorianMonth.March, "March", 31));
        addMonth(new Month<GregorianMonth>(GregorianMonth.April, "April", 30));
        addMonth(new Month<GregorianMonth>(GregorianMonth.May, "May", 31));
        addMonth(new Month<GregorianMonth>(GregorianMonth.June, "June", 30));
        addMonth(new Month<GregorianMonth>(GregorianMonth.July, "July", 31));
        addMonth(new Month<GregorianMonth>(GregorianMonth.August, "August", 31));
        addMonth(new Month<GregorianMonth>(GregorianMonth.September, "September", 30));
        addMonth(new Month<GregorianMonth>(GregorianMonth.October, "October", 31));
        addMonth(new Month<GregorianMonth>(GregorianMonth.November, "November", 30));
        addMonth(new Month<GregorianMonth>(GregorianMonth.December, "December", 31));
    }


    private void addMonth(final Month<GregorianMonth> month) {
        normalMonths.add(month);
        leapMonths.add(month);
    }


    private final List<Month<GregorianMonth>> normalMonths = new LinkedList<Month<GregorianMonth>>();


    private final List<Month<GregorianMonth>> leapMonths = new LinkedList<Month<GregorianMonth>>();


    @Override
    public int epoch() {
        return 1373430;
    }


    @Override
    protected int daysInYearsBeforeYear(final int year) {
        final int y = year-1;
        return 365 * y + y/4 - y/100 + y/400;
    }


    @Override
    public List<Month<GregorianMonth>> getMonths(final int year) {
        return (isLeap(year)) ? leapMonths : normalMonths;
    }


    @Override
    public List<Month<GregorianMonth>> getMonthsSimple(final int year) {
        return getMonths(year);
    }


    public boolean isLeap(final int year) {
        return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
    }


    @Override
    protected void monthNotFound(final int year, final GregorianMonth monthName) {
        throw new Error("?");
    }


    @Override
    protected final GregorianDate createDate(final int days, final int year, final Month<GregorianMonth> month, final int day) {
        return new GregorianDate(days, year, month, day);
    }
}
