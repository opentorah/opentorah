package org.podval.calendar;


public class Date<M> {

    public Date(
        final Calendar<M> calendar,
        final int days,
        final int year,
        final Month<M> month,
        final int day)
    {
        this(calendar, days, year, month, day, 0, 0, 0);
    }


    public Date(
        final Calendar<M> calendar,
        final int days,
        final int year,
        final Month<M> month,
        final int day,
        final int hour,
        final int minute,
        final int parts)
    {
        this.calendar = calendar;
        this.days = days;
        this.year = year;
        this.month = month;
        this.day = day;
        this.hour = hour;
        this.minute= minute;
        this.parts = parts;
    }


    public int getDays() {
        return days;
    }


    public int getYear() {
        return year;
    }


    public Month<M> getMonth() {
        return month;
    }


    public int getDay() {
        return day;
    }


    @Override
    public String toString() {
        return getYear() + " " + getMonth() + " " + getDay();
    }


    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof Date)) return false;

        final Date other = (Date) o;
        return (getMonth().month.getClass().isAssignableFrom(other.getMonth().month.getClass())) ?
            (getYear() == other.getYear()) && (getMonth() == getMonth()) && (getDay() == other.getDay()) :
            (getDays() == other.getDays());
    }


    @Override
    public int hashCode() {
        int hash = 3;
        hash = 37 * hash + this.days;
        return hash;
    }


    private final Calendar<M> calendar;


    private final int days;


    private final int year;


    private final Month<M> month;


    private final int day;

    
    private final int hour;


    private final int minute;


    private final int parts;
}
