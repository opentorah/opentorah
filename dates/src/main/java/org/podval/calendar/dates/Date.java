package org.podval.calendar.dates;


public abstract class Date<M, D extends Date<M, D>> {

    protected Date(
        final int days,
        final int year,
        final Month<M> month,
        final int day)
    {
        this(days, year, month, day, 0, 0, 0);
    }


    protected Date(
        final int days,
        final int year,
        final Month<M> month,
        final int day,
        final int hour,
        final int minute,
        final int parts)
    {
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


    public int getDayOfTheWeek() {
        return Calendar.dayOfTheWeek(getDays());
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


    public int getHour() {
        return hour;
    }


    public int getMinute() {
        return minute;
    }


    public int getParts() {
        return parts;
    }


    public D setTime(final int hour, final int minute, final int parts) {
        return create(getDays(), getYear(), getMonth(), getDay(), hour, minute, parts);
    }


    public D getDate() {
        return create(getDays(), getYear(), getMonth(), getDay(), 0, 0, 0);
    }


    public D getTime() {
        return create(0, 0, null, 0, getHour(), getMinute(), getParts());
    }


    protected abstract D create(final int days, final int year, final Month<M> month, final int day, final int hour, final int minute, final int parts);


    @Override
    public String toString() {
        return getYear() + " " + getMonth() + " " + getDay() + " " + getHour() + ":" + getMinute() + ":" + getParts();
    }


    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof Date)) return false;

        final Date other = (Date) o;
        if (!getMonth().month.getClass().isAssignableFrom(other.getMonth().month.getClass())) {
            throw new IllegalArgumentException("Dates must be of the same type");
        }

        return (getYear() == other.getYear()) && (getMonth() == getMonth()) && (getDay() == other.getDay());
    }


    @Override
    public int hashCode() {
        int hash = 3;
        hash = 37 * hash + this.days;
        return hash;
    }


    private final int days;


    private final int year;


    private final Month<M> month;


    private final int day;

    
    private final int hour;


    private final int minute;


    private final int parts;
}
