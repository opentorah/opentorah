package org.podval.calendar;


public class Date<M> {

    public Date(final int year, final Month<M> month, final int day) {
        this.year = year;
        this.month = month;
        this.day = day;
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
        return getMonth() + " " + getDay() + ", " + getYear();
    }


    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof Date)) return false;
        final Date other = (Date) o;

        return (getYear() == other.getYear()) && getMonth().equals(other.getMonth()) && (getDay() == other.getDay());
    }


    @Override
    public int hashCode() {
        int hash = 3;
        hash = 61 * hash + this.year;
        hash = 61 * hash + (this.month != null ? this.month.hashCode() : 0);
        hash = 61 * hash + this.day;
        return hash;
    }


    private final int year;


    private final Month<M> month;


    private final int day;
}
