package org.podval.calendar;


public final class JewishDate {

    public static JewishDate createFromParts(final long allParts) {
        return new JewishDate(
            0,
            null,
            0,
            Days.hoursFromParts(allParts),
            Days.minutesFromParts(allParts),
            Days.partsFromParts(allParts));
    }


    public JewishDate(
        final int year,
        final Month month,
        final int day)
    {
        this(year, month, day, 0, 0, 0);
    }


    public JewishDate(
        final int year,
        final Month month,
        final int day,
        final int hours,
        final int minutes,
        final long parts)
    {
        this.year = year;
        this.month = month;
        this.day = day;
        this.hours = hours;
        this.minutes = minutes;
        this.parts = parts;
    }


    public int getYear() {
        return year;
    }


    public Month getMonth() {
        return month;
    }


    public int getDay() {
        return day;
    }


    public int getHours() {
        return hours;
    }


    public int getMinutes() {
        return minutes;
    }


    public long getParts() {
        return parts;
    }


    public JewishDate getDate() {
        return new JewishDate(getYear(), getMonth(), getDay(), 0, 0, 0);
    }


    public JewishDate getTime() {
        return new JewishDate(0, null, 0, getHours(), getMinutes(), getParts());
    }


    @Override
    public String toString() {
        return getMonth() + " " + getDay() + ", " + getYear();
    }


    @Override
    public boolean equals(Object o) {
        if (!(o instanceof JewishDate)) return false;
        final JewishDate other = (JewishDate) o;
        return (getYear() == other.getYear()) && (getMonth() == other.getMonth()) && (getDay() == other.getDay());
    }


    @Override
    public int hashCode() {
        int hash = 5;
        hash = 89 * hash + this.year;
        hash = 89 * hash + (this.month != null ? this.month.hashCode() : 0);
        hash = 89 * hash + this.day;
        hash = 89 * hash + this.hours;
        hash = 89 * hash + this.minutes;
        hash = 89 * hash + (int) (this.parts ^ (this.parts >>> 32));
        return hash;
    }


    private final int year;


    private final Month month;


    private final int day;


    private final int hours;


    private final int minutes;


    private final long parts;
}
