package org.podval.calendar;


public final class JewishDate {

//    public enum Month { Tishrei, MarHeshvan, Kislev, Tevet, Shevat, Adar, Adar2, Nissan, Iyar, Sivan, Tamuz, Av, Elul }


    public static JewishDate createFromParts(final long allParts) {
        return new JewishDate(
            0,
            0,
            0,
            JewishCalendar.hoursFromParts(allParts),
            JewishCalendar.minutesFromParts(allParts),
            JewishCalendar.partsFromParts(allParts));
    }


    public JewishDate(
        final int year,
        final int month,
        final int day)
    {
        this(year, month, day, 0, 0, 0);
    }


    public JewishDate(
        final int year,
        final int month,
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


    public int getMonth() {
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
        return new JewishDate(0, 0, 0, getHours(), getMinutes(), getParts());
    }


    public String toString() {
        return JewishCalendar.monthName(getYear(), getMonth()) + " " + getDay() + ", " + getYear();
    }


    private final int year;


    private final int month;


    private final int day;


    private final int hours;


    private final int minutes;


    private final long parts;
}
