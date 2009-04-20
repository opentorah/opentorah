package org.podval.calendar;


public final class JewishDate {

//    public enum Month { Tishrei, MarHeshvan, Kislev, Tevet, Shevat, Adar, Adar2, Nissan, Iyar, Sivan, Tamuz, Av, Elul }




    public static JewishDate createFromDays(final int days) {
        return new JewishDate(0, 0, 0, days, 0, 0, 0);
    }


    public static JewishDate createFromParts(final long allParts) {
        return new JewishDate(
            0,
            0,
            0,
            JewishCalendar.daysFromParts(allParts),
            JewishCalendar.hoursFromParts(allParts),
            JewishCalendar.minutesFromParts(allParts),
            JewishCalendar.partsFromParts(allParts));
    }


    public JewishDate(
        final int year,
        final int month,
        final int day,
        final int days,
        final int hours,
        final int minutes,
        final long parts)
    {
        this.year = year;
        this.month = month;
        this.day = day;
        this.days = days;
        this.hours = hours;
        this.minutes = minutes;
        this.parts = parts;
    }


    public int getYear() {
        possiblyCalculateDate();
        return year;
    }


    public int getMonth() {
        possiblyCalculateDate();
        return month;
    }


    public int getDay() {
        possiblyCalculateDate();
        return day;
    }


    public int getDays() {
        if ((days == 0) && !isDateEmpty()) {
            days = calculateDays();
        }

        return days;
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


    private void possiblyCalculateDate() {
        if (isDateEmpty() && (days != 0)) {
            calculateDate();
        }
    }


    private boolean isDateEmpty() {
        return (year == 0);
    }


    public JewishDate getDate() {
        return new JewishDate(getYear(), getMonth(), getDay(), getDays(), 0, 0, 0);
    }


    public JewishDate getTime() {
        return new JewishDate(0, 0, 0, 0, getHours(), getMinutes(), getParts());
    }


    private void calculateDate() {
        // @todo
    }


    private int calculateDays() {
        // @todo !!!
        return 0;
    }


    private final int year;


    private final int month;


    private final int day;


    private /*final*/ int days;


    private final int hours;


    private final int minutes;


    private final long parts;
}
