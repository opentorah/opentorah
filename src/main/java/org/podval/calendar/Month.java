package org.podval.calendar;


public final class Month {

    public Month(final JewishMonth month, final String name, final int days) {
        this.month = month;
        this.name = name;
        this.days = days;
    }


    public final JewishMonth month;


    public final String name;


    public final int days;
}
