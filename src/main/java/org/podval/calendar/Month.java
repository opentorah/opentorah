package org.podval.calendar;


public final class Month<M> {

    public Month(final M month, final String name, final int days) {
        this.month = month;
        this.name = name;
        this.days = days;
    }


    @Override
    public String toString() {
        return name;
    }


    public final M month;


    public final String name;


    public final int days;
}
