package org.podval.ical;

import org.podval.calendar.Date;
import org.podval.calendar.GregorianMonth;
import org.podval.calendar.GregorianCalendar;

import java.io.OutputStream;
import java.io.PrintStream;


public final class ICalWriter {

    public ICalWriter(final OutputStream os) {
        this.out = new PrintStream(os);
    }


    public void beginCalendar(final String prodId, final String name, final String description) {
        println("BEGIN", "VCALENDAR");
        println("PRODID", prodId);
        println("VERSION", "2.0");
        println("CALSCALE", "GREGORIAN");
        println("METHOD", "PUBLISH");

        if (name != null) {
            println("X-WR-CALNAME", name);
        }

        if (description != null) {
            println("X-WR-CALDESC", description);
        }
    }


    public void endCalendar() {
        println("END", "VCALENDAR");
    }


    public void beginEvent() {
        beginEvent(true);
    }


    public void beginEvent(final boolean transparent) {
        println("BEGIN", "VEVENT");
        println("CLASS", "PUBLIC");
        println("STATUS", "CONFIRMED");
        println("TRANSP", transparent ? "TRANSPARENT" : "OPAQUE");
    }


    public void writeSummary(final String summary) {
        println("SUMMARY", summary);
    }


    public void writeFullDayDuration(final Date<GregorianMonth> date) {
        println("DTSTART;VALUE=DATE", toString(date));
/////        println("DTEND;VALUE=DATE", toString(date.next()));
        println("DURATION", "P1D");
    }


    private String toString(final Date<GregorianMonth> date) {
        final StringBuffer result = new StringBuffer();
        result.append(date.getYear());
        append2digits(result, GregorianCalendar.getInstance().monthNumber(date.getYear(), date.getMonth().month));
        append2digits(result, date.getDay());

        return result.toString();
    }


    private void append2digits(final StringBuffer buf, final int what) {
        if (what < 10) {
            buf.append("0");
        }
        buf.append(what);
    }


//    public void beginEvent() {
//        println("CATEGORIES", "Holidays");
//        println("URL;VALUE=URI", "http://lwhjsdgfjhf");
//        println("DTSTAMP", "2061121T044202Z");
//        println("UID", "asdhjgd-wjks=-f");
//    }


    public void addGoggleContent(
        final String title,
        final String icon,
        final String url,
        final int width,
        final int height)
    {
        println("X-GOOGLE-CALENDAR-CONTENT-TITLE", title);
        println("X-GOOGLE-CALENDAR-CONTENT-ICON", icon);
        println("X-GOOGLE-CALENDAR-CONTENT-URL", url);
        println("X-GOOGLE-CALENDAR-CONTENT-TYPE", "text/html"); // can be image/*
        println("X-GOOGLE-CALENDAR-CONTENT-WIDTH", Integer.toString(width));
        println("X-GOOGLE-CALENDAR-CONTENT-HEIGHT", Integer.toString(height));
    }


    public void endEvent() {
        println("END", "VEVENT");
    }


    private final void println(final String name, final String value) {
        out.print(name);
        out.print(":");
        out.println(value);
    }


    private final void println(final String line) {
        out.println(line);
    }


    private final PrintStream out;
}
