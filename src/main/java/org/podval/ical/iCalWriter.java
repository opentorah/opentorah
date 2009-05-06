package org.podval.ical;

import java.io.OutputStream;
import java.io.PrintStream;


public final class iCalWriter {

    public iCalWriter(final OutputStream os) {
        this.out = new PrintStream(os);
    }


    public void beginCalendar(final String prodId, final String name, final String description) {
        println("BEGIN", "VCALENDAR");
        println("PRODID", prodId);
        println("VERSION", "2.0");
        println("CALSCALE", "GREGORIAN");
        println("METHOD", "PUBLISH");
        println("X-WR-CALNAME", name);
        println("X-WR-CALDESC", description);
    }


    public void endCalendar() {
        println("END", "VCALENDAR");
    }


    public void beginEvent() {
        println("BEGIN", "VEVENT");
        println("CATEGORIES", "Holidays");
        println("CLASS", "PUBLIC");
        println("SUMMARY", "sklfjsflkj"); // Same as Google Title
        println("TRANSP", "TRANSPARENT"); // "OPAQUE"
        println("URL;VALUE=URI", "http://lwhjsdgfjhf");
        println("DTSTAMP", "2061121T044202Z");
        println("UID", "asdhjgd-wjks=-f");
        println("DTSTART;VALUE=DATE", "20070203");
        println("DTEND;VALUE=DATE", "20070204");
/////        println("DURATION", "P1D");
        println("STATUS", "CONFIRMED");
    }


    public void addGoggleContent() {
        // HTML only
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
