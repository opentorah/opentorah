package org.podval.calendar;


public class BirkatHahama {

    public static final int FIRST = JewishCalendar.daysFromDate(JewishCalendar.dateFromDate(1, JewishMonth.Nissan, 4));


    public static JewishDate dateFromNumber(final int number) {
        return JewishCalendar.dateFromDays(FIRST + number * (28 * 365 + 7));
    }


    public static void main(final String[] args) {
        System.out.println(dateFromNumber(203));
        System.out.println(dateFromNumber(204));
        System.out.println(dateFromNumber(205));
        System.out.println(dateFromNumber(206));
    }
}
