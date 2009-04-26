package org.podval.calendar;


public class BirkatHahama {

    private static JewishCalendar jewishCalendar = Calendar.getJewish();


    public static final int FIRST = jewishCalendar.daysFromDate(jewishCalendar.dateFromDate(1, JewishMonth.Nissan, 4));


    public static Date dateFromNumber(final int number) {
        return jewishCalendar.dateFromDays(FIRST + number * (28 * 365 + 7));
    }


    public static void main(final String[] args) {
        System.out.println(dateFromNumber(203));
        System.out.println(dateFromNumber(204));
        System.out.println(dateFromNumber(205));
        System.out.println(dateFromNumber(206));
    }
}
