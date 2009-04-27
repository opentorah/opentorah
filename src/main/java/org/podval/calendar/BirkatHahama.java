package org.podval.calendar;


public class BirkatHahama {

    private static JewishCalendar jewishCalendar = Calendar.getJewish();


    public static final int FIRST = jewishCalendar.dateFromDate(1, JewishMonth.Nissan, 4).getDays();


    public static Date dateFromNumber(final int number) {
        return jewishCalendar.dateFromDays(FIRST + number * (28 * 365 + 7));
    }


    public static void main(final String[] args) {
        System.out.println(dateFromNumber(203));
        System.out.println(dateFromNumber(204));
        System.out.println(dateFromNumber(205));
        System.out.println(dateFromNumber(206));

//        int days = 0;
//        for (int y = 1; y <= 6000; y++) {
//            int nextDays = jewishCalendar.daysFromParts(jewishCalendar.molad(y, 1));
//            if (nextDays < days) {
//                System.out.print("***** Offending year: " + y);
//                break;
//            }
/////            System.out.println("Molad of year " + y + " on day " + days);
//            days = nextDays;
//        }

//        System.out.println(jewishCalendar.molad(19, 1));
//        System.out.println(jewishCalendar.molad(20, 1));
    }
}
