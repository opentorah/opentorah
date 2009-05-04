package org.podval.calendar;


public class SpringPassover {

    public void tabulate() {
        for (int year = 2000; year < 6000; year++) {
            check(year);
        }
    }


    private void check(final int year) {
        final Date<JewishMonth> tDate = JewishCalendar.getInstance().tkufasNissan(year);
        final Date<JewishMonth> pDate = Date.create(year, JewishMonth.Nissan, 14);
        final int delta = pDate.getDays() - tDate.getDays();
        if (delta < 0) {
            System.out.println(delta);
        }
    }


    public static void main(final String[] args) {
        new SpringPassover().tabulate();
    }
}
