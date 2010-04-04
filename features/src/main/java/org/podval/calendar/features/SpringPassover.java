package org.podval.calendar.features;

import org.podval.calendar.dates.JewishDate;
import org.podval.calendar.dates.JewishMonth;


public class SpringPassover {

    public void tabulate() {
        for (int year = 3200; year < 6000; year++) {
            check(year);
        }
    }


    private void check(final int year) {
        final JewishDate tDate = Tkufos.tkufasNissan(year);
        final JewishDate pDate = JewishDate.create(year, JewishMonth.Nissan, 14);
        final int delta = pDate.getDays() - tDate.getDays();
        if (delta < 0) {
            System.out.println(delta);
        }
    }


    public static void main(final String[] args) {
        new SpringPassover().tabulate();
    }
}
