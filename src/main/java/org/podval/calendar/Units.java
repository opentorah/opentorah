package org.podval.calendar;


public final class Units {

    public static long PARTS_IN_HOUR = 1080;


    public static int MINUTES_IN_HOUR = 60;


    public static final long PARTS_IN_MINUTE = PARTS_IN_HOUR / MINUTES_IN_HOUR;


    public static final int HOURS_IN_DAY = 24;


    public static final long PARTS_IN_DAY = HOURS_IN_DAY * PARTS_IN_HOUR;


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    public static final long LUNAR_MONTH = 29 * PARTS_IN_DAY + 12 * PARTS_IN_HOUR + 793;


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    // Our epoch is the 6th day, creation of Man, the first Rosh HaShono
    public static final long FIRST_MOLAD = -5*PARTS_IN_DAY + (1*PARTS_IN_DAY + 5*PARTS_IN_HOUR + 204);


    public static int daysFromParts(final long parts) {
        return (int) (parts / PARTS_IN_DAY);
    }


    public static int hoursFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_DAY) / PARTS_IN_HOUR);
    }


    public static int minutesFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_HOUR) / PARTS_IN_MINUTE);
    }


    public static int partsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_MINUTE);
    }


    public static int hoursMinutesAndPartsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_DAY);
    }


    public static boolean notEarlierInTheDayThan(final long when, final int hour, final int parts) {
        return hoursMinutesAndPartsFromParts(when) >= hour * PARTS_IN_HOUR + parts;
    }
}
