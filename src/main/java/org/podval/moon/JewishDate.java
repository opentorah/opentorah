package org.podval.moon;


public final class JewishDate {

    public static long HOUR = 1080;


    public static int MINUTES_IN_HOUR = 60;


    public static final long MINUTE = HOUR / MINUTES_IN_HOUR;


    public static final int HOURS_IN_DAY = 24;


    public static final long DAY = HOURS_IN_DAY * HOUR;


//    public enum Month { Teshrei, MarHeshvan, Kislev, Tevet, Shevat, Adar, Adar2, Nissan, Iyar, Sivan, Tamuz, Av, Elul }


    private static final int YEARS_IN_CYCLE = 19;


    private static final boolean[] IS_LEAP = new boolean[YEARS_IN_CYCLE];


    static {
        IS_LEAP[3] = true;
        IS_LEAP[6] = true;
        IS_LEAP[8] = true;
        IS_LEAP[11] = true;
        IS_LEAP[14] = true;
        IS_LEAP[17] = true;
        IS_LEAP[0] = true; // 19th year
    }


    private static boolean isLeap(final int year) {
        // Leap years: 3,6,8,11,14,17 and 19 of the 19-year Metonic cycle
        return IS_LEAP[year % YEARS_IN_CYCLE];
    }


    public boolean isLeap() {
        return isLeap(year);
    }


    private static int monthsInYear(final int year) {
        return isLeap(year) ? 13 : 12;
    }


    public int monthsInYear() {
        return monthsInYear(year);
    }


    private static final int[] MONTH_BEFORE_YEAR = new int[YEARS_IN_CYCLE+1];


    static {
        MONTH_BEFORE_YEAR[0] = 0;
        for (int year = 1; year <= YEARS_IN_CYCLE; year++) {
            MONTH_BEFORE_YEAR[year] = MONTH_BEFORE_YEAR[year-1] + monthsInYear(year-1);
        }
    }



    private static int getMonthBeforeYearInCycle(final int yearInCycle) {
        return MONTH_BEFORE_YEAR[yearInCycle-1];
    }


    private static final int MONTH_IN_19_YEAR_CYCLE = getMonthBeforeYearInCycle(YEARS_IN_CYCLE+1);


    /*
    Months:

    Nissan     30
    Iyyar      29
    Sivan      30
    Tammuz     29
    Av         30
    Elul       29
    Tishri     30
    MarHeshvan 29 or 30
    Kislev     29 or 30
    Tevet      29
    Shevat     30
    {Adar I     30}
    Adar {II}  29
     */


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    public static final long LUNAR_MONTH = 29 * DAY + 12 * HOUR + 793;


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    // Our epoch is the 6th day, creation of man, the first Rosh HaShono
    public static final long FIRST_MOLAD = -5*DAY + (1*DAY + 5*HOUR + 204);


    public static JewishDate createFromYearMonth(final int year, final int month) {
        return new JewishDate(year, month, 0, 0, 0, 0);
    }


    public static JewishDate createFromDay(final int day) {
        return new JewishDate(0, 0, day, 0, 0, 0);
    }


    public static JewishDate createFromParts(final long allParts) {
        final long parts = allParts % MINUTE;
        final long allMinutes = allParts / MINUTE;
        final int minutes = (int) (allMinutes % MINUTES_IN_HOUR);
        final int allHours = (int) (allMinutes / MINUTES_IN_HOUR);
        final int hours = allHours % HOURS_IN_DAY;
        final int allDays = allHours / HOURS_IN_DAY;

        return new JewishDate(
            0,
            0,
            allDays,
            hours,
            minutes,
            parts);
    }


    public static final JewishDate moladRoshHaShono(final int year) {
        return createFromYearMonth(year, 1).molad();
    }


    public static final JewishDate dayOfRoshHaShono(final int year) {
        final JewishDate molad = moladRoshHaShono(year);
        int result = molad.getDay();

        final int moladDayOfTheWeek = molad.getDayOfTheWeek();
        if (isADU(moladDayOfTheWeek)) {
            // KH 7:1
            result++;
        } else if (molad.getHours() >= 18) {
            // KH 7:2
            result++;
            if (isADU(moladDayOfTheWeek+1)) {
                // KH 7:3
                result++;
            }
        } else if ((moladDayOfTheWeek == 3) && !isLeap(year) && laterInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && isLeap(year-1) && laterInTheDayThan(molad, 15, 589)) {
            // KH 7:5
            result += 1;
        }

        return createFromDay(result);
    }


    private static boolean isADU(final int dayOfTheWeek) {
        return (dayOfTheWeek == 1) || (dayOfTheWeek == 4) || (dayOfTheWeek == 6);
    }


    private static boolean laterInTheDayThan(final JewishDate molad, final int hour, final int parts) {
        return
            (molad.getHours() > hour) ||
            ((molad.getHours() == hour) && ((molad.getMinutes()*MINUTE + molad.getParts()) > parts));
    }


    public JewishDate(
        final int year,
        final int month,
        final int day,
        final int hours,
        final int minutes,
        final long parts)
    {
        this.year = year;
        this.month = month;
        this.day = day;
        this.hours = hours;
        this.minutes = minutes;
        this.parts = parts;
    }


    public int getYear() {
        return year;
    }


    public int getMonth() {
        return month;
    }


    public int getDay() {
        return day;
    }


    public int getHours() {
        return hours;
    }


    public int getMinutes() {
        return minutes;
    }


    public long getParts() {
        return parts;
    }


    public int getDayOfTheWeek() {
        return (int) ((getDay() + 5) % 7 + 1);
    }


    public int getCycleNumber() {
        return getYear() / 19 + 1;
    }


    public int getYearInCycle() {
        return getYear() % 19;
    }


    public JewishDate molad() {
        final int monthsInPreviousCycles = (getCycleNumber()-1) * MONTH_IN_19_YEAR_CYCLE;
        final int monthInPreviousYears = getMonthBeforeYearInCycle(getYearInCycle());
        final int moladNumber = monthsInPreviousCycles + monthInPreviousYears + (getMonth() - 1);
        return createFromParts(FIRST_MOLAD + LUNAR_MONTH * moladNumber);
    }


    private final int year;


    private final int month;


    private final int day;


    private final int hours;


    private final int minutes;


    private final long parts;


    public static void main(final String[] args) {
        final JewishDate molad = moladRoshHaShono(5769);
        System.out.println("Molad on " + molad.getDayOfTheWeek() + " at " + molad.getMinutes()+ " min " + molad.getParts() + " parts");
    }
}
