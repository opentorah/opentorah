package org.podval.calendar;


public final class JewishCalendar extends Calendar<JewishMonth> {

    public JewishCalendar() {
        isLeap[3] = true;
        isLeap[6] = true;
        isLeap[8] = true;
        isLeap[11] = true;
        isLeap[14] = true;
        isLeap[17] = true;
        isLeap[0] = true; // 19th year

        monthsBeforeYearInCycle[0] = 0;
        monthsBeforeYearInCycle[1] = 0;
        for (int year = 2; year <= YEARS_IN_CYCLE+1; year++) {
            monthsBeforeYearInCycle[year] = monthsBeforeYearInCycle[year-1] + monthsInYear(year-1);
        }
    }


    @Override
    public int epoch() {
        return 0;
    }


    @Override
    public int epochDayOfTheWeek() {
        return 5;
    }


    @Override
    protected int daysInYearsBeforeYear(final int year) {
        return dayOfRoshHaShono(year)-1;
    }


    @Override
    protected int yearDayIsIn(final int days) {
        int result = (4 * days / (4 * 365 + 1));

        while (true) {
            if (dayOfRoshHaShono(result + 1) > days) {
                break;
            }
            result++;
        }

        return result;
    }


    // KH 8:5,6

    private final Month Tishri = new Month(JewishMonth.Tishri, "Tishri", 30);
    private final Month MarHeshvanShort = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 29);
    private final Month MarHeshvanFull = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 30);
    private final Month KislevShort = new Month(JewishMonth.Kislev, "Kislev", 29);
    private final Month KislevFull = new Month(JewishMonth.Kislev, "Kislev", 30);
    private final Month Tevet = new Month(JewishMonth.Tevet, "Tevet", 29);
    private final Month Shevat = new Month(JewishMonth.Shevat, "Shevat", 30);
    private final Month AdarI = new Month(JewishMonth.AdarI, "Adar I", 30);
    private final Month Adar = new Month(JewishMonth.Adar, "Adar", 29);
    private final Month AdarII = new Month(JewishMonth.AdarII, "Adar II", 29);
    private final Month Nissan = new Month(JewishMonth.Nissan, "Nissan", 30);
    private final Month Iyyar = new Month(JewishMonth.Iyyar, "Iyyar", 29);
    private final Month Sivan = new Month(JewishMonth.Sivan, "Sivan", 30);
    private final Month Tammuz = new Month(JewishMonth.Tammuz, "Tammuz", 29);
    private final Month Av = new Month(JewishMonth.Av, "Av", 30);
    private final Month Elul= new Month(JewishMonth.Elul, "Elul", 29);


    private final Month[] shortNormalYear = {
        Tishri, MarHeshvanShort, KislevShort, Tevet, Shevat, Adar,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    private final Month[] regularNormalYear = {
        Tishri, MarHeshvanShort, KislevFull, Tevet, Shevat, Adar,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    private final Month[] fullNormalYear = {
        Tishri, MarHeshvanFull, KislevFull, Tevet, Shevat, Adar,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    private final Month[] shortLeapYear = {
        Tishri, MarHeshvanShort, KislevShort, Tevet, Shevat, AdarI, AdarII,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    private final Month[] regularLeapYear = {
        Tishri, MarHeshvanShort, KislevFull, Tevet, Shevat, AdarI, AdarII,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    private final Month[] fullLeapYear = {
        Tishri, MarHeshvanFull, KislevFull, Tevet, Shevat, AdarI, AdarII,
        Nissan, Iyyar, Sivan, Tammuz, Av, Elul
    };


    // KH 8:7,8
    @Override
    public Month<JewishMonth>[] getMonths(final int year) {
        final int yearLength = yearLength(year);

        final Month[] result;

        if (yearLength == 355) {
            result = fullNormalYear;
        } else if (yearLength == 354) {
            result = regularNormalYear;
        } else if (yearLength == 353) {
            result = shortNormalYear;
        } else if (yearLength == 385) {
            result = fullLeapYear;
        } else if (yearLength == 384) {
            result = regularLeapYear;
        } else if (yearLength == 383) {
            result = shortLeapYear;
        } else {
            throw new Error("Bug in year length calculations!");
        }

        return result;
    }


    @Override
    protected void monthNotFound(final int year, final JewishMonth monthName) {
        throw new IllegalArgumentException((monthName == JewishMonth.Adar) ?
            "No Adar in a leap year" :
            "No numbered Adar in a non-leap year");
    }


    public int yearLength(final int year) {
        return dayOfRoshHaShono(year+1) - dayOfRoshHaShono(year);
    }


    public int dayOfRoshHaShono(final int year) {
        final long molad = molad(year, 1);
        int result = daysFromParts(molad);

        final int moladDayOfTheWeek = dayOfTheWeek(result);
        if (isADU(moladDayOfTheWeek)) {
            // KH 7:1
            result++;
        } else if (hoursFromParts(molad) >= 18) {
            // KH 7:2
            result++;
            if (isADU(moladDayOfTheWeek % 7 + 1)) {
                // KH 7:3
                result++;
            }
        } else if ((moladDayOfTheWeek == 3) && !isLeap(year) && notEarlierInTheDayThan(molad, 9, 240)) {
            // KH 7:4
            result += 2;
        } else if ((moladDayOfTheWeek == 2) && isLeap(year-1) && notEarlierInTheDayThan(molad, 15, 589)) {
            // KH 7:5
            result += 1;
        }

        return result;
    }


    private boolean isADU(final int dayOfTheWeek) {
        return (dayOfTheWeek == 1) || (dayOfTheWeek == 4) || (dayOfTheWeek == 6);
    }

    public static final int YEARS_IN_CYCLE = 19;


    private final boolean[] isLeap = new boolean[YEARS_IN_CYCLE];


    public boolean isLeap(final int year) {
        return isLeap[yearInCycle(year)];
    }


    public int monthsInYear(final int year) {
        return isLeap(year) ? 13 : 12;
    }


    private final int[] monthsBeforeYearInCycle = new int[YEARS_IN_CYCLE+2];


    public final int MONTHS_IN_CYCLE = monthsBeforeYearInCycle(YEARS_IN_CYCLE+1);


    public long molad(final int year, final int month) {
        final int monthsInPreviousCycles = (cycleNumber(year)-1) * MONTHS_IN_CYCLE;
        final int monthInPreviousYears = monthsBeforeYearInCycle(yearInCycle(year));
        final int moladNumber = monthsInPreviousCycles + monthInPreviousYears + (month - 1);
        return FIRST_MOLAD + LUNAR_MONTH * moladNumber;
    }


    public int cycleNumber(final int year) {
        return year / YEARS_IN_CYCLE + 1;
    }


    public int yearInCycle(final int year) {
        return year % YEARS_IN_CYCLE;
    }


    public int monthsBeforeYearInCycle(final int yearInCycle) {
        return monthsBeforeYearInCycle[yearInCycle];
    }


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


    public int daysFromParts(final long parts) {
        return (int) (parts / PARTS_IN_DAY);
    }


    public int hoursFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_DAY) / PARTS_IN_HOUR);
    }


    public int minutesFromParts(final long parts) {
        return (int) ((parts % PARTS_IN_HOUR) / PARTS_IN_MINUTE);
    }


    public int partsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_MINUTE);
    }


    public int hoursMinutesAndPartsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_DAY);
    }


    public boolean notEarlierInTheDayThan(final long when, final int hour, final int parts) {
        return hoursMinutesAndPartsFromParts(when) >= hour * PARTS_IN_HOUR + parts;
    }
}
