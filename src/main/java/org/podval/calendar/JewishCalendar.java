package org.podval.calendar;

import java.util.List;
import java.util.LinkedList;


public final class JewishCalendar extends Calendar<JewishMonth> {

    public JewishCalendar() {
        isLeap[3] = true;
        isLeap[6] = true;
        isLeap[8] = true;
        isLeap[11] = true;
        isLeap[14] = true;
        isLeap[17] = true;
        isLeap[19] = true; // 19th year

        monthsBeforeYearInCycle[0] = 0;
        monthsBeforeYearInCycle[1] = 0;
        for (int year = 2; year <= YEARS_IN_CYCLE+1; year++) {
            monthsBeforeYearInCycle[year] = monthsBeforeYearInCycle[year-1] + monthsInYear(year-1);
        }
        MONTHS_IN_CYCLE = monthsBeforeYearInCycle[YEARS_IN_CYCLE+1];

        // KH 8:5,6

        addMonthToAllYears(new Month(JewishMonth.Tishri, "Tishri", 30));

        final Month<JewishMonth> MarHeshvanShort = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 29);
        final Month<JewishMonth> MarHeshvanFull = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 30);
        final Month<JewishMonth> KislevShort = new Month(JewishMonth.Kislev, "Kislev", 29);
        final Month<JewishMonth> KislevFull = new Month(JewishMonth.Kislev, "Kislev", 30);

        shortNormalYear.add(MarHeshvanShort);
        shortNormalYear.add(KislevShort);
        shortLeapYear.add(MarHeshvanShort);
        shortLeapYear.add(KislevShort);

        regularNormalYear.add(MarHeshvanShort);
        regularNormalYear.add(KislevFull);
        regularLeapYear.add(MarHeshvanShort);
        regularLeapYear.add(KislevFull);

        fullNormalYear.add(MarHeshvanFull);
        fullNormalYear.add(KislevFull);
        fullLeapYear.add(MarHeshvanFull);
        fullLeapYear.add(KislevFull);

        addMonthToAllYears(new Month(JewishMonth.Tevet, "Tevet", 29));
        addMonthToAllYears(new Month(JewishMonth.Shevat, "Shevat", 30));

        final Month<JewishMonth> AdarI = new Month(JewishMonth.AdarI, "Adar I", 30);
        final Month<JewishMonth> Adar = new Month(JewishMonth.Adar, "Adar", 29);
        final Month<JewishMonth> AdarII = new Month(JewishMonth.AdarII, "Adar II", 29);

        shortNormalYear.add(Adar);
        regularNormalYear.add(Adar);
        fullNormalYear.add(Adar);

        shortLeapYear.add(AdarI);
        shortLeapYear.add(AdarII);
        regularLeapYear.add(AdarI);
        regularLeapYear.add(AdarII);
        fullLeapYear.add(AdarI);
        fullLeapYear.add(AdarII);

        addMonthToAllYears(new Month(JewishMonth.Nissan, "Nissan", 30));
        addMonthToAllYears(new Month(JewishMonth.Iyyar, "Iyyar", 29));
        addMonthToAllYears(new Month(JewishMonth.Sivan, "Sivan", 30));
        addMonthToAllYears(new Month(JewishMonth.Tammuz, "Tammuz", 29));
        addMonthToAllYears(new Month(JewishMonth.Av, "Av", 30));
        addMonthToAllYears(new Month(JewishMonth.Elul, "Elul", 29));
    }



    private void addMonthToAllYears(final Month<JewishMonth> month) {
        shortNormalYear.add(month);
        regularNormalYear.add(month);
        fullNormalYear.add(month);
        shortLeapYear.add(month);
        regularLeapYear.add(month);
        fullLeapYear.add(month);
    }


    private final List<Month<JewishMonth>> shortNormalYear = new LinkedList<Month<JewishMonth>>();
    private final List<Month<JewishMonth>> regularNormalYear = new LinkedList<Month<JewishMonth>>();
    private final List<Month<JewishMonth>> fullNormalYear = new LinkedList<Month<JewishMonth>>();
    private final List<Month<JewishMonth>> shortLeapYear = new LinkedList<Month<JewishMonth>>();
    private final List<Month<JewishMonth>> regularLeapYear = new LinkedList<Month<JewishMonth>>();
    private final List<Month<JewishMonth>> fullLeapYear = new LinkedList<Month<JewishMonth>>();


    private final int[] monthsBeforeYearInCycle = new int[YEARS_IN_CYCLE+2];


    public final int MONTHS_IN_CYCLE;


    public int monthsBeforeYearInCycle(final int yearInCycle) {
        return monthsBeforeYearInCycle[yearInCycle];
    }


    @Override
    public int epoch() {
        return 0;
    }


    @Override
    protected int daysInYearsBeforeYear(final int year) {
        return dayOfRoshHaShono(year);
    }


    // KH 8:7,8
    @Override
    public List<Month<JewishMonth>> getMonths(final int year) {
        final int yearLength = yearLength(year);

        final List<Month<JewishMonth>> result;

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
            throw new Error("Bug in year length calculations: year " + year + " has length " + yearLength + "!");
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


    public boolean notEarlierInTheDayThan(final long when, final int hour, final int parts) {
        return hoursMinutesAndPartsFromParts(when) >= hour * PARTS_IN_HOUR + parts;
    }


    public static final int YEARS_IN_CYCLE = 19;


    private final boolean[] isLeap = new boolean[YEARS_IN_CYCLE+1];


    public boolean isLeap(final int year) {
        return isLeap[yearInCycle(year)];
    }


    public int monthsInYear(final int year) {
        return isLeap(year) ? 13 : 12;
    }


    public int yearInCycle(final int year) {
        final int result = year % YEARS_IN_CYCLE;
        return (result == 0) ? YEARS_IN_CYCLE : result;
    }


    public int cycleNumber(final int year) {
        return (year > 0) ?
            (year-1)/ YEARS_IN_CYCLE + 1 :
            year/ YEARS_IN_CYCLE;
    }


    public static long PARTS_IN_HOUR = 1080;


    public static int MINUTES_IN_HOUR = 60;


    public static final long PARTS_IN_MINUTE = PARTS_IN_HOUR / MINUTES_IN_HOUR;


    public static final int HOURS_IN_DAY = 24;


    public static final long PARTS_IN_DAY = HOURS_IN_DAY * PARTS_IN_HOUR;


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


    public int hoursMinutesAndPartsFromParts(final long parts) {
        return (int) (parts % PARTS_IN_DAY);
    }


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    public static final long LUNAR_MONTH = partsFromDate(29, 12, 793);


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    // Our epoch is the 6th day, creation of Man, the first Rosh HaShono
    public static final long FIRST_MOLAD = partsFromDate(1, 5, 204);


    public long molad(final int year, final int month) {
        return FIRST_MOLAD + LUNAR_MONTH * moladNumber(year, month);
    }


    public int moladNumber(int year, final int month) {
        final int monthsInPreviousCycles = (cycleNumber(year)-1) * MONTHS_IN_CYCLE;
        final int monthInPreviousYears = monthsBeforeYearInCycle(yearInCycle(year));
        return monthsInPreviousCycles + monthInPreviousYears + (month - 1);
    }


    public Date<JewishMonth> moladDate(final int year, final int month) {
        return dateFromParts(molad(year, month));
    }


    // KH 9:3
    public final long FIRST_TKUFAS_NISSAN = molad(1, 7) - partsFromDate(7, 9, 642);


    public final long YEAR_OF_SHMUEL = partsFromDate(365, 6, 0);


    public Date<JewishMonth> tkufasNissanShmuel(final int year) {
        return dateFromParts(FIRST_TKUFAS_NISSAN + (year-1)*YEAR_OF_SHMUEL);
    }


    public final int FIRST = JewishCalendar.daysFromParts(FIRST_TKUFAS_NISSAN);


    public static long partsFromDate(final int days, final int hours, final int parts) {
        return days*PARTS_IN_DAY + hours*PARTS_IN_HOUR + parts;
    }
}
