package org.podval.calendar;


public class Months {

    // KH 8:5,6

    private static final Month TISHRI = new Month(JewishMonth.Tishri, "Tishri", 30);
    private static final Month MAR_HESHVAN_SHORT = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 29);
    private static final Month MAR_HESHVAN_FULL = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 30);
    private static final Month KISLEV_SHORT = new Month(JewishMonth.Kislev, "Kislev", 29);
    private static final Month KISLEV_FULL = new Month(JewishMonth.Kislev, "Kislev", 30);
    private static final Month TEVET = new Month(JewishMonth.Tevet, "Tevet", 29);
    private static final Month SHEVAT = new Month(JewishMonth.Shevat, "Shevat", 30);
    private static final Month ADAR_I = new Month(JewishMonth.AdarI, "Adar I", 30);
    private static final Month ADAR = new Month(JewishMonth.Adar, "Adar", 29);
    private static final Month ADAR_II = new Month(JewishMonth.AdarII, "Adar II", 29);
    private static final Month NISSAN = new Month(JewishMonth.Nissan, "Nissan", 30);
    private static final Month IYYAR = new Month(JewishMonth.Iyyar, "Iyyar", 29);
    private static final Month SIVAN = new Month(JewishMonth.Sivan, "Sivan", 30);
    private static final Month TAMMUZ = new Month(JewishMonth.Tammuz, "Tammuz", 29);
    private static final Month AV = new Month(JewishMonth.Av, "Av", 30);
    private static final Month ELUL= new Month(JewishMonth.Elul, "Elul", 29);


    private static Month[] NORMAL_SHORT_YEAR = {
        TISHRI,
        MAR_HESHVAN_SHORT,
        KISLEV_SHORT,
        TEVET,
        SHEVAT,
        ADAR,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    private static Month[] NORMAL_REGULAR_YEAR = {
        TISHRI,
        MAR_HESHVAN_SHORT,
        KISLEV_FULL,
        TEVET,
        SHEVAT,
        ADAR,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    private static Month[] NORMAL_FULL_YEAR = {
        TISHRI,
        MAR_HESHVAN_FULL,
        KISLEV_FULL,
        TEVET,
        SHEVAT,
        ADAR,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    private static Month[] LEAP_SHORT_YEAR = {
        TISHRI,
        MAR_HESHVAN_SHORT,
        KISLEV_SHORT,
        TEVET,
        SHEVAT,
        ADAR_I,
        ADAR_II,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    private static Month[] LEAP_REGULAR_YEAR = {
        TISHRI,
        MAR_HESHVAN_SHORT,
        KISLEV_FULL,
        TEVET,
        SHEVAT,
        ADAR_I,
        ADAR_II,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    private static Month[] LEAP_FULL_YEAR = {
        TISHRI,
        MAR_HESHVAN_FULL,
        KISLEV_FULL,
        TEVET,
        SHEVAT,
        ADAR_I,
        ADAR_II,
        NISSAN,
        IYYAR,
        SIVAN,
        TAMMUZ,
        AV,
        ELUL
    };


    // KH 8:7,8
    public static Month[] getMonths(final int year) {
        final int yearLength = Years.yearLength(year);

        final Month[] result;

        if (yearLength == 355) {
            result = NORMAL_FULL_YEAR;
        } else if (yearLength == 354) {
            result = NORMAL_REGULAR_YEAR;
        } else if (yearLength == 353) {
            result = NORMAL_SHORT_YEAR;
        } else if (yearLength == 385) {
            result = LEAP_FULL_YEAR;
        } else if (yearLength == 384) {
            result = LEAP_REGULAR_YEAR;
        } else if (yearLength == 383) {
            result = LEAP_SHORT_YEAR;
        } else {
            throw new Error("Bug in year length calculations!");
        }

        return result;
    }
}
