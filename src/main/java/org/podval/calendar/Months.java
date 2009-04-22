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


    public static Month[] getMonths(final boolean isLeap, final YearKind yearKind) {
        Month[] result = null;

        if (!isLeap) {
            switch (yearKind) {
            case SHORT: result = NORMAL_SHORT_YEAR; break;
            case REGULAR: result = NORMAL_REGULAR_YEAR; break;
            case FULL: result = NORMAL_FULL_YEAR; break;
            }
        } else {
            switch (yearKind) {
            case SHORT: result = LEAP_SHORT_YEAR; break;
            case REGULAR: result = LEAP_REGULAR_YEAR; break;
            case FULL: result = LEAP_FULL_YEAR; break;
            }
        }

        return result;
    }
}
