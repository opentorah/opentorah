package org.podval.calendar.dates;


public final class JewishCalendar {
//    public JewishCalendar() {
//        // KH 8:5,6
//
//        addMonthToAllYears(new Month(JewishMonth.Tishri, "Tishri", 30));
//
//        final Month<JewishMonth> MarHeshvanShort = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 29);
//        final Month<JewishMonth> MarHeshvanFull = new Month(JewishMonth.MarHeshvan, "MarHeshvan", 30);
//        final Month<JewishMonth> KislevShort = new Month(JewishMonth.Kislev, "Kislev", 29);
//        final Month<JewishMonth> KislevFull = new Month(JewishMonth.Kislev, "Kislev", 30);
//
//        shortNormalYear.add(MarHeshvanShort);
//        shortNormalYear.add(KislevShort);
//        shortLeapYear.add(MarHeshvanShort);
//        shortLeapYear.add(KislevShort);
//
//        regularNormalYear.add(MarHeshvanShort);
//        regularNormalYear.add(KislevFull);
//        regularLeapYear.add(MarHeshvanShort);
//        regularLeapYear.add(KislevFull);
//
//        fullNormalYear.add(MarHeshvanFull);
//        fullNormalYear.add(KislevFull);
//        fullLeapYear.add(MarHeshvanFull);
//        fullLeapYear.add(KislevFull);
//
//        addMonthToAllYears(new Month(JewishMonth.Tevet, "Tevet", 29));
//        addMonthToAllYears(new Month(JewishMonth.Shevat, "Shevat", 30));
//
//        final Month<JewishMonth> AdarI = new Month(JewishMonth.AdarI, "Adar I", 30);
//        final Month<JewishMonth> Adar = new Month(JewishMonth.Adar, "Adar", 29);
//        final Month<JewishMonth> AdarII = new Month(JewishMonth.AdarII, "Adar II", 29);
//
//        shortNormalYear.add(Adar);
//        regularNormalYear.add(Adar);
//        fullNormalYear.add(Adar);
//
//        shortLeapYear.add(AdarI);
//        shortLeapYear.add(AdarII);
//        regularLeapYear.add(AdarI);
//        regularLeapYear.add(AdarII);
//        fullLeapYear.add(AdarI);
//        fullLeapYear.add(AdarII);
//
//        addMonthToAllYears(new Month(JewishMonth.Nissan, "Nissan", 30));
//        addMonthToAllYears(new Month(JewishMonth.Iyyar, "Iyyar", 29));
//        addMonthToAllYears(new Month(JewishMonth.Sivan, "Sivan", 30));
//        addMonthToAllYears(new Month(JewishMonth.Tammuz, "Tammuz", 29));
//        addMonthToAllYears(new Month(JewishMonth.Av, "Av", 30));
//        addMonthToAllYears(new Month(JewishMonth.Elul, "Elul", 29));
//    }
//
//
//
//    private final List<Month<JewishMonth>> shortNormalYear = new LinkedList<Month<JewishMonth>>();
//    private final List<Month<JewishMonth>> regularNormalYear = new LinkedList<Month<JewishMonth>>();
//    private final List<Month<JewishMonth>> fullNormalYear = new LinkedList<Month<JewishMonth>>();
//    private final List<Month<JewishMonth>> shortLeapYear = new LinkedList<Month<JewishMonth>>();
//    private final List<Month<JewishMonth>> regularLeapYear = new LinkedList<Month<JewishMonth>>();
//    private final List<Month<JewishMonth>> fullLeapYear = new LinkedList<Month<JewishMonth>>();
//
//
//    // KH 8:7,8
//    @Override
//    public List<Month<JewishMonth>> getMonths(final int year) {
//        final int yearLength = yearLength(year);
//
//        final List<Month<JewishMonth>> result;
//
//        if (yearLength == 355) {
//            result = fullNormalYear;
//        } else if (yearLength == 354) {
//            result = regularNormalYear;
//        } else if (yearLength == 353) {
//            result = shortNormalYear;
//        } else if (yearLength == 385) {
//            result = fullLeapYear;
//        } else if (yearLength == 384) {
//            result = regularLeapYear;
//        } else if (yearLength == 383) {
//            result = shortLeapYear;
//        } else {
//            throw new Error("Bug in year length calculations: year " + year + " has length " + yearLength + "!");
//        }
//
//        return result;
//    }
}
