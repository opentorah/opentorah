package org.podval.moon;


public class JewishDate {

    public enum Month { Teshrei, MarHeshvan, Kislev, Tevet, Shevat, Adar, Adar2, Nissan, Iyar, Sivan, Tamuz, Av, Elul }

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

    public JewishDate(final int year, final Month month, int day) {
        this.year = year;
        this.month = month;
        this.day = day;
    }


    private final int year;


    private final Month month;


    private final int day;
}
