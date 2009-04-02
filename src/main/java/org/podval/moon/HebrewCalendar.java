/* $Id: HebrewCalendar.java,v 1.1 2006/07/20 15:06:29 dub Exp $ */

package org.podval.moon;


public final class HebrewCalendar {


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


  public static boolean isLeap(int year) {
    // Leap years: 3,6,8,11,14,17 and 19 of the 19-year Metonic cycle
    return ((7*year+1) % 19) < 7;
  }


  public static int numberOfMonth(int year) {
    return isLeap(year) ? 13 : 12;
  }


  // Molad of the year of creation: BeHaRaD: Sunday, 5 hours 204 parts
  //   (876 haloqim (48 minutes 40 secods) before midnight on the epoch)
  // Molad of the next year: WeYaD: Friday, 14 hours
  //   (8AM the morning of the creation of Adam and Eve)


  // Mean lunar period: 29 days 12 hours 793 parts

  public static long epochToMolad(int year, int month) {
//    - 876/25920 + monthsTo(year, month)*
    return 0;
  }
}
