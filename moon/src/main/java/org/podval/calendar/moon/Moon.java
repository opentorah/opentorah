/* $Id: Moon.java,v 1.3 2006/08/02 02:33:02 dub Exp $ */

package org.podval.calendar.moon;

import java.util.Map;
import java.util.LinkedHashMap;

/*
   120  4 20'       (4 40')
   150  3 48'       (2 48')
   170  1 59'         (59')
*/


public final class Moon {

    /* Numbers as they are printed in Chapter 15 Haloha 6 */
    private static final Map<AngleOld, AngleOld> PRINTED = new LinkedHashMap<AngleOld,AngleOld>();

    static {
        PRINTED.put(new AngleOld( 10), new AngleOld(0, 50));
        PRINTED.put(new AngleOld( 20), new AngleOld(1, 38));
        PRINTED.put(new AngleOld( 30), new AngleOld(2, 24));
        PRINTED.put(new AngleOld( 40), new AngleOld(3,  6));
        PRINTED.put(new AngleOld( 50), new AngleOld(3, 44));
        PRINTED.put(new AngleOld( 60), new AngleOld(4, 16));
        PRINTED.put(new AngleOld( 70), new AngleOld(4, 41));
        PRINTED.put(new AngleOld( 80), new AngleOld(5,  0));
        PRINTED.put(new AngleOld( 90), new AngleOld(5,  5));
        PRINTED.put(new AngleOld(100), new AngleOld(5,  8));
        PRINTED.put(new AngleOld(110), new AngleOld(4, 59));
        PRINTED.put(new AngleOld(120), new AngleOld(4, 20));
        PRINTED.put(new AngleOld(130), new AngleOld(4, 14));
        PRINTED.put(new AngleOld(140), new AngleOld(3, 33));
        PRINTED.put(new AngleOld(150), new AngleOld(3, 48));
        PRINTED.put(new AngleOld(160), new AngleOld(1, 56));
        PRINTED.put(new AngleOld(170), new AngleOld(1, 59));
    }


    public Moon() {
    }


    private static AngleOld mnas(AngleOld maslul, double e) {
        return AngleOld.asin(maslul.sin()/Math.sqrt(e*e + 2*e*maslul.cos() + 1));
    }


    private static double e(AngleOld maslul, AngleOld mnas) {
        return maslul.sin()/mnas.sin()*Math.abs(mnas.cos())-maslul.cos();
    }


    public static void main(String[] args) {
        double totale = 0.0;
        for (AngleOld maslul : PRINTED.keySet()) {
            AngleOld mnas = PRINTED.get(maslul);
            double e = e(maslul, mnas);
            e = round(e, 2);

            String line = maslul + ": " + mnas + "  " + e + " " + mnas(maslul, 11.1);

            double correcte = 0.0;
            if (maslul.getDegreeValue() == 120.0) {
                correcte = 11.10;
            } else
            if (maslul.getDegreeValue() == 150.0) {
                correcte = 11.1;
            } else
            if (maslul.getDegreeValue() == 170.0) {
                correcte = 11.1;
            }

            if (correcte != 0.0) {
                line += "  " + correcte + " " + mnas(maslul, correcte);
            } else {
                totale += e;
            }

            System.out.println(line);
        }
        System.out.println("Average e= " + (totale/(17-3)) + " corrected average= " + (totale+3*11.1)/17);
    }


    private static double round(double value, int digits) {
        double quotient = Math.pow(10, digits);
        return Math.round(value*quotient)/quotient;
    }
}
