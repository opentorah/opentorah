/* $Id: Moon.java,v 1.3 2006/08/02 02:33:02 dub Exp $ */

package org.podval.moon;

import java.util.Map;
import java.util.LinkedHashMap;

/*
   120  4 20'       (4 40')
   150  3 48'       (2 48')
   170  1 59'         (59')
*/


public final class Moon {

    /* Numbers as they are printed in Chapter 15 Haloha 6 */
    private static final Map<Angle, Angle> PRINTED = new LinkedHashMap<Angle,Angle>();

    static {
        PRINTED.put(new Angle( 10), new Angle(0, 50));
        PRINTED.put(new Angle( 20), new Angle(1, 38));
        PRINTED.put(new Angle( 30), new Angle(2, 24));
        PRINTED.put(new Angle( 40), new Angle(3,  6));
        PRINTED.put(new Angle( 50), new Angle(3, 44));
        PRINTED.put(new Angle( 60), new Angle(4, 16));
        PRINTED.put(new Angle( 70), new Angle(4, 41));
        PRINTED.put(new Angle( 80), new Angle(5,  0));
        PRINTED.put(new Angle( 90), new Angle(5,  5));
        PRINTED.put(new Angle(100), new Angle(5,  8));
        PRINTED.put(new Angle(110), new Angle(4, 59));
        PRINTED.put(new Angle(120), new Angle(4, 20));
        PRINTED.put(new Angle(130), new Angle(4, 14));
        PRINTED.put(new Angle(140), new Angle(3, 33));
        PRINTED.put(new Angle(150), new Angle(3, 48));
        PRINTED.put(new Angle(160), new Angle(1, 56));
        PRINTED.put(new Angle(170), new Angle(1, 59));
    }


    public Moon() {
    }


    private static Angle mnas(Angle maslul, double e) {
        return Angle.asin(maslul.sin()/Math.sqrt(e*e + 2*e*maslul.cos() + 1));
    }


    private static double e(Angle maslul, Angle mnas) {
        return maslul.sin()/mnas.sin()*Math.abs(mnas.cos())-maslul.cos();
    }


    public static void main(String[] args) {
        double totale = 0.0;
        for (Angle maslul : PRINTED.keySet()) {
            Angle mnas = PRINTED.get(maslul);
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
