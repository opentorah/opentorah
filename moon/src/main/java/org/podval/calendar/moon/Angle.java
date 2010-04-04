/* $Id: Angle.java,v 1.1 2006/07/20 15:06:25 dub Exp $ */

package org.podval.calendar.moon;


public final class Angle {

    public static final double RADIANS_TO_DEGREES = 360.0/(2*Math.PI);


    public static Angle fromRadians(double value) {
        return new Angle(value*RADIANS_TO_DEGREES);
    }


    public static Angle asin(double value) {
        return fromRadians(Math.asin(value));
    }


    public Angle(int degrees, int minutes) {
        this(degrees + minutes / 60f);
    }


    public Angle(double value) {
        this.value = value;
    }


    public int getDegrees() {
        return (int) Math.floor(value);
    }


    public int getMinutes() {
        return (int) Math.round((value - getDegrees()) / (1 / 60f));
    }


    public String toString() {
        int degrees = getDegrees();
        String padDegrees = "";
        if (degrees < 100) padDegrees += " ";
        if (degrees < 10) padDegrees += " ";

        int minutes = getMinutes();
        String padMinutes = "";
        if (minutes < 10) padMinutes = " ";

        return padDegrees + degrees + " " + padMinutes + minutes + "'";
    }


    public double getRadianValue() {
        return value/RADIANS_TO_DEGREES;
    }


    public double getDegreeValue() {
        return value;
    }


    public double cos() {
        return Math.cos(getRadianValue());
    }


    public double sin() {
        return Math.sin(getRadianValue());
    }


    private final double value;
}
