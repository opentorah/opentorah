/*
 * Copyright 2011 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.calendar.moon


// TODO switch to List-of-positions representation
final class Angle(
    val degrees: Int,
    val minutes: Int,
    val seconds: Int,
    val thirds: Int,
    val fourths: Int,
    val fifths: Int,
    val sixths: Int) extends Ordered[Angle]
{
    checkRange(degrees, 360)
    checkRange(minutes)
    checkRange(seconds)
    checkRange(thirds)
    checkRange(fourths)
    checkRange(fifths)
    checkRange(sixths)


    private def checkRange(value: Int): Unit = checkRange(value, 60)


    private def checkRange(value: Int, range: Int): Unit = {
        if (value < 0) throw new IllegalArgumentException("can not be negative")
        if (value >= range) throw new IllegalArgumentException("can not be bigger than " + range)
    }


    override def equals(other: Any): Boolean = other match {
        case that: Angle => 
            (degrees == that.degrees) &&
            (minutes == that.minutes) &&
            (seconds == that.seconds) &&
            (thirds == that.thirds) &&
            (fourths == that.fourths) &&
            (fifths == that.fifths) &&
            (sixths == that.sixths)
        case _ => false
    }


    override def hashCode =
        41*(41*(41*(41*(41*(41*(41+degrees)+minutes)+seconds)+thirds)+fourths)+fifths)+sixths


    override def compare(that: Angle) = {
        var result = degrees.compare(that.degrees)
        if (result != 0) result else {
            result = minutes.compare(that.minutes)
            if (result != 0) result else {
                result = seconds.compare(seconds)
                if (result != 0) result else {
                    result = thirds.compare(that.thirds)
                    if (result != 0) result else {
                        result = fourths.compare(that.fourths)
                        if (result != 0) result else {
                            result = fifths.compare(that.fifths)
                            if (result != 0) result else {
                                sixths.compare(that.sixths)
                            }
                        }
                    }
                }
            }
        }
    }


    def +(other: Angle): Angle = Angle(
        degrees + other.degrees,
        minutes + other.minutes,
        seconds + other.seconds,
        thirds + other.thirds,
        fourths + other.fourths,
        fifths + other.fifths,
        sixths + other.sixths
    )


    def *(n: Int): Angle = Angle(
        n*degrees,
        n*minutes,
        n*seconds,
        n*thirds,
        n*fourths,
        n*fifths,
        n*sixths
    )


    def roundToSeconds(): Angle =
        Angle(
            degrees,
            minutes,
            seconds + carry(thirds+carry(fourths+carry(fifths+carry(sixths))))
        )


    def roundToMinutes(): Angle =
        Angle(
            degrees,
            minutes + carry(seconds + carry(thirds+carry(fourths+carry(fifths+carry(sixths)))))
        )


    private def carry(value: Int): Int = if (value >= 30) 1 else 0


    override def toString: String =
      // TODO padding?
      degrees + "°" +
      minutes + "′" +
      seconds + "″" +
      (if ((thirds == 0) && (fourths == 0) && (fifths == 0) && (sixths == 0)) "" else thirds + "‴") +
      (if ((fourths == 0) && (fifths == 0) && (sixths == 0)) "" else fourths + "″″") +
      (if ((fifths == 0) && (sixths == 0)) "" else fifths + "‴″") +
      (if (sixths == 0) "" else sixths + "‴‴")


    def sin(): Double = scala.math.sin(toRadians())


    def cos(): Double = scala.math.cos(toRadians())


    def toRadians() = scala.math.toRadians(toDegrees())


    def toDegrees(): Double =
        degrees +
        minutes/Angle.MINUTES +
        seconds/Angle.SECONDS +
        thirds /Angle.THIRDS +
        fourths/Angle.FOURTHS +
        fifths /Angle.FIFTHS +
        sixths /Angle.SIXTHS
}


object Angle {

    def apply(degrees: Int): Angle =
        apply(degrees, 0)


    def apply(degrees: Int, minutes: Int): Angle =
        apply(degrees, minutes, 0)


    def apply(degrees: Int, minutes: Int, seconds: Int): Angle =
        apply(degrees, minutes, seconds, 0)


    def apply(degrees: Int, minutes: Int, seconds: Int, thirds: Int): Angle =
        apply(degrees, minutes, seconds, thirds, 0)


    def apply(degrees: Int, minutes: Int, seconds: Int, thirds: Int, fourths: Int): Angle =
        apply(degrees, minutes, seconds, thirds, fourths, 0)


    def apply(degrees: Int, minutes: Int, seconds: Int, thirds: Int, fourths: Int, fifths: Int): Angle =
        apply(degrees, minutes, seconds, thirds, fourths, fifths, 0)


    def apply(degrees: Int, minutes: Int, seconds: Int, thirds: Int, fourths: Int, fifths: Int, sixths: Int): Angle = {
        val fifths_  = carry(fifths, sixths)
        val fourths_ = carry(fourths, fifths_)
        val thirds_  = carry(thirds , fourths_)
        val seconds_ = carry(seconds, thirds_)
        val minutes_ = carry(minutes, seconds_)
        val degrees_ = carry(degrees, minutes_)

        new Angle(
            toRange(degrees_, 360),
            toRange(minutes_),
            toRange(seconds_),
            toRange(thirds_),
            toRange(fourths_),
            toRange(fifths_),
            toRange(sixths)
        )
    }


    private def carry(big: Int, small: Int): Int = big + (small / 60)


    private def toRange(what: Int): Int = toRange(what, 60)
        

    private def toRange(what: Int, range: Int): Int = {
        val result = what % range
        if (result >= 0) result else result+range
    }


    def asin(value: Double): Angle = fromRadians(scala.math.asin(value))


    def fromRadians(value: Double): Angle = fromDegrees(scala.math.toDegrees(value))


    private val SIXTY = 60.toDouble
    private val MINUTES = SIXTY
    private val SECONDS = MINUTES*SIXTY
    private val THIRDS  = SECONDS*SIXTY
    private val FOURTHS = THIRDS*SIXTY
    private val FIFTHS  = FOURTHS*SIXTY
    private val SIXTHS  = FIFTHS*SIXTY


    def fromDegrees(value: Double): Angle = {
        var leftover = value
        val degrees = scala.math.floor(leftover)
        leftover -= degrees

        val minutes = scala.math.floor(leftover*MINUTES)
        leftover -= minutes/MINUTES

        val seconds = scala.math.floor(leftover*SECONDS)
        leftover -= seconds/SECONDS

        val thirds  = scala.math.floor(leftover*THIRDS)
        leftover -= thirds/THIRDS

        val fourths = scala.math.floor(leftover*FOURTHS)
        leftover -= fourths/FOURTHS

        val fifths  = scala.math.floor(leftover*FIFTHS)
        leftover -= fifths/FIFTHS

        val sixths  = scala.math.round(leftover*SIXTHS)

        Angle(
            degrees.toInt,
            minutes.toInt,
            seconds.toInt,
            thirds.toInt,
            fourths.toInt,
            fifths.toInt,
            sixths.toInt)
    }


    def main(args: Array[String]): Unit = {
        val angle = Angle(5, 34)
        val value = angle.toDegrees()
        val angle_ = Angle.fromDegrees(value)
        println(angle + "=" + value + "->" + angle_)
    }
}
