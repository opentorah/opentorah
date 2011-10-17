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
final class AngleNg(val degrees: Int, val more: List[Int]) extends Ordered[AngleNg] {
    checkRange(360)(degrees)
    more.foreach(checkRange(60))

    private def checkRange(range: Int)(value: Int): Unit = {
        if (value < 0) throw new IllegalArgumentException("can not be negative")
        if (value >= range) throw new IllegalArgumentException("can not be bigger than " + range)
    }


    override def equals(other: Any): Boolean = other match {
        case that: AngleNg => zip(that) forall (p => p._1 == p._2) // TODO 'lift' ==
        case _ => false
    }


    override def hashCode = ((41+degrees) /: more) ((v,x) => 41*v+x)


    override def compare(that: AngleNg) =
        zip(that).map(p => p._1.compare(p._2)).find(_ != 0).getOrElse(0) // TODO 'lift' compare


    def +(other: AngleNg): AngleNg = AngleNg(zip(other) map (p => p._1 + p._2)) // TODO 'lift' +


    private def zip(that: AngleNg) = (degrees, that.degrees) :: (more zipAll (that.more, 0, 0))


    def *(n: Int): AngleNg = AngleNg(n*degrees, more map (n * _))


    def roundToSeconds(): AngleNg = roundTo(2)


    def roundToMinutes(): AngleNg = roundTo(1)


    def roundTo(n: Int): AngleNg = {
        if (n < 0) throw new IllegalArgumentException()

        val (more_, toRound) = more splitAt n
        val carry = (toRound :\ 0)((x, c) => if (x + c >= 30) 1 else 0)

        AngleNg(degrees, more_.init :+ (more.last + carry))
    }


    private def carry(value: Int): Int = if (value >= 30) 1 else 0


    // TODO: use symbols: ° ′ ″ ‴
    // TODO: padding
    override def toString: String = (degrees :: more).mkString("(", ", ", ")")


    def sin(): Double = scala.math.sin(toRadians())


    def cos(): Double = scala.math.cos(toRadians())


    def toRadians() = scala.math.toRadians(toDegrees())


    def toDegrees(): Double = ((degrees, 1.0) /: more)((s, x) => s match {case (v, q) => val q_ = q/60.0; (v+x/q_, q_)})
}


object AngleNg {

    def apply(degrees: Int, more: Int*): AngleNg = apply(degrees, more: _*)


    def apply(all: List[Int]): AngleNg = apply(all.head, all.tail)


    def apply(degrees: Int, more: List[Int]): AngleNg = {
        val digits = ((degrees :: more) :\ (0, List()))
            ((x, s) => s match {case (c,r) =>
                val x_ = x+c
                (x_ / 60,  x_ :: r)
            })

        def toRange(range: Int)(what: Int): Int = {
            val result = what % range
            if (result >= 0) result else result+range
        }

        new Angle(toRange(digits.head, 360), digits.tail map toRange(60))
    }



    def asin(value: Double): AngleNg = fromRadians(scala.math.asin(value))


    def fromRadians(value: Double): AngleNg = fromDegrees(scala.math.toDegrees(value))


    private val SIXTY = 60.toDouble
    private val MINUTES = SIXTY
    private val SECONDS = MINUTES*SIXTY
    private val THIRDS  = SECONDS*SIXTY
    private val FOURTHS = THIRDS*SIXTY
    private val FIFTHS  = FOURTHS*SIXTY
    private val SIXTHS  = FIFTHS*SIXTY


    def fromDegrees(value: Double, n: Int): AngleNg = {
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

        AngleNg(
            degrees.toInt,
            minutes.toInt,
            seconds.toInt,
            thirds.toInt,
            fourths.toInt,
            fifths.toInt,
            sixths.toInt)
    }


    def main(args: Array[String]): Unit = {
        val angle = AngleNg(5, 34)
        val value = angle.toDegrees()
        val angle_ = AngleNg.fromDegrees(value)
        println(angle + "=" + value + "->" + angle_)
    }
}
