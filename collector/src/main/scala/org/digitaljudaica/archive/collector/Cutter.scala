package org.digitaljudaica.archive.collector

import java.io.File

import org.digitaljudaica.util.Files
import scala.sys.process._

object Cutter {

  val uncutDirectory: File = new File("/home/dub/Projects/digitaljudaica/rgada")

  val cutDirectory: File = new File("/home/dub/Projects/digitaljudaica/rgada-cut")

  private val defaultOverlap: Int = 0

  private val leftOverlaps: Map[Int, Int] = Map(
    2 -> 260,
    4 -> 260,
    13 -> 440,
    16 -> 480,
    18 -> 180,
    26 -> 60,
    32 -> 80,
    59 -> 160,
    61 -> 160,
    74 -> 100,
    75 -> 100,
    76 -> 120,
    77 -> 140,
    79 -> 60,
    83 -> 40,
    84 -> 280,
    90 -> 60,
    92 -> 60,
    93 -> 60,
    95 -> 140,
    97 -> 60,
    98 -> 60,
    99 -> 60,
    100 -> 60,
    101 -> 80,
    102 -> 80,
    107 -> 100,
    240 -> 320,
    241 -> 300,
    253 -> 200,
    307 -> 120,
    310 -> 200,
    316 -> 200,
    320 -> 220,
    367 -> 200
  )

  private val rightOverlaps: Map[Int, Int] = Map(
    6 -> 300,
    30 -> 160,
    156 -> 300,
    165 -> 400,
    239 -> 600,
    244 -> 200,
    246 -> 320,
    323 -> 200,
    341 -> 400,
    342 -> 200,
    344 -> 240,
    357 -> 120,
    359 -> 300
  )

  def main(args: Array[String]): Unit = {
    val all = Files.filesWithExtensions(uncutDirectory, "jpg").map(spread)
    val spreads: Seq[Int] = all.flatMap(_.right.toOption).sorted

//    val nonSpreads: Seq[String] = all.flatMap(_.left.toOption)
//    println(spreads)
//
//    val missing = (1 to 367).toSet -- spreads.toSet
//    println(missing)

    //spreads.filter(n => 300 < n && n <= 400).foreach(cut)

        cut(367)
  }

  private def inUncut(fileName: String): String =
    new File(uncutDirectory, fileName + ".jpg").getAbsolutePath

  private def spread(fileName: String): Either[String, Int] = {
    val v = fileName.indexOf("об-")
    if (v == -1) Left(fileName) else {
      try {
        val left: Int = fileName.substring(0, v).toInt
        val right: Int = fileName.substring(v + 3).toInt
        if (right == left + 1) Right(left) else Left(fileName)
      } catch {
        case _: NumberFormatException => Left(fileName)
      }
    }
  }

  private def unspread(spread: Int): String = inUncut(spread.toString + "об-" + (spread+1).toString)

  private def page(number: Int, verso: Boolean): String = {
    val numberStr = number.toString
    val base = "000".take(3-numberStr.length) ++ numberStr
    val fileName = base + (if (verso) "-2" else "-1")
    new File(cutDirectory, fileName + ".jpg").getAbsolutePath
  }

  private def cut(spread: Int): Unit = {
    val leftOverlap: Int = leftOverlaps.getOrElse(spread, defaultOverlap)
    val rightOverlap: Int = rightOverlaps.getOrElse(spread, defaultOverlap)

    if (leftOverlap == rightOverlap) {
      execute(s"convert ${unspread(spread)} -crop 2x1+$leftOverlap@ ${inUncut("x-%d")}")
      execute(s"mv ${inUncut("x-0")} ${page(spread, verso = true)}")
      execute(s"mv ${inUncut("x-1")} ${page(spread + 1, verso = false)}")
    } else {
      execute(s"convert ${unspread(spread)} -crop 2x1+$leftOverlap@ ${inUncut("x-%d")}")
      execute(s"mv ${inUncut("x-0")} ${page(spread, verso = true)}")

      execute(s"convert ${unspread(spread)} -crop 2x1+$rightOverlap@ ${inUncut("x-%d")}")
      execute(s"mv ${inUncut("x-1")} ${page(spread + 1, verso = false)}")
    }
  }

  private def execute(command: String): Unit = {
    println(s"running $command")
    command !!
  }
}
