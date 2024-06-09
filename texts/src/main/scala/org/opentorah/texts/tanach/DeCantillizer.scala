package org.opentorah.texts.tanach

object DeCantillizer:
  def main(args: Array[String]): Unit =
    def is(char: Char): Boolean = '\u05B0' <= char && char <= '\u05EA'
    while
      println("Input:")
      val line = scala.io.StdIn.readLine()
      val result = line.filter(is)
      println(s"Result: [$result]")
      true
    do ()
