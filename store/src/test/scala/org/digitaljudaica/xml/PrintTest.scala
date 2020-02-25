package org.digitaljudaica.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.xml.Elem

final class PrintTest extends AnyFlatSpec with Matchers {

  private val width = 120

  private def formatStandard(from: From): String =
    new scala.xml.PrettyPrinter(width, 2).format(Parser.run(from.load))

  // TODO switch to PaigesPrettyPrinter and see that it doesn't lose anything
  // (compare serialization of the input and of the parsed pretty-print...).

  private def formatNg(from: From): String =
    new PrettyPrinter(width, 2).format(Parser.run(from.load))

  "PrettyPrinting" should "work" in {
    val from: From =
      From.resource(Parser, "print")
      // From.xml(<creation><date when="2020-02-24"/><note/></creation>)
      // From.xml(<creation><date when="2020-02-24"/>blah</creation>)

    val standard: String = formatStandard(from)
//    println("--- Standard ---")
//    println(standard)

    val mine: String = formatNg(from)
    println("--- Mine ---")
    println(mine)

    val isSame: Boolean = standard == mine
    println(s"Same: $isSame")

//    mine shouldBe standard
  }
}
