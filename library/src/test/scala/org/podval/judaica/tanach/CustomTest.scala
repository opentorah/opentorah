package org.podval.judaica.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.podval.judaica.tanach.Custom._

final class CustomTest extends AnyFlatSpec with Matchers {

  "minimize()" should "remove redundant children" in {
    check(
      Map(Ashkenaz -> "X", Chabad -> "X", Sefard -> "X"),
      Map(Common -> "X")
    )
    check(
      Map(Ashkenaz -> "X", Sefard -> "X"),
      Map(Common -> "X")
    )
  }

  private def check[T](sourceMap: Map[Custom, T], expectedMap: Map[Custom, T]): Unit = {
    val source: Of[T] = new Of(sourceMap)
    val expectedMaximized: Map[Custom, T] = new Of(expectedMap).maximize

    val minimized = source.minimize
    val maximized = source.maximize
    minimized.customs shouldBe expectedMap
    maximized shouldBe expectedMaximized
  }
}
