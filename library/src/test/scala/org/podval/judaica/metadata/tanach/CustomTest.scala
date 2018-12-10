package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.Custom._
import org.scalatest.{FlatSpec, Matchers}

final class CustomTest extends FlatSpec with Matchers {

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
    val expected: Of[T] = new Of(expectedMap)
    source.minimize.customs shouldBe expectedMap
    source.maximize.customs shouldBe expected.maximize.customs
  }
}
