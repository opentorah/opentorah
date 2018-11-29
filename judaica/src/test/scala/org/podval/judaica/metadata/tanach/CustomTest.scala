package org.podval.judaica.metadata.tanach

import org.scalatest.{FlatSpec, Matchers}

final class CustomTest extends FlatSpec with Matchers {

  "minimize()" should "remove redundant children" in {
    check(
      Map(Custom.Ashkenaz -> None, Custom.Chabad -> None, Custom.Sefard -> None),
      Map(Custom.Common -> None)
    )
  }

  private def check[T](sourceMap: Map[Custom, T], expectedMap: Map[Custom, T]): Unit = {
    val source: Custom.Of[T] = new Custom.Of(sourceMap)
    val expected: Custom.Of[T] = new Custom.Of(expectedMap)
    source.minimize.customs shouldBe expectedMap
    source.toLeaves.customs shouldBe expected.toLeaves.customs
  }
}
