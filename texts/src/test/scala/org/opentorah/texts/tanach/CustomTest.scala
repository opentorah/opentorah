package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Custom.*

final class CustomTest extends AnyFlatSpec, Matchers:

  "Customs" should "load correctly" in:
    Custom.ChayeyOdom.names.hasName("Хаей адам") shouldBe true

  // Ashkenaz and Sefard no longer cover the tree between them: Romania hangs
  // off Common directly, so a map naming only those two is not full.
  "minimize()" should "remove redundant children" in:
    check(
      Map(Ashkenaz -> "X", Chabad -> "X", Sefard -> "X", Romania -> "X"),
      Map(Common -> "X")
    )
    check(
      Map(Ashkenaz -> "X", Sefard -> "X", Romania -> "X"),
      Map(Common -> "X")
    )

  private def check[T](sourceMap: Custom.Customs[T], expectedMap: Custom.Customs[T])(using CanEqual[T, T]): Unit =
    val source: Of[T] = Of[T](sourceMap)
    val expectedMaximized: Custom.Customs[T] = Of(expectedMap).maximize

    val minimized = source.minimize
    val maximized = source.maximize

    given CanEqual[Custom.Customs[T], Custom.Customs[T]] = CanEqual.derived

    minimized.customs shouldBe expectedMap
    maximized shouldBe expectedMaximized
