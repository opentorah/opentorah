package org.opentorah.texts.tanach

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * equals(Any) must answer false for a value of another type, not throw.
 * `derives CanEqual` makes `==` type-safe at compile time, but collections
 * and test frameworks call equals(Any) reflectively at runtime, where no
 * such check applies.
 */
final class EqualsContractTest extends AnyFlatSpec, Matchers:

  private val samples: Seq[(String, Any)] = Seq(
    "Haftarah"       -> Parsha.Vayeilech.haftarah.doFind(Custom.Ashkenaz),
    "WeeklyReading"  -> WeeklyReading(Parsha.Nitzavim, Some(Parsha.Vayeilech)),
    "Span"           -> Parsha.Vayeilech.span,
    "ChapterAndVerse"-> Parsha.Vayeilech.span.from
  )

  "equals" should "return false rather than throw for another type" in:
    for (label, value) <- samples do
      withClue(s"$label.equals(\"other\"): ")(value.equals("other") shouldBe false)
      withClue(s"$label.equals(42): ")(value.equals(42) shouldBe false)
      withClue(s"$label.equals(null): ")(value.equals(null) shouldBe false)

  it should "let a collection hold values of mixed type" in:
    for (label, value) <- samples do
      // Set.contains calls equals(Any) on the stored element
      withClue(s"Set($label).contains(\"other\"): ")(
        Set[Any](value).contains("other") shouldBe false)
