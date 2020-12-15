package org.opentorah.collectorng

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorTest extends AnyFlatSpec with Matchers {

  "Selector.byName()" should "work" in {
      Selector.byName("inventory")
  }
}
