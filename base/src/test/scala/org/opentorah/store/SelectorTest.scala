package org.opentorah.store

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorTest extends AnyFlatSpec, Matchers:

  "Selector.byName()" should "work" in {
      Selector.byName("inventory")
  }
