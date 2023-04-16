package org.opentorah.store

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorTest extends AnyFlatSpec, Matchers:

  "Selector.getForName()" should "work" in
      Selector.getForName("inventory")
