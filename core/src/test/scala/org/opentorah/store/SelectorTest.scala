package org.opentorah.store

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SelectorTest extends AnyFlatSpec, Matchers:

  "Selector.getForName()" should "work" in:
    val inventory: Selector = Selector.getForName("inventory")
    inventory.names.hasName("inventory") shouldBe true
    Selector.getForName("names").title.contains("Имена") shouldBe true
    Selector.getForName("parsha").names.hasName("parsha") shouldBe true
