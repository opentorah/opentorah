package org.opentorah.store

import org.opentorah.xml.{ContentType, From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class StoreTest extends AnyFlatSpec with Matchers {

  "Store.parser" should "work" ignore {
    val result = Parser.parseOrError(From.resource(Store, "store").parse(ContentType.Mixed, Store.parser(Set.empty)))
    println(result)
    result.isRight shouldBe true
    val store = result.right.get
    store.by.isDefined shouldBe true
    val by = store.by.get
    by.selector.names.names.length shouldBe 2
  }
}
