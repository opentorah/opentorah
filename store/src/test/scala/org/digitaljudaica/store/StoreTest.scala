package org.digitaljudaica.store

import java.io.File

import cats.implicits._
import org.digitaljudaica.xml.From
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


final class StoreTest extends AnyFlatSpec with Matchers {

  private val resources: File = new File("./store/src/test/resources/org/digitaljudaica/store").getAbsoluteFile

  "Store.parser" should "work" in {
    From.file(resources, "store").parseDo(Store.parser(Set.empty))

    val result = From.file(resources, "store").parse(Store.parser(Set.empty))
    result.isRight shouldBe true
    val store = result.right.get
    store.by.isDefined shouldBe true
    val by = store.by.get
    by.selector.names.names.length shouldBe 2
  }
}
