package org.digitaljudaica.store

import java.io.File

import cats.implicits._
import org.digitaljudaica.xml.{Context, Load}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


final class StoreTest extends AnyFlatSpec with Matchers {

  private val resources: File = new File("src/test/resources/org/digitaljudaica/store").getAbsoluteFile

  "Store.parser" should "work" in {
    val result = Context.parse(Load.fromFile(resources, "store"), Store.parser(Set.empty))
    result.isRight shouldBe true
    val store = result.right.get
    store.by.isDefined shouldBe true
    val by = store.by.get
    by.selector.names.names.length shouldBe 2
  }
}
