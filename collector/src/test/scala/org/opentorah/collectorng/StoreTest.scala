package org.opentorah.collectorng

import org.opentorah.xml.{From, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

final class StoreTest extends AnyFlatSpec with Matchers {

  "RootStore" should "parse" ignore {
    val result = Parser.parseDo(RootStore.parse(From.file(new File(
      "/home/dub/OpenTorah/alter-rebbe.org/store"), "storeng")))

    result.by.stores.head.names.name shouldBe "книги"
  }
}
