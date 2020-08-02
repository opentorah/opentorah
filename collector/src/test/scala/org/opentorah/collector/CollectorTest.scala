package org.opentorah.collector

import org.opentorah.store.ComponentNg
import org.opentorah.util.Files
import org.opentorah.xml.Parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.podval.tools.run.Run

class CollectorTest extends AnyFlatSpec with Matchers {

  // TODO do the new By and then EntitiesNg...
  "CollectorStore" should "work" ignore {
    val storeFile = Files.file(Run.getProjectRoot, Seq("collector", "src", "test", "resources", "redirect1.xml"))
    val result = Parser.parseDo(ComponentNg.read(CollectorStore.elementName, Files.file2url(storeFile)))
  }
}
