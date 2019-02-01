package org.podval.docbook.gradle

import org.scalatest.{FunSpec, Matchers}

class SaxonTest extends FunSpec with Matchers  {
  describe("Saxon") {
    val version: String = "blah-blah"
//    SaxonUtil.toHtml("DocBookWithSubstitutions.xml", Map("date" -> version)).contains(version) shouldBe false
//    SaxonUtil.toHtml("DocBookWithSubstitutions.xml", Map("version" -> version)).contains(version) shouldBe true
    println(SaxonUtil.toHtml("DocBookWithSubstitutions.xml", Map("version" -> version)))
  }
}
