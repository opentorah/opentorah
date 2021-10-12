package org.opentorah.texts

final class TextTest extends TestBase(Text):

  "Text" should "contain /Tanach" in {
    checkName("/Tanach", "Танах")
  }
