package org.opentorah.docbook.section

trait CommonSection extends Section

object CommonSection {
  val all: List[CommonSection] = List(Common, HtmlCommon)

  def forName(name: String): CommonSection = {
    all.find(_.name.equalsIgnoreCase(name)).getOrElse {
      val sections: String = DocBook2.all.map { docBook2 =>
        "  " + docBook2.name + ": " + docBook2.parameterSections.map(_.name).mkString(", ")
      }.mkString("\n")

      throw new IllegalArgumentException(
        s"""Unsupported section $name;
           |supported sections are:
           |$sections
           |""".stripMargin
      )
    }
  }
}
