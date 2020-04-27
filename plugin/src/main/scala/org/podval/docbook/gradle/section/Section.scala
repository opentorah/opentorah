package org.podval.docbook.gradle.section

trait Section {

  def name: String

  def usesDocBookXslt2: Boolean = false

  final def xsltVersion: String = if (usesDocBookXslt2) "2.0" else "1.0"

  def defaultParameters: Map[String, String]

  def customStylesheet: String
}

object Section {

  val orphan: List[Section] = List(Common, HtmlCommon)
  val all: List[Section] = orphan ++ DocBook2.all

  def forName(name: String): Section = {
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
