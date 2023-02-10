package org.opentorah.docbook

import org.opentorah.xml.{Dialect, Doctype, Namespace, PrettyPrinter}

object DocBook extends Dialect, Doctype:

  override val namespace: Namespace = Namespace(uri = "http://docbook.org/ns/docbook", prefix = "db")

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  override val doctype: String = doctypeString("article")

  def doctype(rootElementName: String): Doctype = new Doctype:
    override def doctype: String = doctypeString(rootElementName)

  private def doctypeString(rootElementName: String): String = Doctype.string(
    rootElementName,
    dtdId,
    dtdUri = "https://docbook.org/xml/5.0/dtd/docbook.dtd"
  )

  val version: String = "5.0"

  val equationElements: Set[String] = Set("equation", "informalequation", "inlineequation")

  val inlineEquationElements: Set[String] = Set("inlineequation")

  // Elements that contain code
  val codeElements: Set[String] = Set()

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements =
      Set("book", "part", "article", "para") ++ //"itemizedlist",
      equationElements ++
      Seq("math", "mrow", "mi"), // TODO from MathML
    clingyElements = Set("footnote")
  )

