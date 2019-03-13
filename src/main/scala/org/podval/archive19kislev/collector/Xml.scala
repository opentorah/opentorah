package org.podval.archive19kislev.collector

import java.io.{File, FileWriter, PrintWriter, Writer}

import scala.xml.{Elem, Node, PrettyPrinter, Utility, XML}

object Xml {

  implicit class Ops(elem: Elem) {

    def elems(plural: String, singular: String, required: Boolean = true): Seq[Elem] =
      oneOptionalChild(plural, required).map(_.elems(singular)).getOrElse(Seq.empty)

    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }

    def elemsFilter(name: String): Seq[Elem] = elem.elems.filter(_.label == name)

    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def getAttribute(name: String): String = attributeOption(name).getOrElse(throw new NoSuchElementException(s"No attribute $name"))

    def attributeOption(name: String): Option[String] = {
      val result: Seq[Node] = elem \ ("@" + name)
      if (result.isEmpty) None else Some(result.text)
    }

    def intAttributeOption(name: String): Option[Int] = attributeOption(name).map { value =>
      try { value.toInt } catch { case e: NumberFormatException => throw new IllegalArgumentException(s"$value is not a number", e)}
    }

    def intAttribute(name: String): Int = intAttributeOption(name).getOrElse(throw new NoSuchElementException(s"No attribute $name"))

    def booleanAttribute(name: String): Boolean = {
      val value = attributeOption(name)
      value.isDefined && ((value.get == "true") || (value.get == "yes"))
    }

    def oneChild(name: String): Elem = oneOptionalChild(name, required = true).get
    def optionalChild(name: String): Option[Elem] = oneOptionalChild(name, required = false)

    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      if (children.size > 1) throw new NoSuchElementException(s"To many children with name $name")
      if (required && children.isEmpty) throw new NoSuchElementException(s"No child with name $name")

      if (children.isEmpty) None else Some(children.head.asInstanceOf[Elem])
    }

    def check(name: String): Elem = {
      if (elem.label != name) throw new NoSuchElementException(s"Expected name $name but got $elem.label")
      elem
    }

    def isDiv(divType: String): Boolean = (elem.label == "div") && (elem.getAttribute("type") == divType)
  }

  def loadResource(clazz: Class[_], name: String, tag: String): Elem =
    open(XML.load(clazz.getResourceAsStream(name + ".xml")), tag)

  def load(file: File, tag: String): Elem = open(load(file), tag)

  def load(file: File): Elem = Utility.trimProper(XML.loadFile(file)).asInstanceOf[Elem]

  def open(what: Elem, tag: String): Elem = what/*(0).asInstanceOf[Elem]*/.check(tag)

  def print(xml: Node, outFile: File, prefix: Seq[String] = Seq.empty): Unit = print(xml, new FileWriter(outFile), prefix)

  private def print(xml: Node, writer: Writer, prefix: Seq[String]) {
    val out = new PrintWriter(writer)

    prefix.foreach(out.println)

    val pretty = prettyPrinter.format(xml)
    // TODO when outputting XML, include <xml> header?
    out.println(pretty)
    out.close()
  }

  private val prettyPrinter = new PrettyPrinter(120, 2)
}
