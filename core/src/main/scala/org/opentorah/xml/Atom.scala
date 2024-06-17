package org.opentorah.xml

// Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
type Atom = scala.xml.Atom[?]

object Atom extends Node.Companion[Atom]:
  def apply(text: String): Atom = scala.xml.Text(text)

  override def is(node: Node): Boolean = node.isInstanceOf[Atom]
  override def as(node: Node): Atom = node.asInstanceOf[Atom]
  
  def text(atom: Atom): String = atom.text

  def isWhitespace(node: Node): Boolean = is(node) && text(as(node)).trim.isEmpty
  def isCharacters(node: Node): Boolean = is(node) && text(as(node)).trim.nonEmpty
