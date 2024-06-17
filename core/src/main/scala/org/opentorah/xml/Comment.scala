package org.opentorah.xml

type Comment = scala.xml.Comment

object Comment extends Node.Companion[Comment]:
  def apply(text: String): Comment = scala.xml.Comment(s" $text ")

  override def is(node: Node): Boolean = node.isInstanceOf[Comment]
  override def as(node: Node): Comment = node.asInstanceOf[Comment]
