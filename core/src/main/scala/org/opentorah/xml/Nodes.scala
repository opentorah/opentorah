package org.opentorah.xml

type Nodes = Seq[Node]

object Nodes:
  // Note: just for case matching of Nil against Nodes...
  given CanEqual[scala.collection.immutable.Nil.type, Nodes] = CanEqual.derived

  def toString(nodes: Nodes): String = nodes.map(Node.toString).mkString(" ")

  def isEmpty(nodes: Nodes): Boolean = nodes.forall(Atom.isWhitespace)

  def descendants(nodes: Nodes, elementName: String): Nodes = nodes.flatMap(node => node.flatMap(_ \\ elementName))

  val all: Parsable[Nodes] = new Parsable[Nodes]:
    override protected def parser: Parser[Nodes] = ParserState.accessZIO(_.nodes)
    override def unparser: Unparser[Nodes] = Unparser[Nodes](content = identity)
