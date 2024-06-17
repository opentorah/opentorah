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

  def optional[T](option: Option[T])(f: T => Nodes): Nodes =
    option.fold[Nodes](Seq.empty)(f)

  def conditional(condition: Boolean)(f: => Nodes): Nodes =
    if !condition then Seq.empty else f

  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if Element.is(n) && Element.is(n1) => Seq(n, Atom(separator)) ++ multi(n1 :: ns, separator)
    case n :: ns => Seq(n) ++ multi(ns, separator)
    case n => n
