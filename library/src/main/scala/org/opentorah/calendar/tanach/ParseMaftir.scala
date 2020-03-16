package org.opentorah.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import org.opentorah.judaica.tanach.Torah
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Torah.Maftir =
    Parser.parseDo(From.xml(maftirElement).parse(maftirParsable(this)))

  protected def maftirElement: Elem

  private def maftirParsable(source: WithNames): Element[Torah.BookSpan] = new Element[Torah.BookSpan](
    elementName = "maftir",
    parser = Torah.spanParser.map(_.resolve.from(this))
  ) {
    override def toXml(value: Torah.BookSpan): Elem = ??? // TODO
  }
}
