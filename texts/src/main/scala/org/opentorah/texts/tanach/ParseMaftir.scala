package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Torah.Maftir = Parser.parseDo(
    new Element[Torah.BookSpan](
      elementName = "maftir",
      parser = Torah.spanParser.map(_.resolve.from(this))
    ).parse(From.xml("Maftir", maftirElement))
  )

  protected def maftirElement: Elem
}
