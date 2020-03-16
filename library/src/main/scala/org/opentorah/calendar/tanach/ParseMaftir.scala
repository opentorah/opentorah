package org.opentorah.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import org.opentorah.judaica.tanach.Torah
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Torah.Maftir =
    Parser.parseDo(From.xml("Maftir", maftirElement)
      .parse(new Element[Torah.BookSpan](
        elementName = "maftir",
        parser = Torah.spanParser.map(_.resolve.from(this))
      ))
    )

  protected def maftirElement: Elem
}
