package org.opentorah.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import org.opentorah.judaica.tanach.Torah
import org.opentorah.judaica.tanach.Torah.Maftir
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Maftir =
    Parser.parseDo(From.xml(maftirElement).parse(
      Element.withName("maftir", Torah.spanParser.map(_.resolve.from(this)))))

  protected def maftirElement: Elem
}
