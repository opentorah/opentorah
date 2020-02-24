package org.podval.calendar.tanach

import org.digitaljudaica.metadata.WithNames
import org.digitaljudaica.xml.{ContentType, From, Parser, Xml}
import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir
import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Maftir =
    Parser.parseDo(From.xml(maftirElement).parse(ContentType.Elements,
      Xml.withName("maftir", Torah.spanParser.map(_.resolve.from(this)))))

  protected def maftirElement: Elem
}
