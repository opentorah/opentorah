package org.opentorah.store

import org.opentorah.util.Collections
import org.opentorah.xml.Parser

abstract class By[+S <: Store](
  inheritedSelectors: Seq[Selector],
  urls: Urls
) extends ComponentBase(inheritedSelectors, urls) {

  def selector: Selector

  final lazy val stores: Seq[S] = Parser.parseDo(Parser.collectAll(load))

  final lazy val siblings: Map[Store, (Option[S], Option[S])] =
    Collections.prevAndNext(stores).toMap

  protected def load: Seq[Parser[S]]
}

object By extends ByComponent

