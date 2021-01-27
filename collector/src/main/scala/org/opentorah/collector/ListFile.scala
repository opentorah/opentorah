package org.opentorah.collector

import org.opentorah.util.Files
import org.opentorah.xml.{Element, Elements, Parsable, Parser, PrettyPrinter}
import java.net.URL

final class ListFile[M, W <: AnyRef](
  url: URL,
  name: String,
  entry: Elements[M],
  wrapper: Seq[M] => W
) {
  def write(entries: Seq[M]): Unit = Files.write(
    file = Files.url2file(url),
    content = PrettyPrinter.default.renderXml(list.xmlElement(entries))
  )

  def get: W = Cache.get[W](
    url,
    load = (url: URL) => wrapper(Parser.parseDo(list.parse(url)))
  )

  private val list: Element[Seq[M]] = new Element[Seq[M]](name) {
    override def contentParsable: Parsable[Seq[M]] = entry.seq
  }
}
