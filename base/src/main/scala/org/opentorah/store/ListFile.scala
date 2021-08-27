package org.opentorah.store

import org.opentorah.util.Files
import org.opentorah.xml.{Element, Elements, PrettyPrinter, ScalaXml}
import java.net.URL

final class ListFile[M, W <: AnyRef](
  url: URL,
  name: String,
  entry: Elements[M],
  wrapper: Seq[M] => W
) {
  def write(entries: Seq[M]): Unit = Files.write(
    file = Files.url2file(url),
    content = PrettyPrinter.default.renderWithHeader(ScalaXml)(list.xmlElement(entries))
  )

  def get: Caching.Parser[W] = Caching.getCached[W](
    url,
    load = (url: URL) => list.parse(url).map(wrapper)
  )

  private def list: Element[Seq[M]] = entry.wrappedSeq(name)
}
