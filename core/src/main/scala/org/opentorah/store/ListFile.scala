package org.opentorah.store

import org.opentorah.util.Files
import org.opentorah.xml.{ElementTo, ElementsTo, From, Parser, PrettyPrinter}
import java.net.URL

final class ListFile[M, W <: AnyRef](
  url: URL,
  name: String,
  entry: ElementsTo[M],
  wrapper: Seq[M] => W
):
  def write(entries: Seq[M]): Unit = Files.write(
    file = Files.url2file(url),
    content = PrettyPrinter.default.renderWithHeader(element = list.xmlElement(entries))
  )

  def get: Parser[W] = Parser.getCachedByUrl[W](
    url,
    load = (url: URL) => list.parse(From.url(url)).map(wrapper)
  )

  private def list: ElementTo[Seq[M]] = entry.wrappedSeq(name)
