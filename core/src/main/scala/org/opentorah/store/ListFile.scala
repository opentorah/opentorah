package org.opentorah.store

import org.opentorah.xml.{ElementsTo, From, Parser}
import java.net.URL

final class ListFile[M, W <: AnyRef](
  url: URL,
  name: String,
  entry: ElementsTo[M],
  wrapper: Seq[M] => W
):
  def get: Parser[W] = Parser.getCachedByUrl[W](
    url,
    load = (url: URL) => entry.wrappedSeq(name).parse(From.url(url)).map(wrapper)
  )
