package org.opentorah.collector

import org.opentorah.metadata.{Language, WithNames}
import org.opentorah.util.Files
import org.opentorah.xml.{Elements, PrettyPrinter, Xml}

trait Store extends WithNames {
  def acceptsIndexHtml: Boolean = false

  def findByName(name: String): Option[Store] = None

  def displayName: String = names.doFind(Language.Russian.toSpec).name

  def structureName: String = names.doFind(Language.English.toSpec).name
}

object Store {

  type Path = Seq[Store]

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
     so `Store` does *not* extend `FromUrl.With`.
   */
  @scala.annotation.tailrec
  def resolve(
    url: Seq[String],
    store: Store,
    result: Path
  ): Option[Path] = if (url.isEmpty || store.acceptsIndexHtml && (url == Seq("index.html"))) {
    if (result.isEmpty) None else Some(result.reverse)
  } else {
    val next: Option[Store] = store.findByName(url.head)
    // Note: using fold() here breaks tail-recursion:
    if (next.isEmpty) None else resolve(url.tail, next.get, next.get +: result)
  }

  def checkExtension(fullName: String, allowedExtension: String, assumeAllowedExtension: Boolean = false): Option[String] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(fullName)
    if (extension.isDefined && !extension.contains(allowedExtension)) if (assumeAllowedExtension) Some(fullName) else None
    else Some(fileName)
  }

  def findByName(name: String, stores: Seq[Store]): Option[Store] =
    stores.find(_.names.hasName(name))

  def renderXml[T <: Store](elements: Elements[T], value: T): String = renderXml(elements.xmlElement(value))
  def renderXml(xml: Xml.Element): String = prettyPrinter.renderXml(xml)

  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    nestElements = Set("p"),
    alwaysStackElements = Set("store", "by")
  )
}
