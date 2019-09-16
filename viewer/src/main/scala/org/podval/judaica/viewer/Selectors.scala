package org.podval.judaica.viewer

trait Selectors {

  import Selector.Format

  def selectors: Seq[Selector]

  final def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)

  final def getSelectorByName(name: String): Selector = Names.doFind(selectors, name, "selector")

  final def dominantSelector: Selector = selectors.head

  final def isDominantSelector(selector: Selector): Boolean = selector == dominantSelector

  final def dominantFormat: Format = if (selectors.isEmpty) Nil else dominantSelector +: dominantSelector.dominantFormat

  final def formats: Seq[Format] =
    if (selectors.isEmpty) Seq(Nil) else selectors.flatMap(selector => selector.formats.map (selector +: _))

  final def parseFormat(formatOption: Option[String]): Format = formatOption.fold(dominantFormat)(parseFormat)

  final def parseFormat(format: String): Format =
    Parse.sequence[String, Selectors, Selector](_.getSelectorByName(_)) ((_, selector) => selector) (this, format.split("/").toIndexedSeq)
}
