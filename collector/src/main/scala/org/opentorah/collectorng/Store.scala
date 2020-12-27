package org.opentorah.collectorng

import org.opentorah.metadata.{Language, WithNames}
import org.opentorah.util.Files

trait Store extends WithNames {
  def findByName(name: String): Option[Store]
}

object Store {

  type Path = Seq[Store]

  def toEnglishUrl(path: Path): String = toUrl(path, Language.English)
  def toRussianUrl(path: Path): String = toUrl(path, Language.Russian)

  def toUrl(path: Path, language: Language): String =
    Files.mkUrl(path.map(_.names.doFind(language.toSpec).name))

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Components`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Components` are read from XML - some are constructed -
     so `Component` does *not* extend `FromUrl.With`.

    TODO accept 'index.html' as empty url - when isDirectory()
   */
  @scala.annotation.tailrec
  def resolve(
    url: Seq[String],
    store: Store,
    result: Path
  ): Option[Path] = if (url.isEmpty) {
    if (result.isEmpty) None else Some(result.reverse)
  } else {
    val next: Option[Store] = store.findByName(url.head)
    // Note: using fold() here breaks tail-recursion:
    if (next.isEmpty) None else resolve(url.tail, next.get, next.get +: result)
  }

  def checkExtension(fullName: String, allowedExtension: String): Option[String] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(fullName)
    if (extension.isDefined && !extension.contains(allowedExtension)) None
    else Some(fileName)
  }

  def findByName(name: String, stores: Seq[Store]): Option[Store] =
    stores.find(_.names.hasName(name))
}
