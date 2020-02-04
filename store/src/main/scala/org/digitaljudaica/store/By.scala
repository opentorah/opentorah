package org.digitaljudaica.store

import java.io.File

import org.digitaljudaica.store.util.Files

sealed trait By {
  def selector: Selector
  def stores: Seq[Store]
}

final class BaseBy(
  override val selector: Selector,
  override val stores: Seq[Store]
) extends By

final class TextsBy(
  override val selector: Selector,
  url: String
) extends By {
  private var storesCache: Option[Seq[TextStore]] = None

  override def stores: Seq[TextStore] = {
    if (storesCache.isEmpty) storesCache = Some(load)
    storesCache.get
  }

  private def load: Seq[TextStore] = { // TODO remote file enumeration? Pre-generated listings?
    for { fileName <- Files.filesWithExtensions(new File(url), ".xml").sorted }
    yield new TextStore(fileName, s"$url/$fileName.xml")
  }
}
