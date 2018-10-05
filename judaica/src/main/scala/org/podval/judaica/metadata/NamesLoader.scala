package org.podval.judaica.metadata

import scala.xml.Elem

trait NamesLoader extends Named {

  // TODO turn loadNames into a def (in Metadata); define toNames separately in Named thingies...

  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  private lazy val toMetadata: Map[Key, Names] = {
    val metadataElements: Seq[Elem] = Metadata.loadMetadataElements(this, Named.className(this), "names")
    Metadata.bind(values, metadataElements.map(element => Names.parse(element, None)))
  }

  trait KeyBase extends Named.NamedBase { this: Key =>
    final override def names: Names = toMetadata(this)
  }

  override type Key <: KeyBase
}
