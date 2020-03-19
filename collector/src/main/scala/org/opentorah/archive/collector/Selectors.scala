package org.opentorah.archive.collector

import org.opentorah.metadata.{Language, LanguageSpec, Name}
import org.opentorah.store.{Path, Selector}

object Selectors {

  case object Collection extends Selector.Named {
    override val names: org.opentorah.metadata.Names =
      new org.opentorah.metadata.Names(Seq(new Name("дело", LanguageSpec.empty)))
  }

  case object Document extends Selector.Named {
    override val names: org.opentorah.metadata.Names =
      new org.opentorah.metadata.Names(Seq(new Name("документ", LanguageSpec.empty)))
  }

  case object Names extends Selector.Nullary {
    // TODO name of this is no longer affected by what is in the names-lists.xml;
    // when Store comes into play and Selectors are read from there,
    // this will get fixed :)
    override val names: org.opentorah.metadata.Names =
    new org.opentorah.metadata.Names(Seq(new Name("Имена", LanguageSpec.empty)))
  }

  case object Name extends Selector.Named {
    override val names: org.opentorah.metadata.Names =
      new org.opentorah.metadata.Names(Seq(new Name("имя", LanguageSpec.empty)))
  }

  def collectionPath(collectionName: String, reference: String): Path =
    new Path(Seq(Collection.bind(new org.opentorah.metadata.Names(Seq(
      new Name(reference, Language.Russian.toSpec),
      new Name(collectionName, Language.English.toSpec)
    )))))

  def documentPath(documentName: String): Path =
    new Path(Seq(Document.bind(new org.opentorah.metadata.Names(Seq(
    new Name(documentName, Language.Russian.toSpec)
  )))))
}
