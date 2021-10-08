package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Named, Names}
import org.opentorah.store.{Store, Stores}

// TODO maybe make Chapters.By and TanachBook.By more explicit? Stores.by(Selector)?
// TODO add names for Tanach and its parts (Nach, Writings, TreiAsar) and corresponding Stores
// TODO make Tanach a Store in Texts!
object Tanach extends Stores.Pure[Store]:
  override def storesPure: Seq[Store] = Seq(TanachBook)

  enum Part(nameOverride: Option[String] = None)
    extends Named.ByLoader[Part](loader = Part, nameOverride), HasName.Enum:

    case All
    case Chumash
    case Nach
    case Prophets
    case EarlyProphets extends Part(nameOverride = Some("Early Prophets"))
    case LateProphets extends Part(nameOverride = Some("Late Prophets"))
    case TreiAsar extends Part(nameOverride = Some("Trei Asar"))
    case Writings

  object Part extends Names.Loader[Part]:
    override val valuesSeq: Seq[Part] = values.toIndexedSeq