package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc.Value],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData.Value],
  revisionDesc: Option[RevisionDesc.Value]
)

object TeiHeader extends Element[TeiHeader]("teiHeader") {

  override def contentParsable: Parsable[TeiHeader] = new Parsable[TeiHeader] {
    override val parser: Parser[TeiHeader] = for {
      fileDesc <- FileDesc.required()
      encodingDesc <- EncodingDesc.element.optional()
      profileDesc <- ProfileDesc.optional()
      xenoData <- XenoData.element.optional()
      revisionDesc <- RevisionDesc.element.optional()
    } yield new TeiHeader(
      fileDesc,
      encodingDesc,
      profileDesc,
      xenoData,
      revisionDesc
    )

    override val unparser: Unparser[TeiHeader] = Tei.concat(
      FileDesc.required(_.fileDesc),
      EncodingDesc.element.optional(_.encodingDesc),
      ProfileDesc.optional(_.profileDesc),
      XenoData.element.optional(_.xenoData),
      RevisionDesc.element.optional(_.revisionDesc)
    )
  }

  def empty: TeiHeader = new TeiHeader(
    fileDesc = FileDesc.empty,
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
}
