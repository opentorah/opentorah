package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc.Value],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData.Value],
  revisionDesc: Option[RevisionDesc.Value]
)

object TeiHeader extends Element[TeiHeader]("teiHeader") {

  override val parser: Parser[TeiHeader] = for {
    fileDesc <- FileDesc.required
    encodingDesc <- EncodingDesc.parsable.optional
    profileDesc <- ProfileDesc.optional
    xenoData <- XenoData.parsable.optional
    revisionDesc <- RevisionDesc.parsable.optional
  } yield new TeiHeader(
    fileDesc,
    encodingDesc,
    profileDesc,
    xenoData,
    revisionDesc
  )

  override val antiparser: Antiparser[TeiHeader] = Tei.concat(
    FileDesc.toXml(_.fileDesc),
    EncodingDesc.parsable.toXmlOption(_.encodingDesc),
    ProfileDesc.toXmlOption(_.profileDesc),
    XenoData.parsable.toXmlOption(_.xenoData),
    RevisionDesc.parsable.toXmlOption(_.revisionDesc)
  )

  def apply(): TeiHeader = new TeiHeader(
    fileDesc = FileDesc(),
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
}
