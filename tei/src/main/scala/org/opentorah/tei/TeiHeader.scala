package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc.Value],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData.Value],
  revisionDesc: Option[RevisionDesc.Value]
)

object TeiHeader extends Element.WithToXml[TeiHeader]("teiHeader") {

  override protected val parser: Parser[TeiHeader] = for {
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

  override protected val antiparser: Antiparser[TeiHeader] = Antiparser(
    FileDesc.elementAntiparser.compose(_.fileDesc),
    EncodingDesc.parsable.elementAntiparserOption.compose(_.encodingDesc),
    ProfileDesc.elementAntiparserOption.compose(_.profileDesc),
    XenoData.parsable.elementAntiparserOption.compose(_.xenoData),
    RevisionDesc.parsable.elementAntiparserOption.compose(_.revisionDesc)
  )

  def apply(): TeiHeader = new TeiHeader(
    fileDesc = FileDesc(),
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
}
