package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

final class TeiHeader(
  val fileDesc: FileDesc,
  val encodingDesc: Option[EncodingDesc.Value],
  val profileDesc: Option[ProfileDesc],
  val xenoData: Option[XenoData.Value],
  val revisionDesc: Option[RevisionDesc.Value]
):
  def copy(
    fileDesc: FileDesc = fileDesc,
    encodingDesc: Option[EncodingDesc.Value] = encodingDesc,
    profileDesc: Option[ProfileDesc] = profileDesc,
    xenoData: Option[XenoData.Value] = xenoData,
    revisionDesc: Option[RevisionDesc.Value] = revisionDesc  
  ): TeiHeader = TeiHeader(
    fileDesc,
    encodingDesc,
    profileDesc,
    xenoData,
    revisionDesc
  )

object TeiHeader extends Element[TeiHeader]("teiHeader"):

  override def contentParsable: Parsable[TeiHeader] = new Parsable[TeiHeader]:
    override val parser: Parser[TeiHeader] = for
      fileDesc: FileDesc <- FileDesc.required()
      encodingDesc: Option[EncodingDesc.Value] <- EncodingDesc.element.optional()
      profileDesc: Option[ProfileDesc] <- ProfileDesc.optional()
      xenoData: Option[XenoData.Value] <- XenoData.element.optional()
      revisionDesc: Option[RevisionDesc.Value] <- RevisionDesc.element.optional()
    yield TeiHeader(
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

  def empty: TeiHeader = TeiHeader(
    fileDesc = FileDesc.empty,
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
