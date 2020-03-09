package org.opentorah.tei

import org.opentorah.xml.Descriptor
import scala.xml.Elem

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData],
  revisionDesc: Option[RevisionDesc]
)

object TeiHeader extends Descriptor[TeiHeader](
  elementName = "teiHeader",
  parser = for {
    fileDesc <- FileDesc.required
    encodingDesc <- EncodingDesc.optional
    profileDesc <- ProfileDesc.optional
    xenoData <- XenoData.optional
    revisionDesc <- RevisionDesc.optional
  } yield new TeiHeader(
    fileDesc,
    encodingDesc,
    profileDesc,
    xenoData,
    revisionDesc
  )
) {

  def apply(fileDesc: FileDesc): TeiHeader = new TeiHeader(
    fileDesc,
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )

  override def toXml(value: TeiHeader): Elem =
    <teiHeader>
      {FileDesc.toXml(value.fileDesc)}
      {EncodingDesc.toXml(value.encodingDesc)}
      {ProfileDesc.toXml(value.profileDesc)}
      {XenoData.toXml(value.xenoData)}
      {RevisionDesc.toXml(value.revisionDesc)}
    </teiHeader>
}
