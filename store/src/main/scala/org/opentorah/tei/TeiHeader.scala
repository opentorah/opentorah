package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
import scala.xml.Elem

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc.Value],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData.Value],
  revisionDesc: Option[RevisionDesc.Value]
)

object TeiHeader extends Element[TeiHeader](
  elementName = "teiHeader",
  parser = for {
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
) with ToXml[TeiHeader] {

  override def toXml(value: TeiHeader): Elem =
    <teiHeader>
      {FileDesc.toXml(value.fileDesc)}
      {EncodingDesc.parsable.toXml(value.encodingDesc)}
      {ProfileDesc.toXml(value.profileDesc)}
      {XenoData.parsable.toXml(value.xenoData)}
      {RevisionDesc.parsable.toXml(value.revisionDesc)}
    </teiHeader>

  def apply(): TeiHeader = new TeiHeader(
    fileDesc = FileDesc(),
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
}
