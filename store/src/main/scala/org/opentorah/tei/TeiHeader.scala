package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class TeiHeader(
  fileDesc: FileDesc,
  encodingDesc: Option[EncodingDesc.Value],
  profileDesc: Option[ProfileDesc],
  xenoData: Option[XenoData.Value],
  revisionDesc: Option[RevisionDesc.Value]
)

object TeiHeader extends Element.WithToXml[TeiHeader]("teiHeader") {

  override protected def parser: Parser[TeiHeader] = for {
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

  override protected def attributes(value: TeiHeader): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: TeiHeader): Seq[Elem] =
    Seq(FileDesc.toXml(value.fileDesc)) ++
    EncodingDesc.parsable.toXml(value.encodingDesc) ++
    ProfileDesc.toXml(value.profileDesc) ++
    XenoData.parsable.toXml(value.xenoData) ++
    RevisionDesc.parsable.toXml(value.revisionDesc)

  def apply(): TeiHeader = new TeiHeader(
    fileDesc = FileDesc(),
    encodingDesc = None,
    profileDesc = None,
    xenoData = None,
    revisionDesc = None
  )
}
