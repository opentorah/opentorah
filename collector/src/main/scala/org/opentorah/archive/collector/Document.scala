package org.opentorah.archive.collector

import java.io.File
import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Name, Names}
import org.opentorah.store.{Selector, Store}
import org.opentorah.tei.{Availability, CalendarDesc, PublicationStmt, Publisher, SourceDesc, Tei}
import scala.xml.Elem

final class Document(
  inheritedSelectors: Seq[Selector],
  fromUrl: URL,
  val tei: Tei,
  val name: String,
  val translations: Map[String, Tei]
) extends Store(inheritedSelectors, Some(fromUrl), fromUrl) {

  override def names: Names = new Names(Seq(Name(name)))

  override def references: Seq[EntityReference] = tei.references

  override def toXml: Elem = Tei.toXml(Document.removeCommon(tei))
}

object Document {

  def writeDocument(
    document: Document,
    teiDirectory: File,
  ): Unit = {
    Util.teiPrettyPrinter.writeXml(
      new File(teiDirectory, document.name + ".xml"),
      Tei.toXml(addCommon(document.tei))
    )
    for ((language: String, translation: Tei) <- document.translations) Util.teiPrettyPrinter.writeXml(
      new File(teiDirectory, document.name + "-" + language + ".xml"),
      Tei.toXml(addCommon(translation))
    )
  }

  // TODO make more things optional in TEI!
  // TODO remove ad-hoc apply() methods from the TEI package.
  // TODO generate langUsage from xml:lang in the body.

  private def addCommon(tei: Tei): Tei = {
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = new PublicationStmt(
          publisher = Some(new Publisher.Value(<ptr target="www.alter-rebbe.org"/>)),
          availability = Some(new Availability(
            status = Some("free"),
            xml = <licence>
              <ab>
                <ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
                  Creative Commons Attribution 4.0 International License</ref>
              </ab>
            </licence>
          ))
        ),
        sourceDesc = new SourceDesc.Value(<p>Facsimile</p>)
      ),
      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(
        calendarDesc = Some(new CalendarDesc.Value(<calendar xml:id="julian"><p>Julian calendar</p></calendar>))
      ))
    ))
  }

  private def removeCommon(tei: Tei): Tei = {
    tei.copy(teiHeader = tei.teiHeader.copy(
      fileDesc = tei.teiHeader.fileDesc.copy(
        publicationStmt = new PublicationStmt(
          publisher = None,
          availability = None
        ),
        sourceDesc = new SourceDesc.Value(Seq.empty)
      ),
      profileDesc = Some(tei.teiHeader.profileDesc.get.copy(
        calendarDesc = None
      ))
    ))
  }
}
