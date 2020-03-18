package org.opentorah.archive.collector

import org.opentorah.metadata.Language
import org.opentorah.reference.{Name, Named, NamedsList, Reference}
import org.opentorah.store.Selector
import org.opentorah.util.Collections
import org.opentorah.xml.XmlUtil
import scala.xml.Elem

object ToXml {

  def toXml(collection: Collection): Elem = {
    val url = Site.collectionUrl(collection.name)
    <item>
      <ref target={url} role="collectionViewer">{collection.reference + ": " + XmlUtil.spacedText(collection.title)}</ref><lb/>
      <abstract>{collection.caseAbstract.xml}</abstract>
    </item>
  }

  def toXml(value: NamedsList): Elem =
    <list xml:id={value.id} role={value.role.orNull}>
      <head>{value.head}</head>
      {for (named <- value.nameds) yield {
      val url: String = Site.namedUrl(named.id.get)
      <l><ref target={url} role="namesViewer">{Name.toXml(named.namedNames.head)}</ref></l>
    }}
    </list>
      .copy(label = value.entity.listElement)

  def toXml(
    namesSelector: Selector.Nullary,
    caseSelector: Selector.Named,
    value: Named,
    references: Seq[Reference]
  ): Elem = {
    def sources(viewer: String, references: Seq[Reference]): Seq[Elem] =
      for (source <- Collections.removeConsecutiveDuplicates(references.map(_.source))) yield {
        // TODO move and reuse
        val name = source.last.selectedName(Language.Russian.toSpec)
        val collectionLike = source.init.last

        val url =
          if (collectionLike.getSelector == namesSelector)
            Site.namedUrl(source.last.selectedName(Language.English.toSpec))
          else if (collectionLike.getSelector == caseSelector)
            Site.documentUrl(
              collectionLike.selectedName(Language.English.toSpec),
              source.last.selectedName(Language.English.toSpec))
          else throw new IllegalArgumentException(s"Wrong selector: $collectionLike")

        <ref target={url} role={viewer}>{name}</ref>
      }

    //    // TODO move into Reference
    //    def name(reference: Reference): String = reference.name
    //      .map(_.text.replace("\n", "").trim
    //        .replace("  ", " ")
    //        .replace("  ", " ")
    //        .replace("  ", " ")
    //        .replace("  ", " ")
    //      )
    //      .filterNot(_.isEmpty).mkString(" ")

    def isFromNames(reference: Reference): Boolean =
      reference.source.init.last.getSelector == namesSelector

    val usedBy: Seq[Reference] = references.filter(_.ref.contains(value.id.get))
    val fromNames: Seq[Reference] = usedBy.filter(isFromNames)
    val bySource: Seq[(String, Seq[Reference])] = usedBy.filterNot(isFromNames)
      .groupBy(_.source.init.reference(Language.Russian.toSpec)).toSeq.sortBy(_._1)

    //    val numbers: Seq[(String, Int)] =
    //      usedBy.groupBy(name).mapValues(_.length).toSeq.sortBy(_._2).reverse
    //    <p rendition="usage">
    //      {for ((name, number) <- numbers) yield <l>{s"$name ($number)"}</l>}
    //    </p>

    <named xml:id={value.id.get} role={value.role.orNull}>
      {for (name <- value.namedNames) yield Name.toXml(name)}
      {value.content}
      <p rendition="mentions">
        <ref target={Site.namedInTheListUrl(value.id.get)} role="namesViewer">[...]</ref>
        {if (fromNames.isEmpty) Seq.empty else {
        val names: String = namesSelector.names.doFind(Language.Russian.toSpec).name

        <l>
          <emph>{names}:</emph>
          {
          val result = sources("namesViewer", fromNames)
          result.init.map(elem => <span>{elem},</span>) :+ result.last
          }
        </l>}}
        {for ((source, references) <- bySource) yield <l><emph>{source}:</emph>{sources("documentViewer", references)}</l>}
      </p>
    </named>
      .copy(label = value.entity.element)
  }
}
