package org.opentorah.collector

import org.opentorah.site.{SiteCommon, SiteService}
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

object CollectorService extends SiteService[Collector]:

  override def projectId: String = "alter-rebbe-2"

  override def bucketName: String = "store.alter-rebbe.org"

  override def contentParsable: Parsable[Collector] = new Parsable[Collector]:
    override def parser: Parser[Collector] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      common: SiteCommon <- SiteCommon.required()
      entities: Entities <- Entities.required()
      entityLists: EntityLists <- EntityLists.required()
      notes: Notes <- Notes.required()
      by: ByHierarchy <- ByHierarchy.followRedirects.required()
    yield Collector(
      fromUrl,
      common,
      entities,
      entityLists,
      notes,
      by
    )

    override def unparser: Unparser[Collector] = Unparser.concat[Collector](
      SiteCommon.required(_.common),
      Entities.required(_.entities),
      EntityLists.required(_.entityLists),
      Notes.required(_.notes),
      ByHierarchy.required(_.by)
    )
