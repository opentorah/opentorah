package org.opentorah.collector

import org.opentorah.site.{SiteCommon, SiteService}
import org.opentorah.store.{Alias, Store}
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
      // TODO do not follow redirects; instead, cache the parsed store from a stub that has the URL?
      by: ByHierarchy <- ByHierarchy.followRedirects.required()
      aliases: Seq[Alias] <- Alias.seq()
    yield Collector(
      fromUrl,
      common,
      entities,
      entityLists,
      notes,
      by,
      aliases
    )

    override def unparser: Unparser[Collector] = Unparser.concat[Collector](
      SiteCommon.required(_.common),
      Entities.required(_.entities),
      EntityLists.required(_.entityLists),
      Notes.required(_.notes),
      ByHierarchy.required(_.by),
      Alias.seq(_.aliases)
    )
