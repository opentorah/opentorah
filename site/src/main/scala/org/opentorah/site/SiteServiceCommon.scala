package org.opentorah.site

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

// TODO if I ever figure out how to call nested Scala object from a Gradle build file,
// this (together with Site.Common) could move into SiteService companion object...
object SiteServiceCommon extends SiteService[Site.Common]:

  override def projectId: String = "???"

  override def bucketName: String = "???"

  override def contentParsable: Parsable[Site.Common] = new Parsable[Site.Common]:
    override def parser: Parser[Site.Common] = for
      fromUrl <- Element.currentFromUrl
      common <- SiteCommon.required()
    yield Site.Common(
      fromUrl,
      common
    )

    override def unparser: Unparser[Site.Common] = Unparser.concat[Site.Common](
      SiteCommon.required(_.common),
    )
