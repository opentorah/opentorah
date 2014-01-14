/*
 *  Copyright 2014 Leonid Dubinsky <dub@podval.org>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.judaica.webapp

import org.podval.judaica.viewer.{Structure, Selector, ContentSelection}

import javax.ws.rs.{GET, Path, PathParam, QueryParam}
import javax.ws.rs.core.{UriBuilder, UriInfo, Context}


class ContentSelectionResource(selection: ContentSelection) {

  import ContentSelectionResource._


  @GET
  def structuresOrContent(
    @QueryParam("showContent") showContent: Boolean,
    @QueryParam("contentFormat") format: String,
    @Context uriInfo: UriInfo) =
    if (!showContent || selection.editions.isNo) structure(uriInfo) else content(format, uriInfo)


  // TODO remove duplication: in the column, here and in the Html (edition stylesheet)
  private[this] def structure(uriInfo: UriInfo) = Html(uriInfo, Some(selection.work),
      <div>
        {Table(selection.structures.structures, uriInfo, structuresColumn)}
        {Table(selection.structures.deepStructures, uriInfo, contentColumn)}
        {if (selection.editions.isNo) {
        val editionsUri = uriInfo.getBaseUriBuilder.path("works").path(selection.work.defaultName).path("editions").build().toString
        <p>List of available <a href={editionsUri}>editions</a></p>
      }}
      </div>
    )


  private[this] def content(format: String, uriInfo: UriInfo) = {
    // TODO show content, finally!
    // TODO thread the "format" here
    "***** Content View is not yet implemented! *****"
  }


  @Path("/{selector}")
  def selector(@PathParam("selector") selectorName: String) =
    new StructureSelectionResource(selection.structure(selectorName))
}



object ContentSelectionResource {

  val structuresColumn: LinkColumn[Structure] = new SimpleLinkColumn[Structure]("Structure") {
    override def text(structure: Structure): String = structure.defaultName
  }


  val contentColumn: LinkColumn[Seq[Selector]] = new LinkColumn[Seq[Selector]]("Content Format") {
    override def link(format: Seq[Selector], uriBuilder: UriBuilder): UriBuilder = uriBuilder.
      queryParam("showContent", true: java.lang.Boolean).
      queryParam("contentFormat", text(format))

    override def text(format: Seq[Selector]): String = format.map(_.defaultName).mkString("/")
  }
}
