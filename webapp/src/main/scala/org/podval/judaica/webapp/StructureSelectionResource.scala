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

import org.podval.judaica.viewer.{Structure, Selector, StructureSelection}

import javax.ws.rs.{Produces, GET, Path, PathParam}
import javax.ws.rs.core.{MediaType, UriBuilder, UriInfo, Context}


class StructureSelectionResource(selection: StructureSelection) {

  import StructureSelectionResource._


  @Path("/content")
  def content = new ContentResource(selection)


  @Path("/{selector}")
  def selector(@PathParam("selector") selectorName: String) = new DivSelectionResource(selection.selectStructure(selectorName))


  @GET
  @Produces(Array(MediaType.TEXT_HTML))
  def structure(@Context uriInfo: UriInfo) = Html(uriInfo,
      <div>
        // TODO make sure the dominant structure is first :)
        {Table(selection.div.structures.values, uriInfo, structuresColumn)}
        {Table(selection.div.formats, uriInfo, contentColumn)}
        {if (selection.editions.isNo) {
        val editionsUri = uriInfo.getBaseUriBuilder.path("works").path(selection.work.defaultName).path("editions").build().toString
        <p>List of available <a href={editionsUri}>editions</a></p>
      }}
      </div>
    )
}



object StructureSelectionResource {

  val structuresColumn: LinkColumn[Structure] = new SimpleLinkColumn[Structure]("Structure") {
    override def text(structure: Structure): String = structure.defaultName
  }


  val contentColumn: LinkColumn[Seq[Selector]] = new LinkColumn[Seq[Selector]]("Content Format") {
    override def link(format: Seq[Selector], uriBuilder: UriBuilder): UriBuilder = uriBuilder.
      path("content").
      queryParam("contentFormat", text(format))

    override def text(format: Seq[Selector]): String = format.map(_.defaultName).mkString("/")
  }
}
