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

import org.podval.judaica.xml.Html
import org.podval.judaica.viewer.{Structure, Edition, Div}

import javax.ws.rs.{PathParam, GET, Path}
import javax.ws.rs.core.{UriInfo, Context}


class SelectionResource(edition: Edition) {

  @GET
//  @Path("/")
  def metadata = {
    // TODO Metadata!
    edition.toString
  }


  @GET
  @Path("/{selector}")
  def structure(@PathParam("selector") selectorName: String, @Context uriInfo: UriInfo) = {
    val table = Table.build(getStructure(selectorName).divs, (div: Div) => div.id, uriInfo.getAbsolutePathBuilder, None)
    val stylesheet = uriInfo.getBaseUriBuilder.path("judaica").build().toString
    Html.html(stylesheet, table)
  }


  @GET
  @Path("/{selector}/{name}")
  def selection(@PathParam("selector") selectorName: String, @PathParam("name") name: String) = {
    val structure = getStructure(selectorName)
//    val div = Existence.verify(structure.byName(name), name, structure.names.default.name)
    // TODO introduce some kind of parameter to distinguish request for the book and its display; use it here to retrieve metadata?
/////    val content = edition.storage.content(structure.type_, name)
    "QQ!"
  }


  private[this] def getStructure(name: String): Structure = Existence.verify(edition.work.structureByName(name), name, "structure")
}
