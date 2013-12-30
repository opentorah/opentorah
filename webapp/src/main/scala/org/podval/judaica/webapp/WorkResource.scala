/*
 *  Copyright 2013 Leonid Dubinsky <dub@podval.org>.
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

import javax.ws.rs.{NotFoundException, PathParam, GET, Path}
import javax.ws.rs.core.{UriInfo, Context}
import org.podval.judaica.viewer.Work
import org.podval.judaica.xml.Html


class WorkResource(work: Work) {

  @GET
  def hello = work.toString  // TODO Metadata!


  @Path("editions")
  @GET
  def editions(@Context uriInfo: UriInfo) = {
    // TODO do a nice, styled table
    val links = work.editions.map{ edition =>
      val name = edition.names.default.name
      val uri = uriInfo.getAbsolutePathBuilder.path(name).build()
      <a href={uri.toString} >{name}</a>
    }
    Html.html("/judaica/judaica", links)   // TODO !!!
  }


  @Path("editions/{edition}")
  def edition(@PathParam("edition") name: String) = {
    val edition = work.getEditionByName(name)
    if (edition.isEmpty) throw new NotFoundException("edition " + name)
    new EditionResource(edition.get)
  }
}
