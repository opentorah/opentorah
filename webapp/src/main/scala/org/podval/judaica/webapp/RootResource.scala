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

import org.podval.judaica.xml.Html
import org.podval.judaica.viewer.Works

import javax.ws.rs.{NotFoundException, PathParam, GET, Path}
import javax.ws.rs.core.{Context, UriInfo}

import scala.xml.Elem

import java.io.File


@Path("/")
class RootResource {

  @GET
  def hello = "HELLO!"


  @Path("works")
  @GET
  def works(@Context uriInfo: UriInfo) = {
    val table: Elem = Table.build(Works.get, uriInfo.getAbsolutePathBuilder, Some("editions")) // TODO reuse a constant...
    val stylesheet = uriInfo.getBaseUriBuilder.path("judaica").build().toString
    Html.html(stylesheet, table)
  }


  @Path("judaica.css")
  def css = new File(Works.textsDirectory, "judaica.css")


  @Path("works/{work}")
  def work(@PathParam("work") name: String) = {
    val work = Works.getByName(name)
    if (work.isEmpty) throw new NotFoundException("work " + name)
    new WorkResource(work.get)
  }
}
