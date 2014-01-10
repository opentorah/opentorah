/*
 *  Copyright 2013-2014 Leonid Dubinsky <dub@podval.org>.
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
import org.podval.judaica.viewer.{Edition, Work, Works}

import javax.ws.rs.{PathParam, GET, Path}
import javax.ws.rs.core.{Context, UriInfo}

import scala.xml.Elem

import java.io.File


// TODO declare content-types produced; produce both XML and HTML
// TODO I can handle synonyms: {editions} and {editions}/{edition}, and look up the first component - but do I want to?
// TODO where do I show TEI metadata?

@Path("/")
class RootResource {

  @GET
  def root = "HELLO!"


  @GET
  @Path("/judaica.css")
  def css = new File(Works.textsDirectory, "judaica.css")


  @GET
  @Path("/works")
  def works(@Context uriInfo: UriInfo) = {
    val table: Elem = Table.build(Works.works, (work: Work) => work.names.default.name, uriInfo.getAbsolutePathBuilder, Some("editions"))
    val stylesheet = uriInfo.getBaseUriBuilder.path("judaica").build().toString
    Html.html(stylesheet, table)
  }


  @GET
  @Path("/works/{work}")
  def work(@PathParam("work") workName: String): String = {
    val work = getWork(workName)
    // TODO Metadata!
    work.toString
  }


  @GET
  @Path("/works/{work}/editions")
  def editions(@PathParam("work") workName: String, @Context uriInfo: UriInfo) = {
    val work = getWork(workName)
    val table = Table.build(work.editions, (edition: Edition) => edition.names.default.name, uriInfo.getAbsolutePathBuilder, None)
    // TODO cascade to the Work's stylesheet if it exists...
    val stylesheet = "/judaica" // TODO uriInfo.getBaseUriBuilder.path("judaica").build().toString
    Html.html(stylesheet, table)
  }


  // TODO handle skipped "editions" - use default edition


  @Path("/works/{work}/editions/{edition}")
  def selection(@PathParam("work") workName: String, @PathParam("edition") editionName: String, @PathParam("selection") selectionKey: String) = {
    val edition = getEdition(workName, editionName)
    new SelectionResource(edition)
  }


  private[this] def getWork(name: String): Work = Existence.verify(Works.workByName(name), name, "work")


  private[this] def getEdition(workName: String, editionName: String): Edition = getEdition(getWork(workName), editionName)


  private[this] def getEdition(work: Work, name: String): Edition = Existence.verify(work.editionByName(name), name, "edition")
}
