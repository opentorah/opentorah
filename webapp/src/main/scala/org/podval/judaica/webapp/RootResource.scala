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

import org.podval.judaica.viewer.{Works, Work, Editions, Edition, NoEditions, SingleEdition, LinearEditions}

import javax.ws.rs.{GET, PathParam, Path}
import javax.ws.rs.core.{UriBuilder, Context, UriInfo}

import java.io.File


// TODO declare content-types produced; produce both XML and HTML
// TODO I can handle synonyms: {editions} and {editions}/{edition}, and look up the first component - but do I want to?
// TODO where do I show TEI metadata?

@Path("/")
class RootResource {

  import RootResource._


  @GET
  def root(@Context uriInfo: UriInfo) = {
    val html =
      <div>
        <p>Welcome to the Judaica Viewer!</p>
        <p>List of available<a href={uriInfo.getAbsolutePathBuilder.path("works").build().toString}>works</a></p>
      </div>
    Html(uriInfo, None, html)
  }


  @GET
  @Path("/works")
  def works(@Context uriInfo: UriInfo) = {
    Html(uriInfo, None, Table(Works.works, uriInfo, worksColumn))
  }


  @GET
  @Path("/works/stylesheet.css")
  def mainStylesheet = {
    new File(Works.textsDirectory, "stylesheet.css")
  }


  @GET
  @Path("/works/{work}")
  def work(@PathParam("work") workName: String) = {
    val work = getWork(workName)
    new SelectionResource(new NoEditions(work), work)
  }


  @GET
  @Path("/works/{work}/stylesheet.css")
  def workStylesheet(@PathParam("work") workName: String) = {
    val work = getWork(workName)
    new File(work.directory, "stylesheet.css")
  }


  @GET
  @Path("/works/{work}/editions")
  def editions(@PathParam("work") workName: String, @Context uriInfo: UriInfo) = {
    val work = getWork(workName)
    Html(uriInfo, Some(work), Table(work.editions, uriInfo, editionsColumn))
  }


  // TODO handle skipped "editions" - use default edition


  @Path("/works/{work}/editions/{edition}")
  def selection(
    @PathParam("work") workName: String,
    @PathParam("edition") editionName: String,
    @PathParam("selection") selectionKey: String) =
  {
    val work = getWork(workName)
    new SelectionResource(getEditions(work, editionName), work)
  }
}


object RootResource {

  private def getWork(name: String): Work = Exists(Works.workByName(name), name, "work")


  private def getEditions(work: Work, editionNames: String): Editions = {
    if (editionNames.contains('+')) {
      val names: Seq[String] = editionNames.split('+')
      new LinearEditions(work, names.map(getEdition(work, _)))
      // TODO diff view
      //    } else if (editionNames.contains('-')) {
    } else {
      new SingleEdition(work, getEdition(work, editionNames))
    }
  }


  private def getEdition(work: Work, name: String): Edition = Exists(work.editionByName(name), name, "edition")


  val worksColumn: LinkColumn[Work] = new LinkColumn[Work]("Works") {
    override def link(work: Work, uriBuilder: UriBuilder): UriBuilder = uriBuilder.path(text(work)).path("editions")
    override def text(work: Work): String = work.defaultName
  }


  val editionsColumn: LinkColumn[Edition] = new SimpleLinkColumn[Edition]("Editions") {
    override def text(edition: Edition): String = edition.defaultName
  }
}
