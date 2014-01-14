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

import org.podval.judaica.viewer.{Work, Works}

import javax.ws.rs.{Produces, PathParam, Path, GET}
import javax.ws.rs.core.{MediaType, UriBuilder, UriInfo, Context}


final class WorksResource {

  import WorksResource._


  @GET
  @Produces(MediaType.TEXT_HTML)
  def works(@Context uriInfo: UriInfo) = Html(uriInfo, Table(Works.works, uriInfo, worksColumn))


  @Path("{work}")
  def work(@PathParam("work") name: String) = new WorkResource(Works.getWorkByName(name))


  @GET
  @Path("/stylesheet.css")
  @Produces("text/css")
  def stylesheet = Works.stylesheet
}



object WorksResource {

  val worksColumn: LinkColumn[Work] = new LinkColumn[Work]("Works") {
    override def link(work: Work, uriBuilder: UriBuilder): UriBuilder = uriBuilder.path(text(work))
    override def text(work: Work): String = work.defaultName
  }
}
