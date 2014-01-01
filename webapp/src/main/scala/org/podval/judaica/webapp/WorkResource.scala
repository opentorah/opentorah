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

import org.podval.judaica.viewer.Work
import org.podval.judaica.xml.Html

import javax.ws.rs.{PathParam, GET, Path}
import javax.ws.rs.core.{UriInfo, Context}


class WorkResource(work: Work) {

  @GET
  def hello = work.toString  // TODO Metadata!


  // TODO I can handle synonyms: {editions} and {editions}/{edition}, and look up the first component - but do I want to?


  @Path("editions")
  @GET
  def editions(@Context uriInfo: UriInfo) = {
    val table = Table.build(work.editions, uriInfo.getAbsolutePathBuilder, None)
    val stylesheet = uriInfo.getBaseUriBuilder.path("judaica").build().toString
    Html.html(stylesheet, table)
  }


  @Path("editions/{edition}")
  def edition(@PathParam("edition") name: String) = new EditionResource(Existence.verify(work.getEditionByName(name), name, "edition"))


  // TODO handle skipped "editions" - use default edition

  // TODO introduce some kind of parameter to distinguish request for the book and its structure; use it here to retrieve metadata?
}
