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

import org.podval.judaica.viewer.{Work, Edition, Editions, Selection}
import javax.ws.rs.{PathParam, Path, GET}
import javax.ws.rs.core.{UriInfo, Context}


final class EditionsResource(work: Work) {

  import EditionsResource._


  @GET
  def editions(@Context uriInfo: UriInfo) =
    Html(uriInfo, Some(work), Table(work.editions, uriInfo, editionsColumn))


  // TODO handle skipped "editions" - use default edition


  @Path("{edition}")
  def selection(@PathParam("edition") editionNames: String) = new ContentSelectionResource(Selection(work, editionNames))
}



object EditionsResource {

  val editionsColumn: LinkColumn[Edition] = new SimpleLinkColumn[Edition]("Editions") {
    override def text(edition: Edition): String = edition.defaultName
  }
}
