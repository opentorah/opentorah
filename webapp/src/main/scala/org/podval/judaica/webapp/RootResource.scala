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

import javax.ws.rs.{Produces, GET, Path}
import javax.ws.rs.core.{MediaType, Context, UriInfo}


// TODO I can handle synonyms: {editions} and {editions}/{edition}, and look up the first component - but do I want to?
// TODO where do I show TEI metadata?
// TODO introduce "interface language" - and select the names accordingly ("lang=en" etc.)

@Path("/")
final class RootResource {

  @GET
  @Produces(Array(MediaType.TEXT_HTML))
  def root(@Context uriInfo: UriInfo) = {
    val html =
      <div>
        <p>Welcome to the Judaica Viewer!</p>
        <p>List of available<a href={uriInfo.getAbsolutePathBuilder.path("works").build().toString}>works</a></p>
      </div>

    Html(uriInfo, html)
  }


  @Path("/works")
  def works = new WorksResource


  @Path("/fonts")
  def fonts = new FontsResource
}
