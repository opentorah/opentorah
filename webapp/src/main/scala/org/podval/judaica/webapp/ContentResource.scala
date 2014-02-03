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

import org.podval.judaica.viewer.{StructureSelection, Content}
import org.podval.judaica.xml.Xml

import javax.ws.rs.{Produces, QueryParam, GET}
import javax.ws.rs.core.{UriInfo, Context, MediaType}


final class ContentResource(selection: StructureSelection) {

  @GET
  @Produces(Array(MediaType.APPLICATION_XML))
  def xml(@QueryParam("contentFormat") format: String, @Context uriInfo: UriInfo): String = {
    val xml = Content.toXmlNode(selection.content(Option.apply(format)))
    val styleSheets = StyleSheets.get(selection, uriInfo)

    // Link HTTP Headres are not supported by Chrome; I have to resort to processing instructions to associate stylesheets with my XML...
//    val links = styleSheets.map(u => Link.fromUri(u).rel("stylesheet").`type`("text/css").build())
//    val result = Response.ok(xml).links(links: _*)
//    result.build()

    val result = new StringBuilder

    styleSheets.foreach(s => result.append(s"""<?xml-stylesheet href="${s.toString}" type="text/css"?>\n"""))
    result.append(Xml.prettyPrinter.format(xml))
    result.append("\n")

    result.toString
  }
}
