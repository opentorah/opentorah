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

import org.podval.judaica.viewer.{Div, StructureSelection}
import javax.ws.rs.{Path, GET, PathParam}
import javax.ws.rs.core.{Context, UriInfo}


final class StructureSelectionResource(selection: StructureSelection) {

  import StructureSelectionResource._


  @GET
  def divs(@Context uriInfo: UriInfo) = Html(uriInfo, Some(selection.work), Table(selection.divs, uriInfo, divsColumn))


  @Path("/{div}")
  def div(@PathParam("div") divName: String) = new ContentSelectionResource(selection.div(divName))
}



object StructureSelectionResource {

  val divsColumn: LinkColumn[Div] = new SimpleLinkColumn[Div]("Divisions") {
    override def text(div: Div): String = div.id
  }
}
