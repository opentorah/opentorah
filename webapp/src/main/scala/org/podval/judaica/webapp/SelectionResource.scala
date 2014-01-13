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

import org.podval.judaica.viewer.{Structures, Structure, Editions, Div, Selector}

import javax.ws.rs.{GET, Path, PathParam, QueryParam}
import javax.ws.rs.core.{UriBuilder, UriInfo, Context}


class SelectionResource(editions: Editions, structures: Structures) {

  import SelectionResource._


  @GET
  def structuresOrContent(
    @QueryParam("showContent") showContent: Boolean,
    @Context uriInfo: UriInfo) =
  {
    if (!showContent) {
      Html(uriInfo, Some(editions.work),
        <div>
          {Table(structures.structures, uriInfo, structuresColumn)}
          {Table(structures.deepStructures, uriInfo, contentColumn)}
        </div>
      )
    } else {
      // TODO show content, finally!
      "***** Content View is not yet implemented! *****"
    }
  }


  @GET
  @Path("/{selector}")
  def divs(
    @PathParam("selector") selectorName: String,
    @Context uriInfo: UriInfo) =
  {
    val structure = getStructure(structures, selectorName)
    Html(uriInfo, Some(editions.work), Table(structure.divs, uriInfo, divsColumn))
  }


  @Path("/{selector}/{div}")
  def div(
    @PathParam("selector") selectorName: String,
    @PathParam("div") divName: String,
    @QueryParam("showContent") showContent: Boolean,
    @Context uriInfo: UriInfo) =
  {
    val structure = getStructure(structures, selectorName)
    val div: Div = Exists(getDiv(structure, divName), divName, "div")
    new SelectionResource(editions, div)
  }
}



object SelectionResource {

  private def getStructure(structures: Structures, name: String): Structure =
    Exists(structures.structureByName(name), name, "structure")


  private def getDiv(structure: Structure, divName: String): Option[Div] = {
    if (structure.isNumbered) {
      val divNumber = divName.toInt // TODO format errors
      structure.divByNumber(divNumber)
    } else {
      structure.asNamed.divByName(divName)
    }
  }


  val structuresColumn: LinkColumn[Structure] = new SimpleLinkColumn[Structure]("Structure") {
    override def text(structure: Structure): String = structure.names.default.name
  }


  val contentColumn: LinkColumn[Seq[Selector]] = new LinkColumn[Seq[Selector]]("Content Format") {
    override def link(format: Seq[Selector], uriBuilder: UriBuilder): UriBuilder = uriBuilder.
      queryParam("showContent", true: java.lang.Boolean).
      queryParam("contentFormat", text(format))

    override def text(format: Seq[Selector]): String = format.map(_.names.default.name).mkString("/")
  }


  val divsColumn: LinkColumn[Div] = new SimpleLinkColumn[Div]("Divisions") {
    override def text(div: Div): String = div.id
  }
}
