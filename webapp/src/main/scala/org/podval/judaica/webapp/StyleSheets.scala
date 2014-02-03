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
 * under the License.
 */

package org.podval.judaica.webapp

import org.podval.judaica.viewer.{Selection, Edition, Work, Works}
import javax.ws.rs.core.{UriInfo, UriBuilder}
import java.io.File
import java.net.URI


object StyleSheets {

  def get(uriInfo: UriInfo): Seq[URI] = resolve(getOptions, uriInfo)


  def get(selection: Selection, uriInfo: UriInfo): Seq[URI] = resolve(getOptions(selection), uriInfo)


  private def getOptions: Seq[Option[UriBuilder => UriBuilder]] = Seq(mainStylesheet)


  private def getOptions(selection: Selection): Seq[Option[UriBuilder => UriBuilder]] =
    Seq(mainStylesheet, workStylesheetUri(selection.work)) ++
    selection.editions.editions.map(editionStylesheetUri(_))


  private def resolve(options: Seq[Option[UriBuilder => UriBuilder]], uriInfo: UriInfo): Seq[URI] = {
      options.flatten.map(f => f(uriInfo.getBaseUriBuilder).path("stylesheet.css").build())
  }


  private def mainStylesheet: Option[UriBuilder => UriBuilder] =
    ifExists(Works.stylesheet, _.path("works"))


  private def workStylesheetUri(work: Work): Option[UriBuilder => UriBuilder] =
    ifExists(work.stylesheet, _.path("works").path(work.defaultName))


  private def editionStylesheetUri(edition: Edition): Option[UriBuilder => UriBuilder] =
    ifExists(edition.stylesheet, _.path("works").path(edition.work.defaultName).path("editions").path(edition.defaultName))


  private def ifExists[T](file: File, what: T): Option[T] = if (file.exists) Some(what) else None
}
