/*
 *  Copyright 2011-2014 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.viewer.{Works, Selection, Edition, Work}

import javax.ws.rs.core.{UriBuilder, UriInfo}

import scala.xml.Elem

import java.io.File


object Html {

  def apply(uriInfo: UriInfo, what: Elem): Elem = apply(uriInfo, Seq(mainStylesheet), what)


  def apply(uriInfo: UriInfo, selection: Selection, what: Elem): Elem = {
    val stylesheets = Seq(mainStylesheet, workStylesheetUri(selection.work)) ++
      selection.editions.editions.map(editionStylesheetUri(_))

    apply(uriInfo, stylesheets, what)
  }


  def apply(uriInfo: UriInfo, stylesheetOptions: Seq[Option[UriBuilder => UriBuilder]], what: Elem): Elem = {
    val stylesheets: Seq[String] =
      stylesheetOptions.flatten.map(f => f(uriInfo.getBaseUriBuilder).path("stylesheet.xml").build().toString)

    <html>
      <head>
        {stylesheets.map(uri => <link rel="stylesheet" type="text/css" href={uri}/>)}
      </head>
      <body>
        {what}
      </body>
    </html>
  }


  private def mainStylesheet: Option[UriBuilder => UriBuilder] =
    ifExists(Works.stylesheet, _.path("works"))


  private def workStylesheetUri(work: Work): Option[UriBuilder => UriBuilder] =
    ifExists(work.stylesheet, _.path("works").path(work.defaultName))


  private def editionStylesheetUri(edition: Edition): Option[UriBuilder => UriBuilder] =
    ifExists(edition.stylesheet, _.path("works").path(edition.work.defaultName).path("editions").path(edition.defaultName))


  private def ifExists[T](file: File, what: T): Option[T] = if (file.exists) Some(what) else None
}
