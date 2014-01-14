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

import org.podval.judaica.viewer.{Work, Selection}
import javax.ws.rs.{Path, GET}
import java.io.File


final class WorkResource(work: Work) {

  @Path("/")
  def content = new ContentSelectionResource(Selection(work))


  @GET
  @Path("/stylesheet.css")
  def stylesheet = new File(work.directory, "stylesheet.css")


  @Path("/editions")
  def editions = new EditionsResource(work)
}
