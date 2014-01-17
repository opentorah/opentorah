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

package org.podval.judaica.viewer

import org.podval.judaica.xml.XmlFile
import scala.xml.Elem
import java.io.File


class ViewerException(message: String, cause: Throwable = null) extends Exception(message, cause)


class NotFoundException(what: String, name: String) extends ViewerException(s"$what $name not found")


class ParseException(file: File, cause: ViewerException) extends Exception(s"In file $file: " + cause.getMessage, cause)



object ParseException {

  def withMetadataFile[T](file: File)(body: Elem => T): T = withFile(file)(body(XmlFile.loadMetadata(file)))


  private def withFile[T](file: File)(body: => T): T =
    try {
      body
    } catch {
      case e: ViewerException => throw new ParseException(file, e)
    }
}
