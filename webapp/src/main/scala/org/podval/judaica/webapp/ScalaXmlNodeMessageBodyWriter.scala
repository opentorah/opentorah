/*
 * Copyright 2010-2013 Leonid Dubinsky <dub@podval.org>.
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


import javax.ws.rs.core.{MultivaluedMap, MediaType}
import javax.ws.rs.ext.{MessageBodyWriter, Provider}

import org.podval.judaica.viewer.Xml

import scala.xml.Node

import java.io.OutputStream
import java.lang.annotation.Annotation
import java.lang.Class
import java.lang.reflect.Type


@Provider
class ScalaXmlNodeMessageBodyWriter extends MessageBodyWriter[Node] {

  def isWriteable(
    aClass: Class[_],
    aType: Type,
    annotations: Array[Annotation],
    mediaType: MediaType) =
  {
    classOf[Node].isAssignableFrom(aClass)
  }


  def getSize(
    nodes: Node,
    aClass: Class[_],
    aType: Type,
    annotations: Array[Annotation],
    mediaType: MediaType) =
  {
    -1L
  }


  def writeTo(
    nodes: Node,
    aClass: Class[_],
    aType: Type,
    annotations: Array[Annotation],
    mediaType: MediaType,
    stringObjectMultivaluedMap: MultivaluedMap[String, Object],
    outputStream: OutputStream) : Unit =
  {
    var answer: String = Xml.prettyPrinter.format(nodes)
    outputStream.write(answer.getBytes())
    outputStream.write('\n')
  }
}
