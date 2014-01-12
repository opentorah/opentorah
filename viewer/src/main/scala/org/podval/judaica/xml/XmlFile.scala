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
 * under the License.
 */

package org.podval.judaica.xml

import Xml.Ops
import scala.xml.{Utility, XML, Elem}
import scala.collection.mutable.WeakHashMap
import java.io.File


object XmlFile {

  def loadResource(clazz: Class[_], name: String, tag: String): Elem =
    open(XML.load(clazz.getResourceAsStream(name + ".xml")), tag)


  def loadMetadata(file: File): Elem = load(file, "index")


  def load(file: File, tag: String): Elem = open(load(file), tag)


  def load(file: File): Elem = {
    if (!cache.contains(file)) {
      cache.put(file, XML.loadFile(file))
    }
    cache(file)
  }


  private def open(what: Elem, tag: String): Elem = Utility.trimProper(what)(0).asInstanceOf[Elem].check(tag)


  private[this] val cache: WeakHashMap[File, Elem] = new WeakHashMap[File, Elem]
}
