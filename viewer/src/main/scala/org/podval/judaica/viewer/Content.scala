/*
 *  Copyright 2014-2018 Leonid Dubinsky <dub@podval.org>.
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

import scala.xml.Elem



trait Content


final case class AppContent(readings: Map[String, Seq[Content]]) extends Content


final case class TextContent(text: String) extends Content


// TODO eliminate
final case class ElemContent(elem: Elem) extends Content


final case class SpanContent(sort: String, text: String) extends Content
