/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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

package org.podval.judaica.viewer

import scala.xml.{Node, Elem}

/*
 * During Shavues 5771, I discussed the Chumash Taich interface with Zev.
 * He thinks that it is not important to present the text for Taich input in
 * non-natural structure like weekly portions; so we can save on the reverse transform.
 * 
 * Zev listed interesting features of Taich:
 * 
 * Sometimes, more than one word need to be translated (=> attribute "span" in internal representation?).
 * Sometimes, there are more than one translation (or one is elucidation of the other).
 * Sometimes, translation has an explanation attached (in Hirshl's original - parens and quotes are used).
 * It is desirable to be prompted with translation of the given word that were already entered - in an overridable way (attribute "auto").
 * 
 * Range selections: Genesis/1:1
 * 
 * Nestedness of the structures: explicit or discovered? book/chapter. book/week, day only in a week...
 * Main structure: used for references; files stored in.
 * Maftir - a mark, not a division.
 *
 * Separate "Names" definitions?
 */

final class Viewer {
}
