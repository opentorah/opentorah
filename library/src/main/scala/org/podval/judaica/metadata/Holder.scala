package org.podval.judaica.metadata

import org.podval.judaica.util.OneValueCache

// In the end, all metadata gets parsed into (lazy) vals in the 'globals' (objects),
// but different parts of it may end up in different vals (or even in different objects).
// To have more flexibility in the order of parsing, and to make possible to use parts that
// are already parsed when parsing the other parts, we hold loaded metadata;
// since we do not need to hold it once the prasing is done, the reference is week.
abstract class Holder[K, M] extends OneValueCache[Map[K, M]] {
  def names: Map[K, Names]
}
