package org.digitaljudaica.xml

import org.typelevel.paiges.{Chunk, Doc, Document, Style}

/*
  scala.xml.PrettyPrinter breaks the line
   - between e1 and e2 in <e1>...</e1><e2>...</e2>
   - between e1 and text in: <e1>...</e1>text,
  separating notes from what they are notes on and punctuation from what its content by spaces.
  I decided to see if Wadler's "Prettier Printer" can help avoid ugly "fix" (Print.merge) and
  have more control over the result. Paiges is a Scala implementation of the "Prettier Printer",
  so I don't have to write my own :)
 */
final class PaigesPrettyPrinter {

}
