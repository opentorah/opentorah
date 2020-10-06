package org.opentorah.collector

class PageParameters(
  val lang: String = "en",
  val style: String, // "main",
  val title: Option[String] = None,
  val target: Option[String] = None,
  val navigationLinks: Seq[NavigationLink]
)

//documentCollection => a("../index", s"[$url]", Some("collectionViewer"))
//prevDocument       => a(url, "⇦", None)
//thisDocument       => a(url, url, None)
//nextDocument       => a(url, "⇨", None)
//facs               => a(url, "⎙", Some("facsimileViewer"))
//transcript         => a(url, "A", Some("documentViewer"))
//translations.map(lang => a(s"${pageParameters.thisDocument.get}-$lang", lang, None)
