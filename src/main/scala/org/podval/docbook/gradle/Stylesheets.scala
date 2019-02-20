package org.podval.docbook.gradle

trait Stylesheets {
  def name: String

  final def dependencyNotation(version: String): String = {
    val classifierStr: String = classifier.fold("")(classifier => s":$classifier")
    s"$groupId:$artifactId:$version$classifierStr@$extension"
  }

  def groupId: String
  def artifactId: String
  def classifier: Option[String]
  def extension: String

  def uri: String

  def directoryName: String

  def archiveSubdirectoryName: String
}

object Stylesheets {
  val xslt1: Stylesheets = new Stylesheets {
    override def name: String = "XSLT"
    override def groupId: String = "net.sf.docbook"
    override def artifactId: String = "docbook-xsl"
    // classifier for the non-NS-aware stylesheets is "resources";
    // they  strip namespaces from DocBook V5.0 and produce a warning
    //   "namesp. cut : stripped namespace before processing";
    // during namespace stripping, the base URI of the document is lost;
    // see details in https://docbook.org/docs/howto/howto.html
    override def classifier: Option[String] = Some("ns-resources")
    override def extension: String = "zip"

    override def uri: String = "http://docbook.sourceforge.net/release/xsl-ns/current"
    override def directoryName: String = "docBookXsl"
    override def archiveSubdirectoryName: String = "docbook"
  }

  val xslt2: Stylesheets = new Stylesheets {
    override def name: String = "XSLT 2.0"
    override def groupId: String = "org.docbook"
    override def artifactId: String = "docbook-xslt2"
    override def classifier: Option[String] = None
    override def extension: String = "jar"

    override def uri: String = "https://cdn.docbook.org/release/latest/xslt"
    override def directoryName: String = "docBookXsl2"
    override def archiveSubdirectoryName: String = "xslt/base"
  }

  def apply(useXslt2: Boolean): Stylesheets = if (useXslt2) xslt2 else xslt1
}
