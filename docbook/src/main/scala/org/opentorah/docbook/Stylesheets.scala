package org.opentorah.docbook

trait Stylesheets {
  protected def name: String

  protected def groupId: String
  protected def artifactId: String
  protected def classifier: Option[String]
  protected def extension: String

  def dependencyNotation(version: String): String = s"$groupId:$artifactId:$version$classifierStr@$extension"
  private def classifierStr: String = classifier.fold("")(classifier => s":$classifier")

  def uri: String

  def archiveSubdirectoryPath: Seq[String]
}

object Stylesheets {
  object xslt1 extends Stylesheets {
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
    override def archiveSubdirectoryPath: Seq[String] = Seq("docbook")
  }

  object xslt2 extends Stylesheets {
    override def name: String = "XSLT 2.0"
    override def groupId: String = "org.docbook"
    override def artifactId: String = "docbook-xslt2"
    override def classifier: Option[String] = None
    override def extension: String = "jar"

    override def uri: String = "https://cdn.docbook.org/release/latest/xslt"
    override def archiveSubdirectoryPath: Seq[String] = Seq("xslt", "base")
  }
}
