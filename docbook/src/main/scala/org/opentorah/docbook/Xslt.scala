package org.opentorah.docbook

import org.opentorah.build.Distribution
import java.io.File

sealed abstract class Xslt(version: String) extends Distribution[File](version):
  final override def toString: String = s"DocBook $name v$version"

  final override protected def cacheDirectory: String = "docbook"
  final override protected def isZip: Boolean = true
  final override protected def installation(root: File): File = root
  final override protected def exists(installation: File): Boolean = installation.exists

  def name: String
  def uri: String
  def usesSaxon6: Boolean

object Xslt:

  val xslt1VersionDefault: String = "1.79.1"
  val xslt2VersionDefault: String = "2.5.0"

  final class Xslt1(version: String) extends Xslt(version):
    override def name: String = "XSLT"
    override def uri: String = "http://docbook.sourceforge.net/release/xsl-ns/current"
    override def groupId: String = "net.sf.docbook"
    override def artifactId: String = "docbook-xsl"
    // classifier for the non-NS-aware stylesheets is "resources";
    // they strip namespaces from DocBook V5.0 and produce a warning
    //   "namesp. cut : stripped namespace before processing";
    // during namespace stripping, the base URI of the document is lost;
    // see details in https://docbook.org/docs/howto/howto.html
    override def classifier: Option[String] = Some("ns-resources")
    override def extension: Option[String] = Some("zip")
    override def archiveSubdirectoryPath: Seq[String] = Seq("docbook")
    override def usesSaxon6: Boolean = true

  final class Xslt2(version: String) extends Xslt(version):
    override def name: String = "XSLT 2.0"
    override def uri: String = "https://cdn.docbook.org/release/latest/xslt"
    override def groupId: String = "org.docbook"
    override def artifactId: String = "docbook-xslt2"
    override def classifier: Option[String] = None
    override def extension: Option[String] = Some("jar")
    override def archiveSubdirectoryPath: Seq[String] = Seq("xslt", "base")
    override def usesSaxon6: Boolean = false
