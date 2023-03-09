package org.opentorah.docbook

import org.opentorah.build.{Dependency, InstallableDependency}

import java.io.File

sealed abstract class Xslt(group: String, artifact: String) extends InstallableDependency[File](group, artifact):
  final override def cacheDirectory: String = "docbook"
  final override def isZip(version: Dependency.Version): Boolean = true
  final override protected def installation(root: File): File = root
  final override protected def exists(installation: File): Boolean = installation.exists

  def name: String
  def uri: String
  def usesSaxon6: Boolean

object Xslt:

  val xslt1VersionDefault: String = "1.79.1"
  val xslt2VersionDefault: String = "2.5.0"

  object Xslt1 extends Xslt(
    group = "net.sf.docbook",
    artifact = "docbook-xsl"
  ):
    override def name: String = "XSLT"
    override def uri: String = "http://docbook.sourceforge.net/release/xsl-ns/current"

    // classifier for the non-NS-aware stylesheets is "resources";
    // they strip namespaces from DocBook V5.0 and produce a warning
    //   "namesp. cut : stripped namespace before processing";
    // during namespace stripping, the base URI of the document is lost;
    // see details in https://docbook.org/docs/howto/howto.html
    override def classifier(version: Dependency.Version): Option[String] = Some("ns-resources")
    override def extension(version: Dependency.Version): Option[String] = Some("zip")
    override def archiveSubdirectoryPath(version: Dependency.Version): Seq[String] = Seq("docbook")
    override def usesSaxon6: Boolean = true

  object Xslt2 extends Xslt(
    group = "org.docbook",
    artifact = "docbook-xslt2"
  ):
    override def name: String = "XSLT 2.0"
    override def uri: String = "https://cdn.docbook.org/release/latest/xslt"
    override def classifier(version: Dependency.Version): Option[String] = None
    override def extension(version: Dependency.Version): Option[String] = Some("jar")
    override def archiveSubdirectoryPath(version: Dependency.Version): Seq[String] = Seq("xslt", "base")
    override def usesSaxon6: Boolean = false
