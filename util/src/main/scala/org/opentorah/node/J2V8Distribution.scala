package org.opentorah.node

import org.opentorah.build.Distribution
import org.opentorah.platform.{Architecture, Os}
import java.io.File

final class J2V8Distribution(version: String) extends Distribution[J2V8](version):
  private val os: Os = Os.get
  private val architecture: Architecture = Architecture.get

  override def toString: String = s"J2V8 for $os on $architecture ($dependencyNotation!$libraryName)"

  private val osName: String = os match
    case Os.Windows => "win32"
    case Os.Mac     => "macosx"
    case Os.Linux   => "linux"
    case _          => throw IllegalArgumentException()

  private val archName: String = architecture match
    case Architecture.i686   => "x86"
    case Architecture.x86_64 => "x86_64"
    case Architecture.amd64  => "x86_64"
    case _                   => throw IllegalArgumentException()

  override protected def cacheDirectory: String = "j2v8library"
  override protected def groupId: String = "com.eclipsesource.j2v8"
  override protected def artifactId: String = s"j2v8_${osName}_$archName"
  override protected def classifier: Option[String] = None
  override protected def extension: Option[String] = None
  override protected def isZip: Boolean = true
  override protected def archiveSubdirectoryPath: Seq[String] = Seq(libraryName)
  override protected def installation(root: File): J2V8 =  J2V8(library = root)
  override protected def exists(installation: J2V8): Boolean = installation.library.exists

  // TODO something with the fileName/archiveSubdirectoryPath is probably off,
  // so if I end up not removing J2V8, it should be fixed...

  private def libraryName: String = s"libj2v8_${osName}_$archName.${os.libraryExtension}"

object J2V8Distribution:

  def forOs: Option[J2V8Distribution] = Os.get match
    case Os.Windows | Os.Mac => Some(J2V8Distribution("4.6.0"))
    // Note: native library needs to be compatible with the Java code used by the plugin (see build.gradle),
    // so it should probably be 4.6.0 even for Linux, but version of Node in it doesn't work with mathjax-node:
    // mathjax-node/lib/main.js:163: SyntaxError:
    //   Block-scoped declarations (let, const, function, class) not yet supported outside strict mode
    //   for (let key in paths) {
    // Conclusion: I have to use 4.8.0 on Linux and in build.gradle, so this probably won't work on any other platform...
    // and even on Linux, if I run two tests that load the library, Gradle demon crashes (although it didn't before...).
    // Real conclusion: do not use J2V8 ):
    case Os.Linux => Some(J2V8Distribution("4.8.0"))
    case _ => None
