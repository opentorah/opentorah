package org.podval.docbook.gradle.j2v8

import com.eclipsesource.v8.V8
import org.podval.docbook.gradle.util.{Architecture, Os}

object J2V8 {

  def dependencyNotationAndLibraryName(os: Os, arch: Architecture): Option[(String, String)] = {
    val version: Option[String] = os match {
      case Os.Windows | Os.Mac => Some("4.6.0")
      // Note: native library needs to be compatible with the Java code used by the plugin (see build.gradle),
      // so it should probably be 4.6.0 even for Linux, but version of Node in it doesn't work with mathjax-node:
      // mathjax-node/lib/main.js:163: SyntaxError:
      //   Block-scoped declarations (let, const, function, class) not yet supported outside strict mode
      //   for (let key in paths) {
      // Conclusion: I have to use 4.8.0 on Linux and in build.gradle, so this probably won't work on any other platform :(
      case Os.Linux => Some("4.8.0")
      case _ => None
    }

    version.map { version =>
      val osName: String = os match {
        case Os.Windows => "win32"
        case Os.Mac => "macosx"
        case Os.Linux => "linux"
        case _ => throw new IllegalArgumentException
      }

      val archName: String = arch match {
        case Architecture.i686 => "x86"
        case Architecture.x86_64 => "x86_64"
        case Architecture.amd64 => "x86_64"
        case _ => throw new IllegalArgumentException
      }

      (
        s"com.eclipsesource.j2v8:j2v8_${osName}_$archName:$version",
        s"libj2v8_${osName}_$archName.${os.libraryExtension}"
      )
    }
  }

  def setNativeLibraryLoaded(): Unit = {
    val field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
    field.setAccessible(true)
    field.set(null, true)
  }

//  def load(): Unit = if (!V8.isLoaded) {
//    // The only way to make V8 set nativeLibraryLoaded is to create (and discard) a runtime:
//    val runtime: V8 = V8.createV8Runtime(
//      "dummy",
//      Files.createTempDirectory("j2v8").toAbsolutePath.toString
//    )
//
//    runtime.release()
//  }
}
