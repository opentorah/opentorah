package org.podval.docbook.gradle.util

import scala.sys.process.Process

object Platform {
  def getOsName: String = System.getProperty("os.name")

  def getOs: Os = {
    val name: String = getOsName.toLowerCase
    if (name.contains("windows")) Os.Windows else
    if (name.contains("aix")) Os.Aix else
    if (name.contains("mac")) Os.Mac else
    if (name.contains("freebsd")) Os.FreeBSD else
    if (name.contains("sunos")) Os.SunOS else
    if (name.contains("linux"))  {
      if (System.getProperty("java.specification.vendor").contains("Android")) Os.Android else Os.Linux
    } else
      throw new IllegalArgumentException(s"Unsupported OS: $name")
  }

  def getArchName: String = System.getProperty("os.arch")

  def getArch: Architecture = getArchName.toLowerCase match {
    case "i686" => Architecture.i686
    case "x86_64" => Architecture.x86_64
    case "amd64" => Architecture.amd64
    case "ppc64" => Architecture.ppc64
    case "ppc64le" => Architecture.ppc64le
    case "s390x" => Architecture.s390x
    case "nacl" => Architecture.nacl
    case "aarch64" => Architecture.aarch64
    case "arm" =>
      // from the Node plugin:
      //   as Java just returns "arm" on all ARM variants, we need a system call to determine the exact arch
      // TODO do this allways (when on Linux)!
      val systemArch = Process("uname -m").!!.trim
      systemArch match {
        case "armv6l" => Architecture.armv6l
        case "armv7l" => Architecture.armv7l
        case "armv8l" => Architecture.armv8l
        case _ => throw new IllegalArgumentException(s"Unsupported ARM architecture: $systemArch")
      }
    case name => throw new IllegalArgumentException(s"Unsupported architecture: $name")
  }
}
