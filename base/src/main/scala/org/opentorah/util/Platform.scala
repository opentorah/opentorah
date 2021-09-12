package org.opentorah.util

import java.io.File
import org.slf4j.{Logger, LoggerFactory}
import scala.sys.process.{Process, ProcessLogger}

object Platform:
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  enum Os(
    val hasUname: Boolean = false,
    val libraryExtension: String = "so"
  ) derives CanEqual:
    case Windows extends Os(libraryExtension = "dll")
    case Aix
    case Mac extends Os(libraryExtension = "dylib")
    case FreeBSD
    case SunOS
    case Linux extends Os(hasUname = true)
    case Android

  def getOsName: String = System.getProperty("os.name")

  def getOs: Os =
    val name: String = getOsName.toLowerCase
    val result = Os.values.find(_.toString.toLowerCase.contains(name.toLowerCase))
      .getOrElse(throw IllegalArgumentException(s"Unsupported OS: $name"))
    if result == Os.Linux && System.getProperty("java.specification.vendor").contains("Android") then Os.Android
    else result

  enum Architecture:
    case i686
    case x86_64
    case amd64
    case ppc64
    case ppc64le
    case s390x
    case nacl
    case aarch64
    case armv6l
    case armv7l
    case armv8l

  // Note: Gradle Node plugin's code claims that Java returns "arm" on all ARM variants.
  def getEnvironmentArchName: String = System.getProperty("os.arch")

  def getSystemArchName: String = exec(command = "uname -m")

  def getArchName: String = if getOs.hasUname then getSystemArchName else getEnvironmentArchName

  def getArch: Architecture =
    val name = getArchName
    Architecture.values.find(_.toString.toLowerCase == name.toLowerCase)
      .getOrElse(throw IllegalArgumentException(s"Unsupported architecture: $name"))

  def which(what: String): Option[File] =
    execOption(command = s"which $what").map(File(_))

  def exec(command: String): String = Process(command).!!.trim

  def execOption(command: String): Option[String] =
    try Some(exec(command))
    catch 
      case _: Exception => None
  
  def exec(
    command: File,
    args: Seq[String],
    cwd: Option[File],
    extraEnv: (String, String)*
  ): String =
    val cmd: Seq[String] = command.getAbsolutePath +: args
    logger.debug(
      s"""Platform.exec(
         |  cmd = $cmd,
         |  cwd = $cwd,
         |  extraEnv = $extraEnv
         |)""".stripMargin
    )

    var err: Seq[String] = Seq.empty
    var out: Seq[String] = Seq.empty

    val exitCode = Process(
      command = cmd,
      cwd,
      extraEnv = extraEnv*
    ).!(ProcessLogger(line => err = err :+ line, line => out = out :+ line))

    val errStr = err.mkString("\n")
    val outStr = out.mkString("\n")

    val result = s"Platform.exec() => exitCode=$exitCode; err=$errStr; out=$outStr"
    if exitCode == 0 then logger.debug(result) else logger.error(result)
    if exitCode == 0 then outStr else throw IllegalArgumentException(s"Platfor.exec() => exitCode=$exitCode")
