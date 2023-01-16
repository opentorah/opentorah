package org.opentorah.platform

import java.io.File
import org.slf4j.{Logger, LoggerFactory}
import scala.sys.process.{Process, ProcessLogger}

object Exec:
  private lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def which(what: String): Option[File] =
    attempt(command = s"which $what").map(File(_))

  def attempt(command: String): Option[String] =
    try Some(Exec(command))
    catch
      case _: Exception => None

  def apply(command: String): String = Process(command).!!.trim

  def apply(
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
    ).!(ProcessLogger(fout = line => out = out :+ line, ferr = line => err = err :+ line))

    val errStr = err.mkString("\n")
    val outStr = out.mkString("\n")

    val result = s"Platform.exec() => exitCode=$exitCode; err=$errStr; out=$outStr"
    if exitCode == 0 then logger.debug(result) else logger.error(result)
    if exitCode == 0 then outStr else throw IllegalArgumentException(s"Platfor.exec() => exitCode=$exitCode")
