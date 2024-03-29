package org.opentorah.util

import scala.jdk.CollectionConverters.SeqHasAsJava

object Logging:
  def configureLogBack(useLogStash: Boolean): Unit = org.slf4j.LoggerFactory.getILoggerFactory match
    case loggerContext: ch.qos.logback.classic.LoggerContext => configureLogback(loggerContext, useLogStash)
    case _ =>

  private def configureLogback(loggerContext: ch.qos.logback.classic.LoggerContext, useLogStash: Boolean): Unit =
    val rootLogger: ch.qos.logback.classic.Logger = loggerContext.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)

    if useLogStash then
      val statusManager = loggerContext.getStatusManager
      if statusManager != null then statusManager.add(ch.qos.logback.core.status.InfoStatus("Configuring logger", loggerContext))

      val encoder = new net.logstash.logback.encoder.LogstashEncoder
      // Ignore default logging fields
      encoder.setExcludeMdcKeyNames(List("timestamp", "version", "logger", "thread", "level", "levelValue").asJava)

      val consoleAppender = new ch.qos.logback.core.ConsoleAppender[ch.qos.logback.classic.spi.ILoggingEvent]
      consoleAppender.setName("jsonConsoleAppender")
      consoleAppender.setContext(loggerContext)
      consoleAppender.setEncoder(encoder)

      rootLogger.detachAndStopAllAppenders()
      rootLogger.addAppender(consoleAppender)

    rootLogger.setLevel(ch.qos.logback.classic.Level.INFO)

  def setInfo(logger: org.slf4j.Logger): Unit = setLevel(logger, ch.qos.logback.classic.Level.INFO)
  def setWarn(logger: org.slf4j.Logger): Unit = setLevel(logger, ch.qos.logback.classic.Level.WARN)

  def setLevel(logger: org.slf4j.Logger, level: ch.qos.logback.classic.Level): Unit = logger match
    case logger: ch.qos.logback.classic.Logger => logger.setLevel(level)
    case _ =>
