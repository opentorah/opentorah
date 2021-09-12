package org.opentorah.fop

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import org.apache.fop.apps.{FOUserAgent, FopFactory}
import org.opentorah.util.Util
import org.opentorah.xml.{Sax, Saxon}
import org.slf4j.{Logger, LoggerFactory}

object Fop:

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val dateFormat: java.text.DateFormat = java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")

  // To enable mathematics typesetting in Fop.run(), pass in plugin = Some(plugin), where plugin is
  // - for MathJax: new MathJaxFopPlugin(Mathematics.getMathJax(...))
  // - for JEuclid: new JEuclidFopPlugin
  def run(
    saxon: Saxon,
    configurationFile: File,
    creationDate: Option[String] = None,
    author: Option[String] = None,
    title: Option[String] = None,
    subject: Option[String] = None,
    keywords: Option[String] = None,
    inputFile: File,
    outputFile: File,
    plugin: Option[FopPlugin] = None
  ): Unit =
    logger.debug(
      s"""Fop.run(
         |  configurationFile = $configurationFile,
         |  inputFile = $inputFile,
         |  outputFile = $outputFile,
         |)""".stripMargin
    )

    Svg.forceXerces()

    val fopFactory: FopFactory = FopFactoryFactory.newFactory(configurationFile, inputFile)

    plugin.foreach(_.configure(fopFactory))

    // PDF metadata:
    val foUserAgent: FOUserAgent = fopFactory.newFOUserAgent

    setPdfMetadata(
      foUserAgent,
      creationDate,
      author,
      title,
      subject,
      keywords
    )

    run(
      saxon,
      fopFactory,
      foUserAgent,
      inputFile,
      outputFile
    )

  private def setPdfMetadata(
    foUserAgent: FOUserAgent,
    creationDate: Option[String],
    author: Option[String],
    title: Option[String],
    subject: Option[String],
    keywords: Option[String]
  ): Unit =
    foUserAgent.setCreator(Util.applicationString)
    creationDate.foreach(creationDate =>
      val value: java.util.Date = dateFormat.parse(creationDate)
      foUserAgent.setCreationDate(value)
    )

    foUserAgent.setAuthor(author.orNull)
    foUserAgent.setTitle(title.orNull)
    foUserAgent.setSubject(subject.orNull)
    foUserAgent.setKeywords(keywords.orNull)

  private def run(
    saxon: Saxon,
    fopFactory: FopFactory,
    foUserAgent: FOUserAgent,
    inputFile: File,
    outputFile: File
  ): Unit =
    val outputStream: OutputStream = BufferedOutputStream(FileOutputStream(outputFile))
    val fop: org.apache.fop.apps.Fop = fopFactory.newFop("application/pdf", foUserAgent, outputStream)

    try
      saxon.transform(
        filters = Seq.empty,
        resolver = None,
        stylesheetFile = None,
        inputSource = Sax.file2inputSource(inputFile),
        result = javax.xml.transform.sax.SAXResult(fop.getDefaultHandler)
      )
    finally
      outputStream.close()
