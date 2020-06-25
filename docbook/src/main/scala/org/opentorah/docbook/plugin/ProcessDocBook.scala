package org.opentorah.docbook.plugin

import java.io.File
import org.gradle.api.Project
import org.opentorah.docbook.section.DocBook2
import org.opentorah.fop.{Fop, FopPlugin, JEuclidFopPlugin, MathJaxFopPlugin}
import org.opentorah.mathjax.MathJax
import org.opentorah.util.{Files, Gradle}
import org.opentorah.xml.{Resolver, Saxon, Xerces}
import org.slf4j.{Logger, LoggerFactory}

final class ProcessDocBook(
  project: Project,
  substitutions: Map[String, String],
  resolver: Resolver,
  isJEuclidEnabled: Boolean,
  mathJax: Option[MathJax],
  fopConfigurationFile: File,
  imagesDirectory: File,
  imagesDirectoryName: String,
  cssDirectory: File,
  cssDirectoryName: String
) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[ProcessDocBook])

  def run(
    docBook2: DocBook2,
    inputFile: File,
    stylesheetFile: File,
    saxonOutputDirectory: File,
    saxonOutputFile: File,
    outputDirectory: File,
    outputFile: File
  ): Unit = {
    val isPdf: Boolean = docBook2.isPdf

    saxonOutputDirectory.mkdirs

    val mathFilter: Option[MathFilter] =
      if (mathJax.isDefined && isPdf) Some(new MathFilter(mathJax.get.configuration)) else None

    // Run Saxon.
    // Note: DocBook XSLT uses Saxon 6 XSLT 1.0 extensions and doesn't work on later Saxon versions
    // ("Don't know how to chunk with Saxonica").
    val saxon: Saxon = if (!docBook2.usesDocBookXslt2) Saxon.Saxon6 else Saxon.Saxon10
    saxon.transform(
      resolver,
      inputFile,
      stylesheetFile,
      xmlReader = Xerces.getFilteredXMLReader(
        Seq(new EvalFilter(substitutions)) ++
        mathFilter.toSeq
        // ++ Seq(new TracingFilter)
      ),
      // do not output the 'main' file when chunking in XSLT 1.0
      outputFile = if (docBook2.usesRootFile) Some(saxonOutputFile) else None
    )

    copyImagesAndCss(docBook2, saxonOutputDirectory)

    // Post-processing.
    if (docBook2.usesIntermediate) {
      logger.info(s"Post-processing ${docBook2.name}")
      outputDirectory.mkdirs

      if (isPdf) {
        val fopPlugin: Option[FopPlugin] =
          if (isJEuclidEnabled) Some(new JEuclidFopPlugin)
          else mathJax.map(new MathJaxFopPlugin(_))

        Fop.run(
          saxon = Saxon.Saxon10,
          configurationFile = fopConfigurationFile,
          creationDate = substitutions.get("creationDate"),
          author = substitutions.get("author"),
          title = substitutions.get("title"),
          subject = substitutions.get("subject"),
          keywords = substitutions.get("keywords"),
          inputFile = saxonOutputFile,
          outputFile,
          fopPlugin,
        )
      }

      docBook2.postProcess(
        inputDirectory = saxonOutputDirectory,
        outputFile
      )
    }
  }

  private def copyImagesAndCss(
    docBook2: DocBook2,
    saxonOutputDirectory: File
  ): Unit = {
    val into: File = Files.prefixedDirectory(saxonOutputDirectory, docBook2.copyDestinationDirectoryName)

    logger.info(s"Copying images")
    Gradle.copyDirectory(project,
      into,
      from = imagesDirectory.getParentFile,
      directoryName = imagesDirectoryName
    )

    if (docBook2.usesCss) {
      logger.info(s"Copying CSS")
      Gradle.copyDirectory(project,
        into,
        from = cssDirectory.getParentFile,
        directoryName = cssDirectoryName,
        substitutions = substitutions
      )
    }
  }
}
