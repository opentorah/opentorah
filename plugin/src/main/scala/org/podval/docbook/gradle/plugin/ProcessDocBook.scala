package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.section.DocBook2
import org.podval.docbook.gradle.xml.{MathFilter, ProcessingInstructionsFilter}
import org.podval.fop.{Fop, FopPlugin}
import org.podval.fop.mathjax.{MathJax, MathJaxFopPlugin}
import org.podval.fop.util.Logger
import org.podval.fop.xml.{Resolver, Saxon, Xml}

final class ProcessDocBook(
  project: Project,
  substitutions: Map[String, String],
  resolver: Resolver,
  isJEuclidEnabled: Boolean,
  mathJax: Option[MathJax],
  layout: Layout,
  logger: Logger
) {
  def run(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String
  ): Unit = {
    logger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")

    val isPdf: Boolean = docBook2.isPdf

    val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

    val saxonOutputDirectory: File = forDocument.saxonOutputDirectory(docBook2)
    saxonOutputDirectory.mkdirs

    val saxonOutputFile: File = forDocument.saxonOutputFile(docBook2)

    // do not output the 'main' file when chunking in XSLT 1.0
    val outputFile: Option[File] = if (docBook2.usesRootFile) Some(saxonOutputFile) else None

    val mathFilter: Option[MathFilter] =
      if (mathJax.isDefined && isPdf) Some(new MathFilter(mathJax.get.configuration, logger)) else None

    // Run Saxon.
    val saxon: Saxon = if (!docBook2.usesDocBookXslt2) Saxon.Saxon6 else Saxon.Saxon9
    saxon.transform(
      resolver,
      inputFile = layout.inputFile(documentName),
      stylesheetFile = layout.stylesheetFile(forDocument.mainStylesheet(docBook2)),
      xmlReader = Xml.getFilteredXMLReader(
        Seq(new ProcessingInstructionsFilter(substitutions, logger)) ++
        mathFilter.toSeq
        // ++ Seq(new TracingFilter)
      ),
      outputFile,
      logger
    )

    copyImagesAndCss(docBook2, saxonOutputDirectory)

    // Post-processing.
    if (docBook2.usesIntermediate) {
      logger.info(s"Post-processing ${docBook2.name}")
      val outputDirectory: File = forDocument.outputDirectory(docBook2)
      outputDirectory.mkdirs

      if (isPdf) {
        val fopPlugin: Option[FopPlugin] =
          if (isJEuclidEnabled) Some(new JEuclidFopPlugin)
          else mathJax.map(new MathJaxFopPlugin(_))

        Fop.run(
          saxon = Saxon.Saxon6, // Saxon9 should work too?
          configurationFile = layout.fopConfigurationFile,
          substitutions.get("creationDate"),
          substitutions.get("author"),
          substitutions.get("title"),
          substitutions.get("subject"),
          substitutions.get("keywords"),
          plugin = fopPlugin,
          inputFile = saxonOutputFile,
          outputFile = forDocument.outputFile(docBook2),
          logger = logger
        )
      }

      docBook2.postProcess(
        inputDirectory = saxonOutputDirectory,
        outputFile = forDocument.outputFile(docBook2)
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
      from = layout.imagesDirectory.getParentFile,
      directoryName = layout.imagesDirectoryName
    )

    if (docBook2.usesCss) {
      logger.info(s"Copying CSS")
      Gradle.copyDirectory(project,
        into,
        from = layout.cssDirectory.getParentFile,
        directoryName = layout.cssDirectoryName,
        substitutions = substitutions
      )
    }
  }
}
