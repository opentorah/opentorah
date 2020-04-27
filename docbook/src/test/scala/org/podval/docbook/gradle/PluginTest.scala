package org.podval.docbook.gradle

import org.podval.docbook.gradle.plugin.DocBook
import org.opentorah.fop.xml.Namespace
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class PluginTest extends AnyFlatSpecLike with Matchers {

  private def test(
    name: String,
    substitutions: Map[String, String],
    document: String)(
    inIndexHtml: String*
  ): Unit = {
    val project = PluginTestProject(
      name,
      prefix = Some("pluginTestProjects"),
      document,
      substitutions,
      isPdfEnabled = true
    )

    project.run()

    val indexHtml: String = project.indexHtml

    for (string: String <- inIndexHtml)
      indexHtml.contains(string) shouldBe true
  }

  it should "preserve the title" in test(
    name = "title",
    substitutions = Map.empty,
    document =
    s"""<article ${DocBook.Namespace.withVersion}>
       |  <info>
       |    <title>Test DocBook File</title>
       |  </info>
       |</article>
      """
  )(
    "Test DocBook File"
  )

  it should "resolve processing instructions and entity substitutions with DTD enabled" in test(
    name = "substitutions-with-DTD",
    substitutions = Map[String, String]("version" -> "\"v1.0.0\""),
    document =
    s"""${DocBook.doctype}
       |<article ${DocBook.Namespace.withVersion} ${Namespace.XLink}>
       |  <para>Processing instruction: <?eval version ?>.</para>
       |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
       |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
       |  <para>Entity: &version;.</para>
       |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
       |</article>
      """
  )(
    "Processing instruction: v1.0.0.",
    "Processing instruction with unknown substitution: Evaluation failed for [version1].",
    "Unknown processing instruction:.",
    "Entity: v1.0.0.",
    """Entity in an attribute:<a class="link" href="http://v1.0.0" target="_top">link!</a>."""
  )

  it should "resolve processing instructions substitutions without DTD enabled" in test(
    name = "substitutions-without-DTD-processing-instructions",
    substitutions = Map[String, String]("version" -> "\"v1.0.0\""),
    document =
    s"""<article ${DocBook.Namespace.withVersion} ${Namespace.XLink}>
       |  <para>Processing instruction: <?eval version ?>.</para>
       |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
       |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
       |</article>
      """
  )(
    "Processing instruction: v1.0.0.",
    "Processing instruction with unknown substitution: Evaluation failed for [version1].",
    "Unknown processing instruction:."
  )

  it should "fail resolving entity substitutions without DTD enabled" in {
    val project: PluginTestProject = PluginTestProject(
      name = "substitutions-without-DTD-entity-substitutions",
      prefix = Some("pluginTestProjects"),
      substitutions = Map[String, String]("version" -> "\"v1.0.0\""),
      document = s"""<article ${DocBook.Namespace.withVersion} ${Namespace.XLink}>
         |  <para>Processing instruction: <?eval version ?>.</para>
         |  <para>Entity: &version;.</para>
         |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
         |</article>
      """
    )

    project.fail().contains(
      """The entity "version" was referenced, but not declared.""") shouldBe true
  }
}
