package org.podval.docbook.gradle

import org.scalatest.{FlatSpec, Matchers}
import DocBookPlugin.DocBookNamespace

class PluginTest extends FlatSpec with Matchers {

  "Plugin" should "preserve the title" in {
    val project: PluginTestProject = new PluginTestProject(name="title", document =
     s"""<article xmlns="$DocBookNamespace" version="5.0">
        |  <info>
        |    <title>Test DocBook File</title>
        |  </info>
        |</article>
      """
    )

    val indexHtml: String = project.getIndexHtml
    indexHtml.contains("Test DocBook File") shouldBe true
  }

  it should "resolve processing instructions and entity substitutions with DTD enabled" in {
    val project: PluginTestProject = new PluginTestProject(name="substitutions-with-DTD", document =
     s"""<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V5.0//EN" "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd">
        |<article xmlns="$DocBookNamespace" version="5.0" xmlns:xlink="http://www.w3.org/1999/xlink">
        |  <para>Processing instruction: <?eval version ?>.</para>
        |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
        |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
        |  <para>Entity: &version;.</para>
        |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
        |</article>
      """,
      substitutions = Map[String, String]("version" -> "\"v1.0.0\"")
    )

    val indexHtml: String = project.getIndexHtml
    indexHtml.contains("Processing instruction: v1.0.0.") shouldBe true
    indexHtml.contains("Processing instruction with unknown substitution: Evaluation failed for [version1].") shouldBe true
    indexHtml.contains("Unknown processing instruction:.") shouldBe true
    indexHtml.contains("Entity: v1.0.0.") shouldBe true
    indexHtml.contains("""Entity in an attribute:<a class="link" href="http://v1.0.0" target="_top">link!</a>.""") shouldBe true
  }

  it should "resolve processing instructions substitutions without DTD enabled" in {
    val project: PluginTestProject = new PluginTestProject(name="substitutions-without-DTD", document =
     s"""<article xmlns="$DocBookNamespace" version="5.0" xmlns:xlink="http://www.w3.org/1999/xlink">
        |  <para>Processing instruction: <?eval version ?>.</para>
        |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
        |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
        |</article>
      """,
      substitutions = Map[String, String]("version" -> "\"v1.0.0\"")
    )

    val indexHtml: String = project.getIndexHtml
    indexHtml.contains("Processing instruction: v1.0.0.") shouldBe true
    indexHtml.contains("Processing instruction with unknown substitution: Evaluation failed for [version1].") shouldBe true
    indexHtml.contains("Unknown processing instruction:.") shouldBe true
  }

  it should "fail resolving entity substitutions without DTD enabled" in {
    val project: PluginTestProject = new PluginTestProject(name="substitutions-without-DTD", document =
     s"""<article xmlns="$DocBookNamespace" version="5.0" xmlns:xlink="http://www.w3.org/1999/xlink">
        |  <para>Processing instruction: <?eval version ?>.</para>
        |  <para>Entity: &version;.</para>
        |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
        |</article>
      """,
      substitutions = Map[String, String]("version" -> "\"v1.0.0\"")
    )

    val output: String = project.fails
    output.contains("""The entity "version" was referenced, but not declared.""") shouldBe true
  }
}
