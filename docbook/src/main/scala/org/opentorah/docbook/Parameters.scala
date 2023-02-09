package org.opentorah.docbook

import org.opentorah.xml.ScalaXml

final class Parameters(sections: Seq[(String, Map[String, String])]):
  //private val reduced: Map[String, String] = sections.map(_._2).reduce(_ ++ _)

  def bySectionXml: ScalaXml.Nodes = Parameters.parametersBySection(sections)

object Parameters:

  // TODO is the pruning a must for XSLT?
  // - keep track of overrides;
  // - print into XSLT;
  // - print for help/information!
  // TODO slightly (?) unfold the call from Format.
  def parametersBySection(parameters: Seq[(String, Map[String, String])]): ScalaXml.Nodes =
    val result = for
      (sectionName: String, sectionParameters: Map[String, String]) <- pruneSequenceOfMaps(parameters)
      if sectionParameters.nonEmpty
    yield ScalaXml.mkComment(sectionName) +: toXml(sectionParameters)
    result.flatten

  private def toXml(parameters: Map[String, String]): Seq[ScalaXml.Element] =
    for (name: String, value: String) <- parameters.toSeq yield
      if value.nonEmpty then <xsl:param name={name}>{value}</xsl:param>
      else <xsl:param name={name}/>

  private def pruneSequenceOfMaps[K, A, B](tail: Seq[(K, Map[A, B])]): Seq[(K, Map[A, B])] = pruneSequenceOfMaps(Seq.empty, tail)

  @scala.annotation.tailrec
  private def pruneSequenceOfMaps[K, A, B](
    acc: Seq[(K, Map[A, B])],
    tail: Seq[(K, Map[A, B])]
  ): Seq[(K, Map[A, B])] = if tail.isEmpty then acc else
    val (key: K, map: Map[A, B]) = tail.head
    val nextTail: Seq[(K, Map[A, B])] = tail.tail
    pruneSequenceOfMaps(acc :+ (key, map -- nextTail.flatMap(_._2.keys).toSet), nextTail)
