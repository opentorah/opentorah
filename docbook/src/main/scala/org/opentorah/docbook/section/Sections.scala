package org.opentorah.docbook.section

import org.opentorah.util.Util
import Section.Parameters

final class Sections(
  val commonSections: Map[CommonSection, Parameters],
  val docBook2s: Map[DocBook2, Parameters],
  val variants: List[(Variant, Parameters)]
) {
  def usedSections: Set[Section] = commonSections.keySet ++ docBook2s.keySet ++ variants.map(_._1.docBook2).toSet

  def substitutions: Parameters = (commonSections.values ++ docBook2s.values).toList.flatten.toMap

  def find(variant: Variant): Parameters = variants.find(_._1 == variant).get._2

  def parameters(
    variant: Variant,
    isInfoEnabled: Boolean
  ): Seq[(String, Parameters)] = {
    val docBook2: DocBook2 = variant.docBook2
    val dynamicParameters: Map[Section, Section.Parameters] = docBook2.dynamicParameters(isInfoEnabled)

    def bracket(section: Section, nonDefaultParameters: Option[Parameters]): Parameters =
      section.defaultParameters ++
      dynamicParameters.getOrElse(section, Map.empty) ++
      nonDefaultParameters.getOrElse(Map.empty)

    @scala.annotation.tailrec
    def prune(acc: Seq[(String, Parameters)], tail: Seq[(String, Parameters)]): Seq[(String, Parameters)] = tail match {
      case Nil => acc
      case (sectionName, sectionParameters) :: nextTail =>
        val overriden: Set[String] = nextTail.flatMap(_._2.keys).toSet
        prune(acc :+ (sectionName, sectionParameters -- overriden), nextTail)
    }

    val variantResult: Seq[(String, Parameters)] =
      if (variant.name.isEmpty) Seq.empty else Seq(variant.fullName -> find(variant))

    val result: Seq[(String, Parameters)] =
      docBook2.commonSections.map { commonSection: CommonSection =>
        commonSection.name -> bracket(commonSection, commonSections.get(commonSection))
      } ++ Seq(
        docBook2.name -> bracket(docBook2, docBook2s.get(docBook2))
      ) ++ variantResult

    prune(Seq.empty, result)
  }

  def allVariants: Seq[Variant] = DocBook2.all.map(_.defaultVariant) ++ variants.map(_._1)

  def runVariants(processors: List[DocBook2]): Seq[Variant] = {
    val nonDefault: Seq[Variant] = variants.filter(x => processors.contains(x._1.docBook2)).map(_._1)
    val defaultOnly: Set[DocBook2] = processors.toSet -- nonDefault.map(_.docBook2)
    defaultOnly.toSeq.map(_.defaultVariant) ++ nonDefault
  }
}

object Sections {

  def apply(parameters: Map[String, Parameters]): Sections = {
    val commonSections: collection.mutable.Map[CommonSection, Parameters] = collection.mutable.HashMap.empty
    val docBook2s: collection.mutable.Map[DocBook2, Parameters] = collection.mutable.HashMap.empty
    val variants: collection.mutable.ListBuffer[(Variant, Parameters)] = collection.mutable.ListBuffer.empty
    for ((name: String, sectionParameters: Parameters) <- parameters) {
      val (sectionName: String, variantName: Option[String]) = Util.split(name, '-')
      if (variantName.isEmpty) {
        val docBook2: Option[DocBook2] = DocBook2.find(sectionName)
        if (docBook2.isEmpty) commonSections += ((CommonSection.forName(sectionName), sectionParameters))
        else docBook2s += ((docBook2.get, sectionParameters))
      } else {
        variants += ((Variant(DocBook2.forName(sectionName), variantName), sectionParameters))
      }
      () // so that Scala doesn't try to unify the types of the accidental returns ;)
    }

    new Sections(
      commonSections = commonSections.toMap,
      docBook2s = docBook2s.toMap,
      variants = variants.toList
    )
  }
}
