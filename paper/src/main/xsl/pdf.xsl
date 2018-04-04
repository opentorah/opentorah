<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="1.0">

    <xsl:import href="@docbook@/fo/docbook.xsl"/>

    <xsl:import href="common.xsl"/>
    

    <!-- Paper size; double-sidedness; not a draft -->
    <xsl:param name="paper.type">USletter</xsl:param>
    <xsl:param name="double.sided">1</xsl:param>
    <xsl:param name="draft.mode">no</xsl:param>

    <!-- FOP extensions -->
    <xsl:param name="fop.extensions">0</xsl:param>
    <xsl:param name="fop1.extensions">1</xsl:param>

    
    <!-- Break before each section -->
    <xsl:attribute-set name="section.title.level1.properties">
        <xsl:attribute name="break-before">page</xsl:attribute>
    </xsl:attribute-set>

    <!-- Suppress content indent -->
    <xsl:param name="body.start.indent">0pt</xsl:param>

    <!-- Some space for the list labels -->
    <xsl:param name="orderedlist.label.width">2em</xsl:param>


    <!-- XInclude shouldn't affect image URIs -->
    <xsl:param name="keep.relative.image.uris">1</xsl:param>
</xsl:stylesheet>
