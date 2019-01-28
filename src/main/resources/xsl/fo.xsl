<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="1.0">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>

    <xsl:import href="common.xsl"/>
    
    <!-- Paper size; double-sidedness; not a draft -->
    <xsl:param name="paper.type">USletter</xsl:param>
    <xsl:param name="double.sided">1</xsl:param>
    <xsl:param name="draft.mode">no</xsl:param>

    <!-- FOP extensions -->
    <xsl:param name="fop.extensions">0</xsl:param>
    <xsl:param name="fop1.extensions">1</xsl:param>

    <!-- XInclude shouldn't affect image URIs -->
    <xsl:param name="keep.relative.image.uris">1</xsl:param>
</xsl:stylesheet>
