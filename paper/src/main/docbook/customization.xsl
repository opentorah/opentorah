<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:xslthl="http://xslthl.sf.net"
    exclude-result-prefixes="xs xslthl"
    version="1.0">

    <xsl:import href="urn:docbkx:stylesheet/docbook.xsl"/>

    <xsl:param name="local.l10n.xml" select="document('')"/>
    <xsl:param name="bodyFontFamily" select="DejaVuSans"/>
    <xsl:param name="titleFontFamily" select="DejaVuSerif"/>
    <xsl:param name="monospaceFontFamily" select="DejaVuMono"/>
</xsl:stylesheet>
