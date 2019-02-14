<?xml version="1.0" encoding="UTF-8"?>
<!-- This needs to be imported into the customization *after* the customizable is! -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

    <!-- Remove trailing period after honorific -->
    <!-- <xsl:param name="punct.honorific"/> -->
    
    <!-- Remove trailing period after run-in head (e.g., formal paragraph title) -->
    <xsl:param name="runinhead.default.title.end.punct"/>

    <!-- This is needed (?) for template-tweaking customizations, like removal of "Chapter" in chapter title -->
    <xsl:param name="local.l10n.xml" select="document('')"/>

    <!-- Remove "Chapter" in chapter title -->
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
        <l:l10n language="en"> 
            <l:context name="title-numbered">
                <l:template name="chapter" text="%n.&#160;%t"/>
            </l:context>
        </l:l10n>
    </l:i18n>

    <!-- Table of contents -->
    <xsl:param name="toc.section.depth">4</xsl:param>

    <!-- Number everything arabically... -->
    <xsl:param name="appendix.autolabel">1</xsl:param>
    <xsl:param name="chapter.autolabel">1</xsl:param>
    <xsl:param name="part.autolabel">1</xsl:param>
    <xsl:param name="reference.autolabel">1</xsl:param>
    <!-- ... except for sections. -->
    <xsl:param name="section.autolabel">0</xsl:param>
    <xsl:param name="section.autolabel.max.depth">6</xsl:param>
</xsl:stylesheet>
