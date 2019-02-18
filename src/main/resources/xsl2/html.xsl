<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:db="http://docbook.org/ns/docbook">
    <xsl:import href="https://cdn.docbook.org/release/latest/xslt/html/chunk.xsl"/>

    <!-- Remove trailing period after honorific -->
    <!-- <xsl:param name="punct.honorific"/> -->

    <!-- This is needed (?) for template-tweaking customizations, like removal of "Chapter" in chapter title -->
    <xsl:param name="local.l10n.xml" select="document('')"/>

    <!-- Remove "Chapter" in chapter title TODO how? -->
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
        <l:l10n language="en">
            <l:context name="title-numbered">
                <l:template name="chapter" text="%n.&#160;%t"/>
            </l:context>
        </l:l10n>
    </l:i18n>

    <!-- Table of contents -->
    <xsl:param name="toc.section.depth">4</xsl:param>

    <!-- Number everything arabically except for sections. TODO how to use this db: stuff? -->
    <xsl:param name="autolabel.elements">
        <db:appendix format="A"/>
        <db:chapter/>
        <db:figure/>
        <db:example/>
        <db:table/>
        <db:equation/>
        <db:part format="I"/>
        <db:reference format="I"/>
        <db:preface/>
        <db:qandadiv/>
        <db:section/>
        <db:refsection/>
    </xsl:param>


    <xsl:param name="section.autolabel.max.depth">6</xsl:param>

    <!-- Use ids as names -->
    <xsl:param name="use.id.as.filename">yes</xsl:param>

    <!-- Include page number in cross-references -->
    <xsl:param name="insert.xref.page.number">yes</xsl:param>

    <!-- Add MathJax support -->
    <xsl:template name="user.head.content">
        <script type="text/javascript">
            window.MathJax = {
              MathML: {
                extensions: [ "content-mathml.js", "mml3.js" ]
              }
            };
        </script>
        <script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML"/>
    </xsl:template>
</xsl:stylesheet>
