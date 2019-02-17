<?xml version="1.0" encoding="UTF-8"?>
<!-- Customizations go here. -->
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:db="http://docbook.org/ns/docbook"
  exclude-result-prefixes="db">

  <!-- Break before each section -->
  <xsl:attribute-set name="section.title.level1.properties">
    <xsl:attribute name="break-before">page</xsl:attribute>
  </xsl:attribute-set>

</xsl:stylesheet>
