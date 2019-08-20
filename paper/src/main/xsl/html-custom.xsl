<?xml version="1.0" encoding="UTF-8"?>
<!-- Customizations go here. -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:db="http://docbook.org/ns/docbook"
  exclude-result-prefixes="db">

  <!-- Add MathJax support -->
  <xsl:template name="user.head.content">
    <script type="text/javascript">
      window.MathJax = <xsl:value-of select = "$mathjax.configuration" />;
    </script>
    <script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML"/>
  </xsl:template>
</xsl:stylesheet>
