<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template name="head">
    <head>
      <style type="text/css">

        table 
        {
          border-collapse: collapse; 
        }

        h1 
        {
          font: bold 16pt Calibri, Tahoma;
          margin: 16px 0 4px 0;
          color: #444444;
        }

        h2 
        {
          font: bold 12pt Calibri, Tahoma; 
          margin: 16px 0 4px 0; 
          color: #444444;
        }

        td, th 
        {
          border: solid 1px #cccccc;
          font: 9pt Calibri, Tahoma;
          padding: 2px 4px
        }

        th 
        {
          background: #444444; 
          border: solid 1px #444444; 
          color: white; 
          font-weight: bold
        }

        .Key,
        .Folder,
        .Location
        {
          font-weight: bold; 
          background: #eeeeee
        }
        
        .RegistryValueBinary
        {
          font: 8pt Consolas,Courier;
        }
        .Hidden
        {
          display: none;
        }

        .FileName,
        .FileDate 
        {
          white-space: nowrap
        }
        
        td.expand
        {
          cursor: pointer
        }
        
        td.expand span
        { 
          display: block; 
          width: 12px; 
          height: 13px; 
          border: solid 1px #444444; 
          overflow: hidden; 
          color: #444444; 
          font-weight: normal; 
          text-align: center; 
          font-size: 10pt; 
          font-family: Arial; 
          line-height: 0.7em; 
          padding: 1px 0 0 0; 
          margin: 0 2px
        }

        tr.Module 
        { 
          display: none;
        }
        
        tr.Process td
        { 
          font-weight: bold
        }
      </style>
      <script type="text/javascript">
          function ExpandModules(td)
          {
            var tr = td.parentNode;
            var table = tr.parentNode;
            var fExpanding;
            for(var i = tr.rowIndex + 1; i &lt; table.rows.length; i++)
            {
              if(table.rows[i].className == 'Process') break;
              if(table.rows[i].style.display == '') { table.rows[i].style.display = 'block'; fExpanding = true; }
              else { table.rows[i].style.display = ''; fExpanding = false; }
            }              
            td.childNodes[0].innerHTML = fExpanding ? '-' : '+';
          }
          
          function showBinaryData(elem)
          {
            var data = elem.parentElement.parentElement.childNodes(1);
            if(data.className.indexOf('Hidden') >= 0) data.className = data.className.replace('Hidden', ''); else data.className += ' Hidden';
          }
        </script>
    </head>
  </xsl:template>

  <xsl:template name="Folder">
    <xsl:param name="Folder" select="." />
    <xsl:param name="Name" />
    <h2><xsl:value-of select="$Name"/></h2>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Date</th>
          <th>Size</th>
          <th>Company</th>
          <th>Product</th>
          <th>Version</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        <xsl:apply-templates select="$Folder/*" mode="Folder" />
      </tbody>
    </table>
  </xsl:template>

  <xsl:template mode="Folder" match="Directory">
    <tr>
      <td class="Folder"><xsl:value-of select="@Name"/></td>
      <td class="Folder"><xsl:value-of select="@Date"/></td>
      <td class="Folder" colspan="5"></td>
    </tr>
    <xsl:apply-templates select="*" mode="Folder" />
  </xsl:template>
  
  <xsl:template mode="Folder" match="File">
    <tr>
      <td class="FileName"><xsl:value-of select="@Name"/></td>
      <td class="FileDate"><xsl:value-of select="@Date"/></td>
      <td><xsl:value-of select="@Size"/></td>
      <td><xsl:value-of select="@VersionCompanyName"/></td>
      <td><xsl:value-of select="@VersionProductName"/></td>
      <td><xsl:value-of select="@VersionProductVersion"/></td>
      <td><xsl:value-of select="@VersionFileDescription"/></td>
    </tr>
  </xsl:template>

  <xsl:template name="Registry">
    <xsl:param name="Key" select="."/>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Value</th>
        </tr>
      </thead>
      <tbody>
        <xsl:apply-templates mode="Registry" select="$Key" />
      </tbody>
    </table>
  </xsl:template>
  
  <xsl:template mode="Registry" match="Key">
    <tr><td colspan="3" class="Key"><xsl:value-of select="@Path"/></td></tr>
    <xsl:apply-templates select="Value" mode="Registry" />
    <xsl:apply-templates select="Key" mode="Registry" />
  </xsl:template>
  
  <xsl:template mode="Registry" match="Value">
    <tr>
      <td><xsl:value-of select="@Name" /></td>
      <td><xsl:value-of select="@Type" /></td>
      <td>
        <xsl:choose>
          <xsl:when test="@Type = 'Binary'">
            <div><a href="#" onclick="return showBinaryData(this);">Toggle Binary Data Display</a></div>
            <div class="RegistryValueBinary Hidden">
              <xsl:value-of select="@Value"/>
            </div>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@Value" />
          </xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>
  </xsl:template>
  
</xsl:stylesheet> 
