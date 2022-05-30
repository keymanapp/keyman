<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="element_style">
    <xsl:call-template name="list_style" />
    <xsl:call-template name="button_style" />
    <xsl:call-template name="menu_style" />
  </xsl:template>
  
  <xsl:template name="element_head">
    <xsl:call-template name="list_head" />
    <xsl:call-template name="button_head" />
    <xsl:call-template name="menu_head" />
  </xsl:template>
  
  <!--
  -
  - Generic list implementation: supports entry plus expanding details
  -
  -->
  
  <xsl:template name="list_style">
    /* List Styles */
    .list { }
    .list_packageitem { border: solid 2px #ECF8FE; display: block; background: #FFFFFF; width:100%; margin: 2px; padding: 2px; font: bold 12pt Tahoma; }
    .list_packagehover { border: solid 2px #ECF8FE; display: block; background: #ECF8FE; width:100%; margin: 2px; padding: 2px; font: bold 12pt Tahoma; }
    .list_item { border: solid 2px #CEEDFB; display: block; background: #FFFFFF; width:100%; margin: 2px; padding: 2px; }
    .x-list_hover { border: solid 2px #CEEDFB; display: block; background: #ECF8FE; width:100%; margin: 2px; padding: 2px; }
    .list_icon { display: inline; margin: 0px 6px 0px 2px; vertical-align: middle; }
    .list_detail { clear: both; font: 8pt Tahoma; margin: 2px; display: none }
    .list_expand { display: inline; cursor: default; background-image: url("img\btn_expand.gif"); background-repeat: no-repeat; background-position: 50% 50%; width: 16px; }
    table tr { text-align: left; font: 8pt Tahoma; padding: 0px; margin: 0px; }
    .table_header { font: bold 8pt Tahoma; white-space: nowrap; }
  </xsl:template>
  <xsl:template name="list_head">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
          function list_mouseover(n)
          {
            if(document.getElementById('list_'+n).className=="list_item") document.getElementById('list_'+n).style.filter = 'progid:DXImageTransform.Microsoft.Gradient(gradientType=1,startColorStr=#ceedfb,endColorStr=#ecf8fe);';
            else if (document.getElementById('list_'+n).className="list_packageitem") document.getElementById('list_'+n).className="list_packagehover";
            if(document.getElementById('button_options_'+n).className=="grey")
              document.getElementById('button_options_'+n).className="";
            event.cancelBubble=true; return true;
          }
          function list_mouseout(n)
          {
            if(document.getElementById('list_'+n).className=="list_item") document.getElementById('list_'+n).style.filter = '';
            else if(document.getElementById('list_'+n).className=="list_packagehover") document.getElementById('list_'+n).className="list_packageitem";
            document.getElementById('button_options_'+n).className="grey";
            event.cancelBubble=true; return true;
          }
          function list_detail(n)
          {
            var k = document.getElementById('list_detail_'+n), p = document.getElementById('list_expand_'+n);
            if(k.style.display == 'block')
            {
              k.style.display = 'none';
              p.style.backgroundImage = 'url("img\\btn_expand.gif")';
            }
            else
            {
              k.style.display = 'block';
              p.style.backgroundImage = 'url("img\\btn_contract.gif")';
            }
          }
      </script>
    ]]></xsl:text>
  </xsl:template>
  
  <!-- 
  -
  - Checkbox implementation
  -
  -->

  <xsl:template name="checkbox">
    <xsl:param name="id" />
    <xsl:param name="checked" />
    <xsl:param name="onclick" />
    <input type="checkbox">
      <xsl:attribute name="id"><xsl:value-of select="$id"/></xsl:attribute>
      <xsl:if test="$checked = 'true'"><xsl:attribute name="checked"></xsl:attribute></xsl:if>
      <xsl:attribute name="onclick"><xsl:value-of select="$onclick"/></xsl:attribute>
    </input>
  </xsl:template>

  <!-- 
  -
  - Button implementation
  -
  -->

  <xsl:template name="button_style">
    k\:button { float: none; cursor: default; font: bold 8pt Tahoma;  }

    k\:button.grey k\:btnleft  { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btngrey_left.gif");  }
    k\:button.grey k\:btnmid   { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btngrey_mid.gif");   }
    k\:button.grey k\:btnright { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btngrey_right.gif"); }
    k\:button.hover k\:btnleft  { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btnsel_left.gif");  }
    k\:button.hover k\:btnmid   { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btnsel_mid.gif");   }
    k\:button.hover k\:btnright { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btnsel_right.gif"); }
    k\:button.down k\:btnleft   { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btndown_left.gif"); background-position: 1px 0px; }
    k\:button.down k\:btnmid    { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btndown_mid.gif"); padding: 6px 4px 4px 6px; }
    k\:button.down k\:btnright  { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btndown_right.gif");  }
    k\:button k\:btnleft  { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btn_left.gif"); height: 24px; width: 5px; margin-left: 2px; background-repeat: no-repeat; }
    k\:button k\:btnmid   { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btn_mid.gif"); background-repeat: repeat-x; height: 24px; padding: 5px 5px 5px 5px; }
    k\:button k\:btnright { background-image: url("<xsl:value-of select="/Keyman/templatepath"/>img/btn_right.gif"); height: 24px; width: 5px; margin-right: 2px; }
  </xsl:template>
  
  <xsl:template name="button_head">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
        function kbuttonover()
        {
          var e = event.srcElement; while(e && e.tagName != 'button') e=e.parentElement;
          if(e) e.className="hover";
        }
        function kbuttonout(n)
        {
          var e = event.srcElement; while(e && e.tagName != 'button') e=e.parentElement;
          if(e) e.className="";
        }
        function kbuttondown()
        {
          var e = event.srcElement; while(e && e.tagName != 'button') e=e.parentElement;
          if(e) e.className="down";
          if(e) location.href=e.command;
        }
        function buttonsetup()
        {
          for(var i = 0; i<document.all.length; i++) {
            document.all[i].unselectable = 'on';
            document.all[i].style.cursor = 'default';
          }
        }
        window.attachEvent('onload', buttonsetup);
        
        function kbuttonup()
        {
          var e = event.srcElement; while(e && e.tagName != 'button') e=e.parentElement;
          if(e) e.className="";
        }
      </script>
    ]]></xsl:text>
  </xsl:template>
  
  <xsl:template name="button">
    <xsl:param name="caption" />
    <xsl:param name="command" />
    <xsl:param name="className" />
    <xsl:param name="id" />
    <k:button tabindex="1">
      <xsl:attribute name="ID">button_<xsl:value-of select="$id"/></xsl:attribute>
      <xsl:attribute name="command">
        <xsl:value-of select="$command"/>
      </xsl:attribute>
      <xsl:attribute name="class"><xsl:value-of select="$className"/></xsl:attribute>
      <xsl:attribute name="onmouseover">javascript:kbuttonover();</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:kbuttonout(<xsl:value-of select="index"/>);</xsl:attribute>
      <xsl:attribute name="onmousedown">javascript:kbuttondown();</xsl:attribute>
      <xsl:attribute name="onblur">javascript:kbuttonup();</xsl:attribute>
      <k:btnleft/>
      <k:btnmid>
        <xsl:value-of select="$caption" />
      </k:btnmid>
      <k:btnright/>
    </k:button>
  </xsl:template>

  <!-- 
  -
  - Popup menu implementation
  -
  -->

  <xsl:template name="menu_style">
    /* Popup menu styles */
        .menu { position: absolute; display: block; background: #CEEDFB; visibility: hidden; border: solid 2px #4E6D9B; font: 8pt Tahoma; padding: 0px;}
        k\:menuitem { width: 100%; padding: 4px; }
        k\:menuitem.hover { color: #FFFFFF; background: #4E6D9B; }
        k\:menuitem.down { background: #CEEDFB; } 
  </xsl:template>
  
  <xsl:template name="menu_head">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script>
        var menudiv=null;
        
        function PageOffset(e)
        {
          var pt = {x: 0, y: 0};
          while(e)
          {
            pt.x += e.offsetLeft; pt.y += e.offsetTop; pt.x -= e.scrollLeft; pt.y -= e.scrollTop;
            e = e.offsetParent;
          }
          return pt;
        }
        
        function ShowMenu(name,align)
        {
          var menu = document.getElementById('menu_'+name), button = document.getElementById('button_'+name);
          menudiv.innerHTML = menu.innerHTML;
          var pb = PageOffset(button);
          var pm = PageOffset(menu.offsetParent);
          //document.getElementById('debug').innerHTML = pb.x+','+pb.y+','+pb.w+','+pb.h+' -- '+pm.x+','+pm.y+' -- '+menu.offsetWidth+' -- '+(pb.x-pm.x+button.offsetWidth);

          //menudiv.style.left = 500;
          menudiv.style.width='150px';
          menudiv.style.visibility='visible';
          if(align=='right') menudiv.style.left = pb.x+button.offsetWidth-menudiv.offsetWidth;
          else menudiv.style.left = pb.x; 
          menudiv.style.top = pb.y+button.offsetHeight;
          menudiv.setCapture(false);
        }
        
        function HideMenu()
        {
          if(event.srcElement != menudiv) 
          {
            menudiv.style.visibility='hidden';
            //button.className='';
            document.releaseCapture();
          }
        }
      
        function menuitemover()
        {
          var e = event.srcElement; while(e && e.tagName != 'menuitem') e=e.parentElement;
          if(e) e.className="hover";
        }
        function menuitemout()
        {
          var e = event.srcElement; while(e && e.tagName != 'menuitem') e=e.parentElement;
          if(e) e.className="";
        }
        function menuitemdown()
        {
          var e = event.srcElement; while(e && e.tagName != 'menuitem') e=e.parentElement;
          if(e)
          {
            location.href=e.command;
            e.className="";
          }
        }
        
        function menusetup()
        {
          menudiv = document.createElement('DIV');
          menudiv.className='menu';
          menudiv.attachEvent('onmousedown', HideMenu);
          document.body.appendChild(menudiv);
        }
        window.attachEvent('onload', menusetup);
        
      </script>
    ]]></xsl:text>
  </xsl:template>

  <xsl:template name="menuitem">
    <xsl:param name="caption" />
    <xsl:param name="command" />
    <k:menuitem>
      <xsl:attribute name="onmouseover">javascript:menuitemover();</xsl:attribute>
      <xsl:attribute name="onmouseout">javascript:menuitemout();</xsl:attribute>
      <xsl:attribute name="onmousedown">javascript:menuitemdown();</xsl:attribute>
      <xsl:attribute name="command">
        <xsl:value-of select="$command" />
      </xsl:attribute>
      <xsl:value-of select="$caption"/>
    </k:menuitem>
  </xsl:template>
  
  <xsl:template name="menubutton">
    <xsl:param name="caption" />
    <xsl:param name="menutemplate" />
    <xsl:param name="id" />
    <xsl:param name="align" />
    <xsl:param name="className" />
    
    <xsl:call-template name="button">
      <xsl:with-param name="caption"><xsl:value-of select="$caption" /></xsl:with-param>
      <xsl:with-param name="command">javascript:ShowMenu('<xsl:value-of select="$id"/>','<xsl:value-of select="$align" />');</xsl:with-param>
      <xsl:with-param name="id"><xsl:value-of select="$id"/></xsl:with-param>
      <xsl:with-param name="className"><xsl:value-of select="$className"/></xsl:with-param>
    </xsl:call-template>
    
  </xsl:template>

</xsl:stylesheet>