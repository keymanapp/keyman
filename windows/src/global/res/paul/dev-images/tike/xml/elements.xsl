<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

<xsl:template name="head">
  <head>
    <style type='text/css'>
      #pageheader { font: bold 24pt Tahoma; padding:	10px 0px 10px 20px; border-bottom: 2px solid #C64301; }


      .iobox { float: left; text-align: center; font: bold 9pt Tahoma; border: 2px solid black; }
      .ioinputbox { height: 60px; background: white; border-top: 2px solid black; text-align: center; font: bold 9pt Tahoma; background-image: url('blueellipse.png'); background-repeat: no-repeat; }
      .iooutputbox { height:	60px; background: #CEEDF8; border-top: 2px solid black; text-align: center; font: bold 9pt Tahoma; background-image: url('whiteellipse.png'); background-repeat: no-repeat; }
      .ioicon { padding: 4px; width: 40px; display: inline-block; }
      .ioarrow { width: 70px; height: 83px; float: left; vertical-align: middle; background-image: url("ioarrow.png"); background-repeat: no-repeat; background-position: 50% 50%; }

      .pages { overflow-y: auto; width: 100%; height: expression(document.getElementById('tabs').offsetTop - document.getElementById('pageheader').offsetHeight); }

      .checkbox { background-image: url("chk_checkbox.gif"); float: left; height: 24px; background-position: 6px 7px; background-repeat: no-repeat; font: bold 8pt Tahoma; padding: 5px 0px 0px 20px;}
      .checked { background-image: url("chk_checked.gif"); }

      .headerimage { float: left; width: 140px; margin: 5px 20px 0px 10px; font: 20pt Tahoma; text-align: center; }
      .pagetext { float: left; padding: 0px 20px 20px 0px; font: 10pt Tahoma; text-align: justify; }
      .pagetext ul { margin: 0px 10px 10px 16px; }

      .tabb { top: -2px; position: relative; float: left; height: 90px; width: 90px; text-align: center; font: bold 10pt Tahoma; border-top: 2px solid #C64301; background: #EEEEEE; color: #888888; }
      .tabbselected { border-top: none; width: 90px; border-left: 2px solid #C64301; border-right: 2px solid #C64301; border-bottom: 2px solid #C64301; padding-top: 2px; background: #FFFFFF; color: #000000; }
      .tabbackground { position: absolute; bottom: 0px; background: #EEEEEE; width: 100%; padding-bottom: 2px; border-top: 2px solid #C64301; }
      .tabbspacer { float: left; width: 30px; }


      #keyboardlist { padding: 10px; clear: all; }

      .keyboarditem { margin-top: 5px; height: 24px; border: 2px solid #CEEDFB; padding: 2px 0px 2px 2px; display: block; }
      .keyboardexpand { float: left; width: 20px; height: 24px; background-image: url("btn_expand.gif"); background-position: 45% 50%; background-repeat: no-repeat; }
      .keyboardcontract { background-image: url("btn_contract.gif"); }
      .keyboardicon { float: left; margin: 4px 2px 0px 0px; }
      .keyboardtext { float: left; margin: 2px 0px 2px 2px; font: bold 12pt Tahoma; }
      .keyboardname { float: left; margin: 5px 0px 0px 10px; font: 10pt Tahoma; }
      .keyboarddetails { clear: all; display: none; padding-left: 20px; margin-bottom: 2px;  }
      .keyboarddetailtext { float: left; font: 9pt Tahoma; }
      table.keyboarddetailtext tr th { text-align: left; padding-left: 0px; padding-right: 8px; }
      .keyboardoptions { float: right; }



      k\:button { cursor: default }

      k\:button.hover k\:btnleft { background-image: url("btnsel_left.gif"); }
      k\:button.hover k\:btnmid { background-image: url("btnsel_mid.gif"); }
      k\:button.hover k\:btnright { background-image: url("btnsel_right.gif"); }
      k\:button.down k\:btnleft { background-image: url("btndown_left.gif"); background-position: 1px 0px; }
      k\:button.down k\:btnmid { background-image: url("btndown_mid.gif"); padding: 6px 4px 4px 6px; }
      k\:button.down k\:btnright { background-image: url("btndown_right.gif"); }
      k\:button k\:btnleft { margin-left: 2px; background-image: url("btn_left.gif"); background-repeat: no-repeat; height: 24px; float: left; width: 5px; }
      k\:button k\:btnmid { background-image: url("btn_mid.gif"); background-repeat: repeat-x; font: bold 8pt Tahoma; float: left; height: 24px; padding: 5px 5px 5px 5px; }
      k\:button k\:btnright { margin-right: 2px; background-image: url("btn_right.gif"); background-repeat: no-repeat; height: 24px; float: left; width: 5px; }

      body { padding: 0px; margin: 0px; overflow: hidden; cursor: default}
      html { overflow: hidden }

    </style>
    <xsl:text disable-output-escaping="yes"><![CDATA[
    <script type="text/javascript">
    var globalfocus;
    var mouseisdown = false;
    var tabselected = 1;
    function selecttabb(n) {
      document.getElementById('tabb1').className = 'tabb'+(n==1?' tabbselected':'');
      document.getElementById('Keyboards').style.display = (n==1?'block':'none');
      document.getElementById('tabb2').className = 'tabb'+(n==2?' tabbselected':'');
      document.getElementById('Packaging').style.display = (n==2?'block':'none');
      document.getElementById('tabb3').className = 'tabb'+(n==3?' tabbselected':'');
      document.getElementById('Branding').style.display = (n==3?'block':'none');
      document.getElementById('tabb4').className = 'tabb'+(n==4?' tabbselected':'');
      document.getElementById('Distribution').style.display = (n==4?'block':'none');
      tabselected = n;
      
      switch (n) {
        case 1: x = 'Keyboards'; break
        case 2: x = 'Packaging'; break
        case 3: x = 'Branding'; break
        case 4: x = 'Distribution'; break
      }

      var q = document.getElementById('uppertext'+n).style.display;
      document.getElementById('upperexpand').className = (q=='block'?'checkbox checked':'checkbox');      
      document.getElementById('upperexpand').innerHTML = 'Show ' + x + ' help';
    }
    function pageload() {
      for(var i = 0; i<document.all.length; i++)
        document.all[i].unselectable = 'on';
      var kbuttons = document.getElementsByTagName('button');
      for(var b=0; b<kbuttons.length; b++) {
        kbuttons[b].onmouseover = function() { this.className='hover'; return true; }
        kbuttons[b].onmouseout = function() { this.className=''; return true; }
        kbuttons[b].onmousedown = function() { this.className='down'; mouseisdown=true; globalfocus=this.id; return true; }
        kbuttons[b].onmouseup = function() { this.className='hover'; mouseisdown=false; globalfocus=this.id; return true; }
        kbuttons[b].onkeypress = function() { this.className=(window.event.keyCode==32?'down':''); return true; }
        kbuttons[b].onkeyup = function() { this.className='hover'; return true; }
        kbuttons[b].onactivate = function() { if(!mouseisdown) this.className='hover'; return true; }
        kbuttons[b].ondeactivate = function() { this.className=''; return true; }
      }
    }
    function showdetails(x) {
      var n = document.getElementById('keyboardplus'+x).className;
      document.getElementById('keyboardplus'+x).className = (n=='keyboardexpand'?'keyboardexpand keyboardcontract':'keyboardexpand');
      document.getElementById('kbddetails'+x).style.display = (n=='keyboardexpand'?'block':'none');
    }
    function expanddetails(x) {
      var n = document.getElementById('keyboardplus'+x).className;
      if( window.event.keyCode == 43 ) {
        document.getElementById('keyboardplus'+x).className = 'keyboardexpand keyboardcontract';
        document.getElementById('kbddetails'+x).style.display = 'block';
      } else if (window.event.keyCode == 45 ) {
        document.getElementById('keyboardplus'+x).className = 'keyboardexpand';
        document.getElementById('kbddetails'+x).style.display = 'none';
      }
    }
    function highlightkeyboard(x) {
      document.getElementById('keyboarditem'+x).style.background = '#CEEDFB';
    }
    function unhighlightkeyboard(x) {
      document.getElementById('keyboarditem'+x).style.background = (globalfocus==document.getElementById('keyboarditem'+x).id?'#CEEDFB':'');
    }
    function hoverkeyboard(x) {
      document.getElementById('keyboarditem'+x).style.background = (globalfocus==document.getElementById('keyboarditem'+x).id?'#CEEDFB':'ECF8FE');
    }
    function unhoverkeyboard(x) {
      document.getElementById('keyboarditem'+x).style.background = (globalfocus==document.getElementById('keyboarditem'+x).id?'#CEEDFB':'');
    }
    function showhideupper()
    {
      var n = document.getElementById('upperexpand').className;
      document.getElementById('upperexpand').className = (n=='checkbox'?'checkbox checked':'checkbox');
      document.getElementById('uppertext'+tabselected).style.display = (n=='checkbox'?'block':'none');
    }
    </script>
    ]]></xsl:text>
  </head>

</xsl:template>
  
<xsl:template name="checkbox">
  <input type="checkbox" />
</xsl:template>

<xsl:template name="button">
  <xsl:param name="caption" />
  <xsl:param name="command" />
  <xsl:param name="id" />
  <k:button tabindex="1">
    <xsl:attribute name="ID">button_<xsl:value-of select="$id"/>
    </xsl:attribute>
    <xsl:attribute name="command">
      <xsl:value-of select="$command"/>
    </xsl:attribute>
    <xsl:attribute name="onmouseover">javascript:kbuttonover();</xsl:attribute>
    <xsl:attribute name="onmouseout">javascript:kbuttonout();</xsl:attribute>
    <xsl:attribute name="onmousedown">javascript:kbuttondown();</xsl:attribute>
    <k:btnleft/>
    <k:btnmid>
      <xsl:value-of select="$caption" />
    </k:btnmid>
    <k:btnright/>
  </k:button>
</xsl:template>

  
</xsl:stylesheet>