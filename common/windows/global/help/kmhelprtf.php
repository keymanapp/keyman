<?php
  $nl = "\r\n";

  function RTF( $arg ) {
    switch( $arg ) {
      case "ParaStyle0": return "\\widctlpar\\li630\\sb0\\sa200\\ql \\f0\\fs20\\cf0 ";
      case "CharStyle0": return "\\f0\\fs20\\cf0 ";
      case "ParaStyle1": return "\\keepn\\keep\\li0\\sb60\\sa120\\ql\\level1 \\f0\\fs36\\cf2 ";
      case "ParaStyle2": return "\\keepn\\keep\\li0\\sb0\\sa0\\ql\\level2 \\shading100\\cbpat3 \\f0\\fs36\\cf2 ";
      case "ParaStyle3": return "\\keepn\\keep\\li315\\sb60\\sa120\\ql\\level3 \\brdrb\\brdrs\\brdrw30\\brdrcf3 \\f0\\fs24\\cf0 ";
#      case "ParaStyle4": return "\\keepn\\keep\\li630\\sb0\\sa120\\ql\\level4 \\brdrt\\brdrs\\brdrw15\\brdrcf3 \\f0\\fs20\\cf0 ";
      case "ParaStyle4": return "\\keepn\\keep\\li630\\sb0\\sa120\\ql\\level4 \\f0\\fs20\\cf0 ";
      case "ParaStyleFtn": return "\\f0\\fs20\\cf0 ";
    }
  }

/************************************************************************************************************************/

  function HTML2RTF( $text ) {
    # Backslashes
    $text = str_replace( "\\", "\\\\", $text );

    # Non-breaking spaces
    $text = str_replace( "&nbsp;", "\\~", $text );

    # Line breaks
    $text = str_replace( "<br>", "\\line ", $text );

    # Special characters!!!!!!!!!!!!!!!!!!!!???????????????????????????????????????????????????????????????????????
    $text = str_replace( "&quot;", '"', $text );
    $text = str_replace( "&amp;", "&", $text );
    $text = str_replace( "&gt;", "\\'3E", $text );
    $text = str_replace( "&lt;", "\\'3C", $text );
    $text = str_replace( "&copy;", "\\'A9", $text );
    $text = str_replace( "&reg;", "\\'AE", $text );

    $text = str_replace( "&Agrave;", "\\'C0", $text );
    $text = str_replace( "&Aacute;", "\\'C1", $text );
    $text = str_replace( "&Egrave;", "\\'C8", $text );
    $text = str_replace( "&Eacute;", "\\'C9", $text );
    $text = str_replace( "&Igrave;", "\\'CC", $text );
    $text = str_replace( "&Iacute;", "\\'CD", $text );
    $text = str_replace( "&Ograve;", "\\'D2", $text );
    $text = str_replace( "&Oacute;", "\\'D3", $text );
    $text = str_replace( "&Ugrave;", "\\'D9", $text );
    $text = str_replace( "&Uacute;", "\\'DA", $text );

    $text = str_replace( "&Acirc;", "\\'C2", $text );
    $text = str_replace( "&Auml;", "\\'C4", $text );
    $text = str_replace( "&Ecirc;", "\\'CA", $text );
    $text = str_replace( "&Euml;", "\\'CB", $text );
    $text = str_replace( "&Icirc;", "\\'CE", $text );
    $text = str_replace( "&Iuml;", "\\'CF", $text );
    $text = str_replace( "&Ocirc;", "\\'D4", $text );
    $text = str_replace( "&Ouml;", "\\'D6", $text );
    $text = str_replace( "&Ucirc;", "\\'DB", $text );
    $text = str_replace( "&Uuml;", "\\'DC", $text );

    $text = str_replace( "&Yacute;", "\\'DD", $text );
    $text = str_replace( "&Ccedil;", "\\'C7", $text );

    $text = str_replace( "&agrave;", "\\'E0", $text );
    $text = str_replace( "&aacute;", "\\'E1", $text );
    $text = str_replace( "&egrave;", "\\'E8", $text );
    $text = str_replace( "&eacute;", "\\'E9", $text );
    $text = str_replace( "&igrave;", "\\'EC", $text );
    $text = str_replace( "&iacute;", "\\'ED", $text );
    $text = str_replace( "&ograve;", "\\'F2", $text );
    $text = str_replace( "&oacute;", "\\'F3", $text );
    $text = str_replace( "&ugrave;", "\\'F9", $text );
    $text = str_replace( "&uacute;", "\\'FA", $text );

    $text = str_replace( "&acirc;", "\\'E2", $text );
    $text = str_replace( "&auml;", "\\'E4", $text );
    $text = str_replace( "&ecirc;", "\\'EA", $text );
    $text = str_replace( "&euml;", "\\'EB", $text );
    $text = str_replace( "&icirc;", "\\'EE", $text );
    $text = str_replace( "&iuml;", "\\'EF", $text );
    $text = str_replace( "&ocirc;", "\\'F4", $text );
    $text = str_replace( "&ouml;", "\\'F6", $text );
    $text = str_replace( "&ucirc;", "\\'FB", $text );
    $text = str_replace( "&uuml;", "\\'FC", $text );

    $text = str_replace( "&yacute;", "\\'FD", $text );
    $text = str_replace( "&ccedil;", "\\'E7", $text );

    $text = str_replace( "&laquo;", "\\'AB", $text );
    $text = str_replace( "&raquo;", "\\'BB", $text );

    # HTML Only
    $text = preg_replace( '#<HTMLONLY>(.*?)</HTMLONLY>#s', '', $text );

    # Special links
    $text = preg_replace( '#<a\s+.*?hhclick.*?>(.*?)</a>#s', '$1', $text );

    # Ordinary links (single- and double-quoted)
    $text = preg_replace( '#<a\s+.*?href\s*=\s*"([^"]+)"[^>]*>(.*?)</a>#se', "RTFLink('\\2','\\1')", $text );
    $text = preg_replace( '#<a\s+.*?href\s*=\s*\'([^\']+)\'[^>]*>(.*?)</a>#se', "RTFLink('\\2','\\1')", $text );

    # Boldity
    $text = preg_replace( '#<b>(.*?)</b>#s', '\\b $1\\b0 ', $text );

    # Italicity
    $text = preg_replace( '#<i>(.*?)</i>#s', '\\i $1\\i0 ', $text );

    # Comments
    $text = preg_replace( '#<!--(.*?)-->#s', '', $text );

    # Images (single- and double-quoted)
    $text = preg_replace( '#<img\s+.*?src\s*=\s*"([^"]+)"[^>]*>#se', "RTFPic('\\1',100)", $text );
    $text = preg_replace( '#<img\s+.*?src\s*=\s*\'([^\']+)\'[^>]*>#se', "RTFPic('\\1',100)", $text );

    # Special stuff
    $text = str_replace( "<pre class='example'>", "\\f1\\fs19 ", $text );
    $text = str_replace( "</pre>", "", $text );

    $text = str_replace( "<center>", "", $text );
    $text = str_replace( "</center>", "", $text );


    return $text;
  }

/************************************************************************************************************************/

  function RTFFootnote( $text ) {
    $s = "\\super\\chftn\\nosupersub{\\footnote \\super\\chftn\\nosupersub " . RTF( "ParaStyleFtn" ) .  HTML2RTF( $text ) . "}";

    return $s;
  }

/************************************************************************************************************************/

  function RTFLink( $text, $dest ) {
    $p = substr( $dest, 0, 6 );
    if( $p != "http:/" && $p != "ftp://" ) {
      # Don't build a link, because the url is to a local html file
      $s = $text;
    } else {
      # Build a link
      $s = "{\\field{\\*\\fldinst { HYPERLINK " . $dest . " }}{\\fldrslt {\\ul\\cf3 " . $text . "}}}";
      $s .= RTFFootnote( $dest );
    }
    return $s;
  }

/************************************************************************************************************************/

  function RTFChapter( $title ) {
    global $doctitle;

    $s = "\\sectd\\sbkodd\\facingp\\titlepg \\pgncont\\pgndec " .
         "{\\headerl \\pard\\ql \\tqc\\tx4536\\tqr\\tx9026 \\plain\\f0\\fs18\\cf0 \\chpgn\\tab\\tab " . $doctitle . "\\par}" .
         "{\\headerr \\pard\\ql \\tqc\\tx4536\\tqr\\tx9026 \\plain\\f0\\fs18\\cf0 " . HTML2RTF( $title ) . "\\tab\\tab\\chpgn\\par}" .
         "\\pard\\plain " . RTFTOCEntry( HTML2RTF( $title ), 1 ) . "\\par" .
         "\\pard\\absw11170\\phpg\\posxc \\pvmrg\\posy2350 \\nowrap " .
         "\\brdrb\\brdrs\\brdrw45\\brdrcf3 \\s1\\qc \\f0\\fs84\\cf3 " . HTML2RTF( $title ) . "\\par " .
         "\\pard\\absw11170\\phpg\\posxc \\pvmrg\\posy2350 \\nowrap " .
         "\\qc " . RTFPic( "../img/keys.png", 83 ) . "\\par";

    $s .= "\\sect\\sectd\\sbkodd ";

    return $s;
  }


  function RTFEndChapter() {
    $s = "\\sect ";

    return $s;
  }
/************************************************************************************************************************/

  function RTFHeader() {
    $s = "{" .
         "\\rtf\\ansi\\deff0\\paperw11906\\paperh16838 \\margl1440\\margr1440\\margt1440\\margb1440 {\\*\\template ..\\\\..\\\\..\\\\global\\\\help\\\\pdfiser.dot }\\facingp\\gutter0 ";

#{\\vern71}
    return $s;
  }

/************************************************************************************************************************/

  function RTFFontTable() {
    # Font table 0:Verdana 1: Courier New

    $s = "{\\fonttbl" .
         "{\\f0\\fswiss Verdana;}" .
         "{\\f1\\fmodern Courier New;}" .
         "}";

    return $s;
  }

/************************************************************************************************************************/

  function RTFColorTable() {
    # Colour table -- 0:Auto, 1:Black, 2:White, 3:Dark, 4:Pale

    #This is the B&W version
    $s = "{\\colortbl;" .
         "\\red0\\green0\\blue0;" .
         "\\red255\\green255\\blue255;" .
         "\\red128\\green128\\blue128;" .
         "\\red229\\green229\\blue229;" .
         "}";

    #This is the colour version
    #$s = "{\\colortbl;" .
    #     "\\red0\\green0\\blue0;" .
    #     "\\red255\\green255\\blue255;" .
    #     "\\red172\\green71\\blue44;" .
    #     "\\red246\\green224\\blue216;" .
    #     "}";


    return $s;
  }

/************************************************************************************************************************/

  function RTFStyleSheet() {
    # Style sheet -- 0:(Body Text), 1:Title
    $s .= "{\\stylesheet" .

      "{\\s0 " . RTF("ParaStyle0") . "Body Text;}" .

      "{\\s1 " . RTF("ParaStyle1") . "Heading 1;}" .
      "{\\s2 " . RTF("ParaStyle2") . "Heading 2;}" .
      "{\\s3 " . RTF("ParaStyle3") . "Heading 3;}" .

      "{\\s4 " . "\\keep\\li0\\sb160\\sa80\\ql \\plain\\f0\\fs22\\cf0 " . "TOC 1;}" .
      "{\\s5 " . "\\keep\\li320\\sb0\\sa80\\ql \\plain\\f0\\fs20\\cf0 " . "TOC 2;}" .
      "{\\s6 " . "\\keep\\li640\\sb0\\sa80\\ql \\plain\\f0\\fs18\\cf0 " . "TOC 3;}" .

      "}";

    return $s;
  }

/************************************************************************************************************************/

  function RTFDocSettings() {
    # Footnote settings:
    $s = "\\fet0\\ftnbj\\ftnnar\\ftnrstpg ";

    return $s;
  }

/************************************************************************************************************************/

  function RTFTitlePage( $title, $subtitle ) {
    global $doctitle;

    $doctitle = HTML2RTF( $title ) . " " . HTML2RTF( $subtitle );

    $s = "\\sectd\\sbkodd\\marglsxn1100\\margrsxn1100\\titlepg" .
         "\\pard\\plain\\qc \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain\\qc \\f0\\fs72\\cf0 " . HTML2RTF( $title ) . "\\par " .
         "\\pard\\plain\\qc \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain\\qc \\f0\\fs72\\cf0 " . HTML2RTF( $subtitle ) . "\\par " .
         "\\sect ";

    return $s;
  }

  function RTFColophon( $product, $copyright, $version ) {
    global $doctitle;

    $s = "\\sectd\\sbkeven\\titlepg" .
         "\\pard\\plain \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain \\f0\\fs144\\cf0\\par " .
         "\\pard\\plain \\f0\\fs144\\cf0\\par " .

         "\\pard\\plain\\sa320 \\f0\\fs16\\cf0 " . $doctitle . "\\par " .

         "\\pard\\plain\\sa320 \\f0\\fs16\\cf0 " . HTML2RTF( $copyright ) . "\\par " .

         "\\pard\\plain\\sa320 \\f0\\fs16\\cf0 No part of this document may be reproduced or retransmitted " .
         "in any form or by any means unless accompanied by an official distribution of the product " .
         "without written permission from Tavultesoft, apart from personal reference or usage or other dealing " .
         "which is considered 'fair use' under copyright law.\\par " .

         "\\pard\\plain\\sa320 \\f0\\fs16\\cf0 The contents of this document are applicable to " . HTML2RTF( $product ) .
         " version " . HTML2RTF( $version ) . " unless otherwise noted. No guarantee is provided as to the accuracy of the " .
         "information contained herein.\\par " .

         "\\pard\\plain\\sa320 \\f0\\fs16\\cf0 All trademarks or registered trademarks mentioned in this document " .
         "are the property of their respective owners.\\par " .

         "\\sect ";

    return $s;
  }

/************************************************************************************************************************/

  function RTFTOCEntry( $title, $level ) {
    $s = "{\\tc " . sprintf( "\\tcl%d {\\v ", $level ) . $title . "}}"; 

    return $s;
  }

/************************************************************************************************************************/

  function RTFContents() {
    global $doctitle;

    # Set up section properties
    $s = "\\sectd\\sbkodd\\facingp \\pgncont\\pgndec " .
         "{\\headerl \\pard\\ql \\tqc\\tx4536\\tqr\\tx9026 \\plain\\f0\\fs18\\cf0 \\chpgn\\tab\\tab " . $doctitle . "\\par}" .
         "{\\headerr \\pard\\ql \\tqc\\tx4536\\tqr\\tx9026 \\plain\\f0\\fs18\\cf0 Table of Contents\\tab\\tab\\chpgn\\par}";

    $s .= "\\pard\\keepn\\keep\\li0\\sb0\\sa0\\ql\\level1\\s1 \\f0\\fs36\\cf0 " .
          "Table of Contents" . "\\par ";

    $s .= "\\pard {\\field{\\*\\fldinst { TOC \\\\f }}}\\par";

    # End section
    $s .= "\\sect ";
    
    return $s;
  }

/************************************************************************************************************************/

  function RTFPic( $p, $szp ) {
    $p = str_replace( ".gif", ".png", $p );

    $f = fopen( $p, "rb" );
    $data = fread( $f, filesize( $p ));
    $hexdata = bin2hex( $data );
    fclose( $f );

    # If it's not a PNG file
    if( substr( $data, 1, 3 ) != "PNG" )
      return "";

    $w = (ord(substr($data,16))<<24) | (ord(substr($data,17))<<16) | (ord(substr($data,18))<<8) | ord(substr($data,19));
    $h = (ord(substr($data,20))<<24) | (ord(substr($data,21))<<16) | (ord(substr($data,22))<<8) | ord(substr($data,23));

#NOTE!! If the pictures come out too small in Word2K or Word XP, remove the \\picscalex and \\picscaley bits:

    $s = "{\\pict\\pngblip\\picw" . $w. "\\pich" . $h . "\\picwgoal" . floor($w * 15 * ($szp / 100)) . "\\pichgoal" . floor($h * 15 * ($szp / 100)) . "\\picscalex74\\picscaley74 " . $hexdata . "}";
    
    return $s;
  }

/************************************************************************************************************************/

  function dispatcher( $func, $args ) {
    global $rtftext;

    switch( $func ) {
      case "PAGE": $rtftext .= PAGE( $args ); break;
      case "ENDPAGE": $rtftext .= ENDPAGE( $args ); break;
      case "TITLE": $rtftext .= TITLE( $args ); break;
      case "BODY": $rtftext .= BODY( $args ); break;
      case "ENDBODY": $rtftext .= ENDBODY( $args ); break;
      case "SECTION": $rtftext .= SECTION( $args ); break;
      case "ENDSECTION": $rtftext .= ENDSECTION( $args ); break;
      case "HEADING": $rtftext .= HEADING( $args ); break;
      case "TEXT": $rtftext .= TEXT( $args ); break;
      case "FOOTER": $rtftext .= FOOTER( $args ); break;
      case "BULLETLIST": $rtftext .= BULLETLIST( $args ); break;
      case "LINKSLIST": $rtftext .= LINKSLIST( $args ); break;
      case "PAGENAV": $rtftext .= PAGENAV( $args ); break;
      case "RELATED": $rtftext .= RELATED( $args ); break;
      case "HTML": $rtftext .= HTML( $args ); break;
      case "ICONTABLE": $rtftext .= ICONTABLE( $args ); break;
      case "DESCLINKSTABLE": $rtftext .= DESCLINKSTABLE( $args ); break;
      case "IMAGE": $rtftext .= IMAGE( $args ); break;
      case "HTABLE": $rtftext .= HTABLE( $args ); break;
      case "VTABLE": $rtftext .= VTABLE( $args ); break;
      case "TABLE": $rtftext .= TABLE( $args ); break;
      case "HOWTO": $rtftext .= HOWTO( $args ); break;

      default: dispatcher2( $func, $args );
    }
  }

/************************************************************************************************************************/

  function PAGE() {

    # Nothing for now

    return $s;
  }

/************************************************************************************************************************/

  function ENDPAGE() {

    $s = "\\pard\\plain\\s0 " . RTF( "ParaStyle0" ) . "\\par ";
    return $s;
  }

/************************************************************************************************************************/

  function TITLE( $title ) {
    # STYLE: PageTitle
    #  font: normal 18pt Verdana, sans-serif;
    #  color: white; background-color: #AC472C;
    #  text-align: left;
    #  padding-top: 4px;
    #  height: 42px;

    $celldef = "\\clbrdrt\\brdrs\\brdrw60\\brdrcf3 \\clbrdrl\\brdrnone \\clbrdrb\\brdrnone \\clbrdrr\\brdrnone " .
               "\\clcfpat3\\clshdng10000 \\cellx720" .
               "\\clbrdrt\\brdrs\\brdrw60\\brdrcf3 \\clbrdrl\\brdrnone \\clbrdrb\\brdrnone \\clbrdrr\\brdrnone " .
               "\\clcfpat3\\clshdng10000 \\cellx9026";
         
    $s = "\\pard\\keepn \\s2\\level2\\keepn \\plain " .
         RTFTOCEntry( HTML2RTF( $title ), 2 ) .
         "\\plain\\fs1\\cf2 " . $title . "\\par" .
         "\\trowd \\trqr\\trgaph0\\trrh550 \\trleft0 " . $celldef .
         "\\pard \\intbl " . RTF( "ParaStyle2" ) . "\\qc" . RTFPic( "../img/kmtitlelogo.png", 100 ) . "\\cell " .
         "\\pard \\intbl " . RTF( "ParaStyle2" ) . $title . "\\cell " .
         "\\pard \\intbl \\row " .
         "\\pard\\keepn \\plain \\par";

    return $s;
  }

/************************************************************************************************************************/

  function FOOTER() {
    global $COPYRIGHT, $KEYMANVERSION, $DOCVERSION;

    # Nothing for now

    return $s;
  }

/************************************************************************************************************************/

  function TEXT( $text ) {

    $s = "\\pard\\s0" . RTF( "ParaStyle0" );

    $text = HTML2RTF( $text );

    /* Replace blank lines in text with RTF newlines; also line breaks with spaces */
    $text = str_replace( "\r\n\r\n", "\\par ", $text );
    $text = str_replace( "\r\n", " ", $text );

    $s .= $text . "\\par ";

    return $s;
  }

/************************************************************************************************************************/

  function HOWTO( $text ) {
    global $nl;

    $s = "\\pard\\s0" . RTF( "ParaStyle0" ) . "{\\b HOWTO: ";

    $itemarr = explode( $nl, $text );

    $header = HTML2RTF( array_shift( $itemarr ) );

    $s .= $header . "}\\par \\pard\\s0" . RTF( "ParaStyle0" ) . "\\fi-360\\li990";

    foreach( $itemarr as $item )
        $s .= "\\bullet\\tab " . HTML2RTF( $item ) . "\\par ";

    return $s;
  }

/************************************************************************************************************************/

  function BODY() {

    # Nothing for now

    return $s;
  }

/************************************************************************************************************************/

  function ENDBODY() {

    $s = "\\pard\\plain\\s0 " . RTF( "ParaStyle0" ) . "\\par ";

    return $s;
  }

/************************************************************************************************************************/

  function SECTION( $title ) {
    global $nl;
   
    $s = "\\pard\\s3 \\plain" . RTFTOCEntry( HTML2RTF( $title ), 3 ) . RTF( "ParaStyle3" ) . 
         "\\b " . HTML2RTF( $title ) . "\\b0 \\par";

    return $s;
  }

/************************************************************************************************************************/

  function ENDSECTION() {
    global $nl;

    # Nothing for now
    return $s;
  }

/************************************************************************************************************************/

  function BULLETLIST( $items ) {
    global $nl;

    $s = "\\pard\\s0" . RTF( "ParaStyle0" ) . "\\fi-360\\li990";

    $itemarr = explode( $nl, $items );

    foreach( $itemarr as $item )
	$s .= "\\bullet\\tab " . HTML2RTF( $item ) . "\\par ";

    return $s;
  }

/************************************************************************************************************************/

  function LINKSLIST( $items ) {
    global $nl;

    $s = "\\pard\\s0" . RTF( "ParaStyle0" ) . "\\fi-360\\li990";

    $itemarr = explode( $nl, $items );

    foreach( $itemarr as $item )
      $item = HTML2RTF( $item );

    $s .= convertlinks( $items, "\\bullet\\tab ", "\\par " ) . $nl;

    $s .= "</ul>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function PAGENAV( $args ) {
    global $nl;

    # Nothing for now
    return "";

    preg_match_all( '/"([^"]*)"/', $args, $linkarr );

    list( $left, $lurl, $right, $rurl ) = $linkarr[1];
  
    $s = "<table class=\"PageNav\"><tr>" . $nl .
         "<td class=\"Prev\">" . $nl;

    if( $left == "" )
      $s .= "&nbsp;" . $nl;
    else {
      $s .= "<a href=\"" . $lurl . "\">" . $nl;
      $s .= "<img src=\"../img/uparrow.gif\" alt=\"Up\" width=8 height=8 border=0>" . $nl;
      $s .= $left . $nl;
      $s .= "</a>" . $nl;
    }

    $s .= "</td>" . $nl .
          "<td class=\"Next\">" . $nl;


    if( $right == "" )
      $s .= "&nbsp;" . $nl;
    else {
      $s .= "<a href=\"" . $rurl . "\">" . $nl;
      $s .= $right . $nl;
      $s .= "<img src=\"../img/rightarrow.gif\" alt=\"Next\" width=8 height=8 border=0>" . $nl;
      $s .= "</a>" . $nl;
    }

    $s .= "</td>" . $nl .
          "</tr></table>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function HEADING( $heading ) {
#    text-align: left;
#    font: bold x-small Verdana, sans-serif;
#    border-top: 1px solid #AC472C;

    $s = "\\pard\\s4" . RTF( "ParaStyle4" );
    $s .= "\\b " . $heading . "\\b0 \\par";

    return $s;
  }

/************************************************************************************************************************/

  /*
  ** Takes input in the form of consecutive strings, separated with whitespace, commas, or
  ** semicolons (or a mixture of all of the above), and delimited with double quotes, and
  ** converts it into a series of links, delimited with $sep1 (before) and $sep2 (after).
  */
  function convertlinks( $text, $sep1, $sep2 ) {
    global $nl;

    preg_match_all( '/"([^"]*)"/', $text, $linkarr );

    $s = "";
  
    for( $i = 0; $i < count( $linkarr[1] ); $i += 2 ) {
      $s .= $sep1 . RTFLink( $linkarr[1][$i], $linkarr[1][$i+1] ) . $sep2;
    }

    return $s;
  }

/************************************************************************************************************************/

  function RELATED( $text ) {
    global $nl;

    # Do nothing for the RTF document (I reckon)

    return "";



    $s = HEADING("Related Topics");
  
    $s .= "\\pard\\s0" . RTF("ParaStyle0") . "\\plain" . convertlinks( $text, "", "\\line " ) . "\\par ";

    return $s;
  }

/************************************************************************************************************************/

  function HTML( $text ) {
    # Do nothing here, of course!!


    return $s;
  }

/************************************************************************************************************************/

  function MakeTable( $cols, $colwidths, $cellarr, $frowa, $frowb, $fcola, $fcolb, $func ) {

    // output the table

    # Start table
    $s = "\\pard \\s0\\level0 \\plain ";
    $celldef = "\\clbrdrt\\brdrnone \\clbrdrl\\brdrnone \\clbrdrb\\brdrnone \\clbrdrr\\brdrnone \\cellx";

    $rows = floor(count( $cellarr ) / $cols);

    # Rows
    for( $r=0; $r < $rows; $r++ ) {

      # Row start
      $s .= "\\trowd \\trqc\\trgaph0\\trkeep "; #\\trautofit1
      if( $frowa != "" )
        $s .= "\\trhdr "; //Header row

      $rt = 0;
      for( $c = 0; $c < $cols; $c++ ) {
        $rt += $colwidths[$c] * 15;
        $s .= $celldef . $rt . " ";
      }

      # Cells
      for( $c = 0; $c < $cols; $c++ ) {

        $s .= "\\pard \\intbl \\s0\\sa60 " . RTF( "CharStyle0" );

        if( $r == 0 )
          $s .= $frowa;
        if( $c == 0 )
          $s .= $fcola;

        if( $func != "" )
          $s .= $func( $cellarr[ ($r * $cols) + $c ] );
        else
          $s .= $cellarr[ ($r * $cols) + $c ];

        if( $c == 0 )
          $s .= $fcolb;
        if( $r == 0 )
          $s .= $frowb;

        $s .= "\\cell ";
      }
      
      # End row
      $s .= "\\pard \\intbl \\row ";
    }

    # End table
    $s .= "\\pard\\plain \\par";

    return $s;
  }

/************************************************************************************************************************/

  function ParseTable( $text, &$cols, &$rows, &$colwidths ) {

    // find count of columns
    $text = str_replace( "\r\n", "\n", $text );
    $strings = explode( "\n", $text );
    preg_match_all( '/"([^"]*)"/', $strings[0], $cellarr );

    $cols = count( $cellarr[0] );

    //get col widths
    for( $c = 0; $c < $cols; $c++ )
      $colwidths[$c] = $cellarr[1][$c];

    //get cells' contents
    preg_match_all( '/"([^"]*)"/', $text, $cellarr );

    //remove the col width cells
    for( $c = 0; $c < $cols; $c++ )
      array_shift( $cellarr[1] );

    $rows = floor(count( $cellarr[1] ) / $cols);

    return $cellarr[1];
  }

/************************************************************************************************************************/

  function TABLE( $text ) {
    $cellarr = ParseTable( $text, $cols, $rows, $colwidths );
    return MakeTable( $cols, $colwidths, $cellarr, "", "", "", "", "HTML2RTF" );
  }

/************************************************************************************************************************/

  function HTABLE( $text ) {
    $cellarr = ParseTable( $text, $cols, $rows, $colwidths );
    return MakeTable( $cols, $colwidths, $cellarr, "", "", "\\b ", "\\b0 ", "HTML2RTF" );
  }

/************************************************************************************************************************/

  function VTABLE( $text ) {
    $cellarr = ParseTable( $text, $cols, $rows, $colwidths );
    return MakeTable( $cols, $colwidths, $cellarr, "\\b ", "\\b0 ", "", "", "HTML2RTF" );
  }

/************************************************************************************************************************/

  /* Table with links in left-hand column, and description in right-hand column */
  /* Expects three columns of input: link text, link url, and desc text. May barf on other input. */
  function DESCLINKSTABLE( $text ) {

    $cellarr = ParseTable( $text, $cols, $rows, $colwidths );

    for( $r=$rows-1; $r >= 0; $r-- ) {
      # Make the first cell the link
      $cellarr[ $r * $cols + 0 ] = RTFLink( HTML2RTF( $cellarr[ $r * $cols + 0 ] ), $cellarr[ $r * $cols + 1 ] );

      # Make the second cell the description
      $cellarr[ $r * $cols + 1 ] = $cellarr[ $r * $cols + 2 ];

      # Splice out the third cell (because the real table only has two cells/row
      array_splice( $cellarr, $r*$cols+2, 1 );
    }

    # Reduce the number of cols
    $cols = 2;

    return MakeTable( $cols, $colwidths, $cellarr, "", "", "\\b ", "\\b0 ", "HTML2RTF" );

  }

/************************************************************************************************************************/

  function ICONTABLE( $text ) {

    $cellarr = ParseTable( $text, $cols, $rows, $colwidths );
    return MakeTable( $cols, $colwidths, $cellarr, "\\qc\\fs16 ", "", "", "", "HTML2RTF" );

  }

/************************************************************************************************************************/

  function IMAGE( $text ) {
    global $nl;

    preg_match_all( '/"([^"]*)"/', $text, $paramarr );

    $s = "\\pard\\s0" . RTF("ParaStyle0") . "\\qc " .
         RTFPic( $paramarr[1][1] . ".png", 100 ) . "\\line " .
         $paramarr[1][0] . "\\par ";

#    $s =  "<p class=\"Image\">" . $nl;
#    $s .= "<img src=\"" . $paramarr[1][1] . "\" alt=\"(image: " . $paramarr[1][0] . ")\"><br>" . $nl;
#    $s .= $paramarr[1][0] . $nl;
#    $s .= "</p>";

    return $s;
  }

/************************************************************************************************************************/

?>