<?php
  $nl = "\r\n";
  $howtocount = 0;

  function dispatcher( $func, $args ) {
    switch( $func ) {
      case "PAGE": echo PAGE( $args ); break;
      case "ENDPAGE": echo ENDPAGE( $args ); break;
      case "TITLE": echo TITLE( $args ); break;
      case "BODY": echo BODY( $args ); break;
      case "ENDBODY": echo ENDBODY( $args ); break;
      case "SECTION": echo SECTION( $args ); break;
      case "ENDSECTION": echo ENDSECTION( $args ); break;
      case "HEADING": echo HEADING( $args ); break;
      case "TEXT": echo TEXT( $args ); break;
      case "FOOTER": echo FOOTER( $args ); break;
      case "BULLETLIST": echo BULLETLIST( $args ); break;
      case "LINKSLIST": echo LINKSLIST( $args ); break;
      case "PAGENAV": echo PAGENAV( $args ); break;
      case "RELATED": echo RELATED( $args ); break;
      case "HTML": echo HTML( $args ); break;
      case "ICONTABLE": echo ICONTABLE( $args ); break;
      case "DESCLINKSTABLE": echo DESCLINKSTABLE( $args ); break;
      case "IMAGE": echo IMAGE( $args ); break;
      case "HTABLE": echo HTABLE( $args ); break;
      case "VTABLE": echo VTABLE( $args ); break;
      case "TABLE": echo TABLE( $args ); break;
      case "HOWTO": echo HOWTO( $args ); break;

      case "FEATURELIST": echo FEATURELIST( $args ); break;
      case "FEATURE": echo FEATURE( $args ); break;
      case "ENDFEATURELIST": echo ENDFEATURELIST( $args ); break;

      default: dispatcher2( $func, $args );
    }
  }

/************************************************************************************************************************/

  function HOWTO( $text )
  {
    global $nl;
    global $howtocount;

    $s = "";

    $sarr = explode( $nl, $text );

    $s = $s . "<p class=\"HowtoTitle\">How to: <a href=\"javascript: togglehowto('divhowto" . $howtocount . "')\">" . $sarr[0] . "</a></p>" . $nl;
    $s = $s . "<div class=\"hiddenobj\" id=\"divhowto" . $howtocount . "\"><ol>";
    for($i = 1; $i < count($sarr); $i++)
    {
      $s = $s . "<li>" . $sarr[$i] . "</li>" . $nl;
    }
    $s = $s . "</ol><p class=\"HowtoLastItem\"></p></div>" . $nl;

    $howtocount++;

    return $s;
  }

/************************************************************************************************************************/

  function ENDPAGE() {
    global $nl;

    $s = "</body>" . $nl .
         "</html>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function TITLE( $title ) {
    global $nl;
    return "<p class=\"PageTitle\">" . $nl .
           "  <img align=\"left\" src=\"../img/kmtitlelogo.gif\" width=33 height=33 hspace=7 alt='(image: Keyman icon)'>" . $nl .
           $title . $nl .
           "</p>" . $nl;
  }

/************************************************************************************************************************/

  function FOOTER() {
    global $nl, $COPYRIGHT, $KEYMANVERSION;     #, $DOCVERSION;
    global $argv;

    # Instead of make time, we display last modified time for this topic.
    $DOCVERSION = date( "D j M Y" , filemtime( $argv[0] ) );

    $s = "<table class=\"footer\"><tr>" . $nl .
         " <td class=\"Copyright\">" . $nl .
         "  " . $COPYRIGHT . $nl .
         " </td>" . $nl .
         " <td class=\"Version\">" . $nl .
         "   Keyman " . $KEYMANVERSION . " -- " . $DOCVERSION . $nl .
         " </td>" . $nl .
         "</tr></table>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function TEXT( $text ) {
    global $nl;

    /* Replace blank lines in text with HTML newlines */
    $text = str_replace( "\r\n\r\n", "\r\n</p><p>\r\n", $text );

    $s = "<p>" . $nl . $text . $nl . "</p>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function BODY() {
    global $nl;

    $s = "<div class=\"Body\">" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function ENDBODY() {
    global $nl;

    $s = "</div>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function SECTION( $title ) {
    global $nl;

    $s = "<p class=\"SectionTitle\">" . $title . "</p>" . $nl;
    $s .= "<div class=\"Section\">" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function ENDSECTION() {
    global $nl;

    $s = "</div>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function BULLETLIST( $items ) {
    global $nl;

    $s = "<ul>" . $nl;

    $itemarr = explode( $nl, $items );

    foreach( $itemarr as $item )
	$s .= "  <li>" . $item . "</li>" . $nl;

    $s .= "</ul>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function LINKSLIST( $items ) {
    global $nl;

    $s = "<ul>" . $nl;

    $s .= convertlinks( $items, "  <li>", "</li>" ) . $nl;

    $s .= "</ul>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function PAGENAV( $args ) {
    global $nl;

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
    global $nl;

    $s = "<p class=\"Heading\">" . $heading . "</p>" . $nl;

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
      $s .= $sep1 . "<a href=\"" . $linkarr[1][$i+1] . "\">" . $linkarr[1][$i] . "</a>" . $sep2 . $nl;
    }

    return $s;
  }

/************************************************************************************************************************/

  function RELATED( $text ) {
    global $nl;

    /* Replace blank lines in text with HTML newlines */
    $text = str_replace( "\r\n\r\n", "\r\n</p><p>\r\n", $text );

    $s = HEADING("Related Topics");

    $s .= "<p>" . $nl . convertlinks( $text, "", "<br>" ) . $nl . "</p>" . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function HTML( $text ) {
    global $nl;

    $s = $text . $nl;

    return $s;
  }

/************************************************************************************************************************/

  function ICONTABLE( $text ) {
    global $nl;

    /* find count of columns */
    $text = str_replace( "\r\n", "\n", $text );
    $strings = explode( "\n", $text );
    preg_match_all( '/"([^"]*)"/', $strings[0], $cellarr );

    $cols = count( $cellarr[0] );

    /* get cells' contents */
    preg_match_all( '/"([^"]*)"/', $text, $cellarr );

    /* try '/"(([^"]*(\\")?)*)"/' to match with \" embedded */

    /* output the table */

    $s = "<table border=0 cellpadding=0 cellspacing=0 align=\"center\" class=\"IconList\">" . $nl;

    for( $i=1; $i < (count( $cellarr[0] ) / $cols); $i++ ) {
      $s .= " <tr>" . $nl;
      for( $j=0; $j < $cols; $j++ ) {
        $s .= "  <td class=\"Center\">" . $cellarr[1][ ($i * $cols) + $j ] . "</td>" . $nl;
      }
      $s .= " </tr>" . $nl;
    }

    $s .= "</table>";

    return $s;

  }

/************************************************************************************************************************/

  function MakeTable( $cols, $colwidths, $cellarr, $frowa, $frowb, $fcola, $fcolb ) {

    // output the table

    # Start table
    $s = "<table border=0 cellpadding=2 cellspacing=0 align=\"center\">";

    $rows = floor(count( $cellarr ) / $cols);

    # Rows
    for( $r=0; $r < $rows; $r++ ) {

      # Row start
      $s .= "<tr>";

      # Cells
      for( $c=0; $c < $cols; $c++ ) {
        $s .= "<td valign=\"top\"";
        if( $colwidths[$c] != "" )
            $s .= " width=" . $colwidths[$c];
        $s .= ">";

        if( $r == 0 )
          $s .= $frowa;
        if( $c == 0 )
          $s .= $fcola;

        $s .= $cellarr[ ($r * $cols) + $c ];

        if( $c == 0 )
          $s .= $fcolb;
        if( $r == 0 )
          $s .= $frowb;

        $s .= "</td>";
      }

      # End row
      $s .= "</tr>";
    }

    # End table
    $s .= "</table>";

    return $s;
  }

/************************************************************************************************************************/

  function ParseTable( $text, $frowa, $frowb, $fcola, $fcolb ) {

# 8710 total!

    // find count of columns
    $text = str_replace( "\r\n", "\n", $text );
    $strings = explode( "\n", $text );
    preg_match_all( '/"([^"]*)"/', $strings[0], $cellarr );

    $cols = count( $cellarr[0] );

    //get col widths
    for( $c = 0; $c < $cols; $c++ )
      $colwidths[$c] = $cellarr[0][$c];

    //get cells' contents
    preg_match_all( '/"([^"]*)"/', $text, $cellarr );

    //remove the col width cells
    for( $c = 0; $c < $cols; $c++ )
      array_shift( $cellarr[1] );

    $s = MakeTable( $cols, $colwidths, $cellarr[1], $frowa, $frowb, $fcola, $fcolb );

    return $s;
  }

/************************************************************************************************************************/

  function TABLE( $text ) {
    return ParseTable( $text, "", "", "", "" );
  }

/************************************************************************************************************************/

  function HTABLE( $text ) {
    return ParseTable( $text, "", "", "<b>", "</b>" );
  }

/************************************************************************************************************************/

  function VTABLE( $text ) {
    return ParseTable( $text, "<b>", "</b>", "", "" );
  }

/************************************************************************************************************************/

  /* Table with links in left-hand column, and description in right-hand column */
  /* Expects three columns of input: link text, link url, and desc text. May barf on other input. */
  function DESCLINKSTABLE( $text ) {
    global $nl;

    /* get cells' contents */
    preg_match_all( '/"([^"]*)"/', $text, $cellarr );

    /* try '/"(([^"]*(\\")?)*)"/' to match with \" embedded */

    /* output the table */

    $s = "<p><table border=0 cellspacing=0 class=\"DescLinksList\">" . $nl;

    /* Go through the rows */
    for( $i=1; $i < (count( $cellarr[0] ) / 3); $i++ ) {
      $s .= " <tr>" . $nl;

      $s .= "  <td>";
      $s .= "   <a href=\"" . $cellarr[1][ ($i * 3) + 1 ] . "\">" . $cellarr[1][ ($i * 3) ] . "</a>" . $nl;
      $s .= "  </td>" . $nl . "  <td>";
      $s .= "   " . $cellarr[1][ ($i * 3) + 2 ] . $nl;
      $s .= "  </td>";

      $s .= " </tr>" . $nl;
    }

    $s .= "</table></p>";

    return $s;

  }

/************************************************************************************************************************/

  function IMAGE( $text ) {
    global $nl;

    preg_match_all( '/"([^"]*)"/', $text, $paramarr );

    $t = explode("/", $paramarr[1][1]);
    if(!strstr($t[count($t)-1], ".")) $t = ".gif"; else $t = "";

    $s =  "<p class=\"Image\">" . $nl;
    $s .= "<img src=\"" . $paramarr[1][1]  . $t . "\" alt=\"(image: " . $paramarr[1][0] . ")\"><br>" . $nl;
    $s .= $paramarr[1][0] . $nl;
    $s .= "</p>";

    return $s;
  }

/************************************************************************************************************************/

function FEATURELIST($cols)
{
    global $featurecount, $featurecolcount;

    
    $colarray = explode(",", $cols);

    $featurecount = 0; 
    $featurecolcount = count($colarray);

    $s = "<table cellspacing=0 cellpadding=0 class='fl'>" . $nl;
    $s .= '<tr><th class="fl">&nbsp;</th>';

    for($i = 0; $i < $featurecolcount; $i++)
      $s .= '<th class="flcol">' . $colarray[$i] . '</th>';
     
    $s .= '</tr>';

    return $s;
}

function FEATURE($args) {
    global $nl, $featurecount, $featurecolcount;

    if($featurecount % 2 == 0) $cls = "even"; else $cls = "odd";
    $featurecount++;
    
    $id = uniqid("div");
 
    $argarray = explode( $nl, $args );
    $ftext = $argarray[0];
    for($i = 1; $i <= $featurecolcount; $i++)
    {
      $s = $argarray[$i]; 
      if(strstr($s, "Y")) $fcol[$i-1] = "<img src='../img/fullcircle.gif'>"; 
      else if(strstr($s, "T")) $fcol[$i-1] = "<img src='../img/tick.gif'>"; 
      else if($s[0] == ':') $fcol[$i-1] = substr($s, 1);
      else $fcol[$i-1] = "&nbsp;";
    }

    $desc = "";    
    for($i = $featurecolcount+1; $i < count($argarray); $i++) $desc = $desc . $argarray[$i] . " ";
 
    $s = '<tr><td class="fl '.$cls.'"><li><a style="text-decoration: none"; href="javascript: togglehowto(' . "'" . $id . "'" . ')">' .
#         '<img id="' . $id . "img" . '" src="/images/expand.gif" border=0 align="middle"> ' .
         $ftext . '</a>';


    for($i = 0; $i < count($fcol); $i++)
       $s .= '<td class="flcol '.$cls.'">' . $fcol[$i] . '</td>';

    $s .= '</tr>';

    $s .= '<tr><td class="flhidden" style="width: '. (500+count($fcol)*75) . '" colspan="' . (count($fcol) + 1) . '"><div id="' . $id . '" class="hiddenobj">' .
         $desc . '</div></td></tr>';
    
    return $s;
}


function ENDFEATURELIST() {
    return "</table>";
}


?>
