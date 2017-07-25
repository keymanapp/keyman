<?php
  $nl = "\r\n";

  function SITEMAP() {
    global $nl;
    $s = "<!doctype html public \"-//IETF//DTD HTML//EN\">" . $nl .
         "<html>" . $nl .
         "<head>" . $nl .
         "  <!-- Sitemap 1.0 -->" . $nl .
         "</head>" . $nl .
         "<body>" . $nl;

    for( $i = 0; $i < func_num_args(); $i++ )
      $s .= func_get_arg( $i );

    $s .= "</body>" . $nl .
          "</html>" . $nl;

    return $s;
  }

  function PROPERTIES() {
    global $nl;

    $s = "<object type=\"text/site properties\">" . $nl;

    for( $i=0; $i < func_num_args(); $i+=2 )
      $s .= "  <param name=\"" . func_get_arg( $i ) . "\" value=\"" . func_get_arg( $i+1 ) . "\">" . $nl;

    $s .= "</object>" . $nl;

    return $s;
  }

  function TLIST() {
    global $nl;

    $s = "<ul>" . $nl;

    for( $i=0; $i < func_num_args(); $i++ )
      $s .= "  <li>" . func_get_arg( $i ) . $nl;

    $s .= "</ul>";

    return $s;
  }

  function TOPIC( $name, $url="", $img=-1 ) {
    global $nl;

    $s = "<object type=\"text/sitemap\">" . $nl;
    $s .= "  <param name=\"Name\" value=\"" . $name . "\">" . $nl;
    if( $url != "" )
      $s .= "  <param name=\"Local\" value=\"" . $url . "\">" . $nl;
    if( $img != -1 )
      $s .= "  <param name=\"ImageNumber\" value=\"" . $img . "\">" . $nl;
    $s .= "</object>" . $nl;

    return $s;
  }

  function CONTENTS( $s ) {
    global $nl;

    echo "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" . $nl;
    echo "<HTML>" . $nl;
    echo "<HEAD>" . $nl;
    echo "  <!-- Sitemap 1.0 -->" . $nl;
    echo "</HEAD>" . $nl;
    echo "<BODY>" . $nl;
    echo "<OBJECT type=\"text/site properties\">" . $nl;
    echo "  <param name=\"Window Styles\" value=\"0x800620\">" . $nl;
    echo "</OBJECT>" . $nl;

    $s = str_replace( "\r\n", "\n", $s );
    $strings = explode( "\n", $s );

    $indent = 0;
    $indentsize = 2;		 /* Force TOC to use 2 character indents! */

    foreach( $strings as $s ) {
      $ws = strspn( $s, " " );
      if( $ws == strlen( $s ) || strlen( $s ) == 0 ) {
        /* blank line -- ignore it */
      } else {
	$ws = $ws / $indentsize + 1;
        if( $ws > $indent ) {
          for($indent=$indent; $indent < $ws; $indent++) echo whitespace( $indent ) . "<UL>" . $nl;
        } else if( $ws < $indent ) {
          for($indent=$indent; $indent > $ws; $indent--) echo whitespace( $indent-1 ) . "</UL>" . $nl;
        }

        $substrings = explode( ";", $s );

        $name = trim( $substrings[0] );
        if( count( $substrings ) > 1 )
          $url = trim( $substrings[1] );
        else
          $url = "";
        if( count( $substrings ) > 2 )
          $image = trim( $substrings[2] );
        else
          $image = "";

        echo whitespace( $indent ) . "<LI> ";
        echo whitespace( $indent ) . "  <OBJECT type=\"text/sitemap\">" . $nl;
        echo whitespace( $indent ) . "    <param name=\"Name\" value=\"" . $name . "\">" . $nl;
        if( $url != "" )
          echo whitespace( $indent ) . "    <param name=\"Local\" value=\"" . $url . "\">" . $nl;
        if( $image != "" )
          echo whitespace( $indent ) . "    <param name=\"ImageNumber\" value=\"" . $image . "\">" . $nl;
        echo whitespace( $indent ) . "  </OBJECT>" . $nl;
      }
    }
    while( $indent > 0 ) {
      $indent--;
      echo whitespace( $indent ) . "</UL>" . $nl;
    }
    
    echo "</BODY>";
    echo "</HTML>" . $nl;
  }

  function INDEX( $s ) {
    global $nl;

    echo "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" . $nl;
    echo "<HTML>" . $nl;
    echo "<HEAD>" . $nl;
    echo "  <!-- Sitemap 1.0 -->" . $nl;
    echo "</HEAD>" . $nl;
    echo "<BODY>" . $nl;

    $s = str_replace( "\r\n", "\n", $s );
    $strings = explode( "\n", $s );

    $level = 0;
    $indent = -1;
    $keyword = "";
    foreach( $strings as $s ) {
      $ws = strspn( $s, " " );
      if( $ws == strlen( $s ) ) {
        /* blank line -- ignore it */
      } else {
        /* if the first character is a semicolon, then it is more urls for the last keyword */
        if( $s[ $ws ] == ";" ) {
          $substrings = explode( ";", $s );
          for( $i = 1; $i < count( $substrings ); $i++ ) {
            $url = trim( $substrings[$i] );
            if( $url[0] == "*" )
              echo whitespace( $indent ) . "  <param name=\"See Also\" value=\"" . substr( $url, 1 ) . "\">" . $nl;
            else
              echo whitespace( $indent ) . "  <param name=\"Local\" value=\"" . $url . "\">" . $nl;
          }
        } else {
          if( $keyword != "" )
            echo whitespace( $indent ) . "</OBJECT>" . $nl;
    
          if( $ws > $indent ) {
            echo whitespace( $indent ) . "<UL>" . $nl;
            $indent = $ws;
            $level++;
          } else if( $ws < $indent ) {
            $indent = $ws;
            echo whitespace( $indent ) . "</UL>" . $nl;
            $level--;
          }

          $substrings = explode( ";", $s );
          echo whitespace( $indent ) . "<LI> ";
          echo whitespace( $indent ) . "<OBJECT type=\"text/sitemap\">" . $nl;

          $keyword = trim( $substrings[0] );
          echo whitespace( $indent ) . "  <param name=\"Name\" value=\"" . $keyword . "\">" . $nl;

          if( count( $substrings ) == 1 ) {
              echo whitespace( $indent ) . "  <param name=\"See Also\" value=\"" . $keyword . "\">" . $nl;
          } else {
            for( $i = 1; $i < count( $substrings ); $i++ ) {
              $url = trim( $substrings[$i] );
              if( $url[0] == "*" )
                echo whitespace( $indent ) . "  <param name=\"See Also\" value=\"" . substr( $url, 1 ) . "\">" . $nl;
              else
                echo whitespace( $indent ) . "  <param name=\"Local\" value=\"" . $url . "\">" . $nl;
            }
          }
        }
      }
    }
    if( $keyword != "" )
      echo whitespace( $indent ) . "</OBJECT>" . $nl;

    while( $level > 0 ) {
      $indent -= 2;
      echo whitespace( $indent ) . "</UL>" . $nl;
      $level--;
    }
    
    echo "</BODY>";
    echo "</HTML>" . $nl;
  }

  function whitespace( $n ) {
    $ws = "";
    for( $i = 0; $i < $n; $i++ )
      $ws .= " ";
    return $ws;
  }
?>