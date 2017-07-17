<?php
  if( getenv( "KMDOCS_RTF" ) !== false )
    include( "kmhelprtf.php" );
  else
    include( "kmhelphtml.php" );


  function FORMAT( $s ) {
    global $nl;
    
    $s = str_replace( "\r\n", "\n", $s );

    $strings = explode( "\n", $s );

    $func = "";
    $args = "";
    $indent = 0;
    for( $i = 0; $i <= count( $strings ); $i++ ) {
      if( $i < count( $strings ) )
        $s = $strings[$i];

      $ws = strspn( $s, " " );

      if( ($ws == 0) && ($s != "") ) {
        if( $func != "" ) {
          /* Call the function with its arguments */
          $args = rtrim( $args );          
          dispatcher( $func, $args );
          
          $func = "";
          $args = "";
        }

        /* We're starting a new section here! */
        preg_match( "/^([^:\s]+)(([:\s]\s+)|([:\s]?[\s]*$))/", $s, $matches );
        $func = $matches[1];

        $len = strlen( $func ) + 1;
        $indent = strspn( substr( $s, $len ), " " ) + $len;
        $args = substr( $s, $indent ) . $nl;
      } else if( $s == "" ) {
        $args .= $nl;
      } else {
        $args .= substr( $s, $indent ) . $nl;
      }
    }

  }

?>
