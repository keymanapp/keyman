<?php
  $COPYRIGHT = "Copyright &copy;1994-2001 Tavultesoft";
  $KEYMANVERSION = "5.0.101.0";
  $DOCVERSION = "1.0 (21/4)";

  include( "../../../global/help/kmhelp.php" );

  function dispatcher2( $func, $args ) {
    switch( $func ) {
      case "WEBSITEPAGE": echo WEBSITEPAGE( $args ); break;
      case "EXECOBJECT": echo EXECOBJECT( $args ); break;
      case "EXECENTRY": echo EXECENTRY( $args ); break;
      case "EXECENTRY2": echo EXECENTRY2( $args ); break;
      case "EXECLIST": echo EXECLIST(); break;
      case "ENDEXECLIST": echo ENDEXECLIST(); break;
    }
  }

  function PAGE( $title ) {
    global $nl;

    $s = "<html>" . $nl .
         "<head>" . $nl .
         "  <title>" . $title . "</title>" . $nl .
         "  <link rel=\"stylesheet\" media=\"screen\" href=\"../inc/kmhelp.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"print\" href=\"../inc/kmhelpprint.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"screen\" href=\"../inc/kmbuild.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"print\" href=\"../inc/kmbuildprint.css\" type=\"text/css\">" . $nl .
         "</head>" . $nl .
         "<body>" . $nl .
         $nl;

    return $s;
  }

  function WEBSITEPAGE( $title ) {
    global $nl;

    $s = "<html>" . $nl .
         "<head>" . $nl .
         "  <title>" . $title . "</title>" . $nl .
         "  <link rel=\"stylesheet\" media=\"screen\" href=\"../inc/kmhelp.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"print\" href=\"../inc/kmhelpprint.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"screen\" href=\"../inc/kmbuild.css\" type=\"text/css\">" . $nl .
         "  <link rel=\"stylesheet\" media=\"print\" href=\"../inc/kmbuildprint.css\" type=\"text/css\">" . $nl .
         "  <script language=\"Javascript\">" . $nl .
         "  <!--" . $nl .
         "    window.open(\"http://www.tavultesoft.com/keyman/\");" . $nl .
         "  -->" . $nl .
         "  </script>" . $nl .
         "</head>" . $nl .
         "<body>" . $nl .
         $nl;

    return $s;
  }

  function EXECOBJECT( $args ) {
    global $nl;

    preg_match_all( '/"([^"]*)"/', $args, $paramarr );

    return $s;
  }

  $execcount = 0;

  function EXECENTRY( $args ) {
    global $nl, $execcount;

    preg_match_all( '/"([^"]*)"/', $args, $paramarr );

    $execcount++;

    $s = "  <tr><td>" . $nl .
         "     <input type=checkbox> <a onmouseover=\"this.style.textDecoration = 'underline'\" " . $nl ;
    $s = $s .
         "       onmouseout=\"this.style.textDecoration = 'none'\" onclick=\"exec" . $execcount . ".hhclick();\" >" . 
                 $paramarr[1][1] . "</a>" . $nl . $nl;
    $s = $s .
         "    <object id=exec" . $execcount . " type='application/x-oleobject' classid='clsid:adb880a6-d8ff-11cf-9377-00aa003b7a11'" . $nl .
         "       codebase='hhctrl.ocx#Version=4,72,8252,0' width=1 height=1>" . $nl .
         "       <param name='Command' value='ShortCut'>" . $nl .
         "            <!-- Run a program -->" . $nl .
         "       <param name='Item1'   value='TfrmSomething,kmbuild.bat," . $paramarr[1][0] . "'>" . $nl .
         "            <!-- Window class, exe, params -->" . $nl .
         "     </object></td><td>" . $nl;
         
   if($paramarr[1][2] != "")
      $s = $s . $paramarr[1][2];
   else
      $s = $s . "&nbsp;";
      
   $s = $s . 
         "  </td></tr>" . $nl .
         $nl;
    
    return $s;
  }

  function EXECENTRY2( $args ) {
    global $nl;

    preg_match_all( '/"([^"]*)"/', $args, $paramarr );

    if($paramarr[1][0] != "")
    {
      $s = "  <tr>" . $nl .
           "     <td><input type=checkbox> <a onmouseover=\"this.style.textDecoration = 'underline'\" " . $nl .
           "       onmouseout=\"this.style.textDecoration = 'none'\" onclick=\"window.open('" . $paramarr[1][0] . "');\">" . 
                 $paramarr[1][1] . "</a></td>" . $nl . $nl .
           "     <td>" . $paramarr[1][2] . "</td>" . $nl . 
           "  </tr>" . $nl;
    }
    else
      $s = "  <tr>" . $nl .
           "    <td><input type=checkbox> " . $paramarr[1][1] . "</td><td>" . $paramarr[1][2] . "</td>" . $nl . 
           "  </tr>" . $nl;
  
    return $s;
  }

  function EXECLIST()
  {
     return "<table>";
  }

  function ENDEXECLIST()
  {
     return "</table>";
  }
?>
