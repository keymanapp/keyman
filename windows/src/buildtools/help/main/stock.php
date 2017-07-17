<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

HTML:       <style>span.dir { font-weight: bold; color: #700000 }</style>

PAGE:       Tavultesoft Keyman 6.0 Build Help

TITLE:      Stock Objects and UI Templates

BODY

SECTION:      Stock Objects

CODE:         c:\\keyman\\7.0\\src\\developer\\stock

TEXT:         The stock.kct file contains a set of reference objects for the customisation process.  It is used internally by
              Keyman Developer to make sure that the customisation files that end users create contain all the appropriate objects,
              as well as providing a source for messages, etc.  It is built from the following source files:
BULLETLIST:   c:\\keyman\\7.0\\src\\keyman\\kmshell\\custforms.txt (which references all stockable forms in KMSHELL)
              c:\\keyman\\7.0\\src\\developer\\stock\\messages.txt [->c:\\keyman\\7.0\\src\\global\\delphi\\cust\\MessageIdentifiers.pas]
              c:\\keyman\\7.0\\src\\developer\\stock\\trayicon.ico

TEXT:         The stock.kct file is put by default into the following path:
TEXT:           c:\\keyman\\7.0\\src\\developer\\stock\\stock.kct
TEXT:         This is then copied into:
TEXT:           c:\\keyman\\7.0\\bin\\developer\\stock.kct     
              
ENDSECTION

SECTION:      UI Templates

CODE:         c:\\keyman\\7.0\\src\\developer\\uitemplates

TEXT:         A UI template is included in the Keyman Developer build, as a starting point for developers to create their own templates.
              The template is copied to a new file and then modified by the end user.  The idea is to have a set of templates that end users
              can choose from when designing their user interfaces.

HEADING:      keyman.kct

TEXT:         The special UI template keyman.kct is used for the Standard Edition of Keyman (ProductID 1), 
              although it can still be used as a template by other users.  It is included as a .pxx file in both Keyman Developer and
              Keyman Standard Edition.  Keyman.pxx will be included in non-branded installers, but not in branded installers.  Keyman.kct
              is also included as a UI template in Keyman Developer (if used by a developer, it will be assigned a new product id upon encryption).

ENDBODY

PAGENAV:      "Table of Contents", "contents.html", "Build", "build.html"

FOOTER

ENDPAGE
ENDFORMAT
);
?>
