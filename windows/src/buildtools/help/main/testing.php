<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

PAGE:       Tavultesoft Keyman 5.0 Build Help

TITLE:      Test Process

BODY

SECTION:      Test Process
TEXT:         The following steps are needed to test before uploading:
BULLETLIST:   Clean install on Windows 2000
              Upgrade on Windows 2000
              Clean install on Windows 2000 (restricted permissions)
              Upgrade on Windows 2000 (restricted permissions)
              Clean install on Windows 98
              Upgrade on Windows 98
              Run regtest under Win 98 and Win 2000
              Compile each of the keyboards in TIKE and test
              Test in Word 97, Win2K, Win98
              Test in RichEdit 2.0, 3.0, Win2K/RichEdit
              Test any new features
ENDSECTION

ENDBODY

PAGENAV:      "Build", "build.html", "Upload", "upload.html"

FOOTER

ENDPAGE
ENDFORMAT
);
?>
