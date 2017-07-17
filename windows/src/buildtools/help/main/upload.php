<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

PAGE:       Tavultesoft Keyman 5.0 Build Help

TITLE:      Upload Process

BODY

SECTION:      Upload Process

TEXT:         The following steps are required for uploading the new version to the server:
EXECLIST
EXECENTRY2:   "", "Upload programs",       "Upload keymandeveloper5-0-x-0.exe, keyman5-0-x-0.exe, reseller editions, PDF files, versionhistory.html to ~tavultesoft/uploads"
EXECENTRY2:   "", "Run upload-move",       "In tavultesoft.com ssh session, run ~tavultesoft/upload-move as user tavultesoft to transfer files into their final locations"
EXECENTRY2:   "", "Edit news items",       "Edit news items in /www/docs/tavultesoft.com/inc/news.txt"
EXECENTRY2:   "", "Post messages",         "Post message to keymanlist, tavultesoft-announce"
ENDEXECLIST

ENDSECTION

ENDBODY

PAGENAV:      "Testing", "testing.html", "", ""

FOOTER

ENDPAGE
ENDFORMAT
);
?>
