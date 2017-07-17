<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

PAGE:       Tavultesoft Keyman 5.0 Build Help

TITLE:      Build

BODY

SECTION:      Build Process
TEXT:         The following steps are needed to make a build:
EXECLIST
EXECENTRY2:   "",                               "Exit tools",             "Exit from Delphi, VC++"
EXECENTRY:    "make clean",                     "make clean",             "Clean to ensure that everything is built"
EXECENTRY:    "\keyman\\5.1\bin\bldutil\buildresellers.exe Keyman", "buildresellers Keyman", "Add the resellers for Keyman"
EXECENTRY:    "make all",                       "make all",               "Check that everything compiles"
EXECENTRY:    "make clean",                     "make clean",             "Clean before updating from CVS"
EXECENTRY:    "notepad \keyman\\5.1\src\global\delphi\general\certificate_check.pas", "check certificatecheck.pas", "Check that certcheck.pas is enabled"
EXECENTRY:    "notepad \keyman\\5.1\src\global\inc\certificate_check.h", "check certificatecheck.h", "Check that certcheck.h is enabled"
EXECENTRY:    "cvs -q update -d -P",            "cvs -q update -d -P",    "Update from CVS (don't forget to fix any conflicts)"
EXECENTRY:    "make all",                       "make all",               "Check to make sure the updated files compile (if necessary)"
EXECENTRY:    "make clean",                     "make clean",             "Clean before making a final build"
EXECENTRY2:   "http://www.tavultesoft.com/issuetrakka/admin.html", "Update Issuetrakka",  "Add the new version information to issuetrakka: (well, don't)"
EXECENTRY:    "notepad versionhistory.txt",     "notepad versionhistory.txt", "Ensure \keyman\\5.1\src\versionhistory.txt is up to date"
EXECENTRY:    "notepad version.txt",            "notepad version.txt",    "Set the new build version"
EXECENTRY:    "cvs -q commit",                  "cvs -q commit",          "Commit changes to CVS"
EXECENTRY:    "cmd",                            "cvs tag Release-6-0-x-0 .", "Tag all files with new release version"
EXECENTRY:    "make release",                   "make release",           "Build the release files"
ENDEXECLIST

ENDSECTION

ENDBODY

PAGENAV:      "Prerequisites", "prereq.html", "Testing", "testing.html"

FOOTER

ENDPAGE
ENDFORMAT
);
?>
