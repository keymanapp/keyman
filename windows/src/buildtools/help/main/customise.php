NOTE: This file discusses the old customisation process, not the reseller customisation.  These two customisations will often be used
concurrently (e.g. if a reseller also wants a customised version of Keyman)

<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

PAGE:       Tavultesoft Keyman 5.0 Build Help

TITLE:      Customisation Process

BODY

SECTION:      Customisation Process
TEXT:         Customisation of Keyman affects the following programs:
BULLETLIST:   keyman.exe
              kmshell.exe
              kmuninst.exe
              kmredist.bin [stored in \dev]

SECTION:      Steps involved in customisation
TEXT:         We need to look at the following stuff:

BUILD PROCESS
=============

Use conditional defines to determine which target gets built.
Add stuff into the Makefiles for different targets (e.g. GeezWord, etc.):

###########################################################################3
FILES
=====
1. Duplicate the current global\res\keyman directory for new target
2. Duplicate the current inst\rel\keyman directory for new target
3.

MAKEFILES
=========
Releases.mak: add the new release details to the target

RESOURCES
=========
Edit kmshell\icons.rc
Edit keyman\keyman.rc
Edit kmredist\sfx.rc

Add the new release details

PROGRAMS
========
Edit kmshell.dpr; keymanstrings.pas; frmAbout; frmMain;
     frmAppInstall; frmRegist; frmHTML; frmUninstall; frmView;
     disable keyboard installation, uninstallation except through
     appinstall interface.
Edit keyman.exe; keymanstrings.cpp; disable menu entries if necessary
Edit kmredist.dpr; keymanstrings.pas
Edit kmuninst.dpr; keymanstrings.pas

###########################################################################3

Lockdown:

  -- Cannot install other keyboards after appinstall; displays a dialog suggesting visit www.tavultesoft.com and upgrade to full Keyman

Dialog boxes:

  -- appinstall: update look/feel, caption, text for CLIENT
  -- regist: update caption, text
  -- UfrmAbout: redesign for CLIENT
  -- UfrmHTML: Update license header
  -- UfrmMain: Lots of bits and pieces
  -- UfrmRegister: remove it
  -- uninstall: update slightly for CLIENT
  -- view: remove it

Message boxes:

  -- Need to make text customisable

Testing:

  -- Installing Keyman over CLIENT-VERSION

Build environment:

  -- We could have separate .dfms -- is this necessary/easier?  Problems with updating to 5.1
  -- Move all Keyman text into separate unit -- have all constant text as consts/resourcestring in this unit.

KEYMAN.EXE
==========

-- Can just use the keymansp.dll for the splash screen.
-- Need to update the tooltray icon, menu.
-- use #IFDEF ?

KMUNINST.EXE
============

-- move all text into separate unit using consts
-- not much other customisation required.


HELPFILES
=========

-- Look at combining CLIENT and other text
-- Adding pages for the client program

===================================================================

EXECLIST
CHECKBOX:                                       "Exit tools",             "Exit from Delphi, VC++"
ECHECKEXEC:   "make clean all",                 "make clean all",         "Check to ensure that your changes compile"
ECHECKEXEC:   "make clean",                     "make clean",             "Clean before updating from CVS"
ECHECKEXEC:   "cvs -q update -d -P",            "cvs -q update -d -P",    "Update from CVS (don't forget to fix any conflicts)"
ECHECKEXEC:   "make all",                       "make all",               "Check to make sure the updated files compile"
ECHECKEXEC:   "make clean",                     "make clean",             "Clean before making a final build"
ECHECKEXEC2:  "http://www.tavultesoft.com/issuetrakka/admin.html", "Update Issuetrakka",  "Add the new version information to issuetrakka: ..."
CHECKBOX:                                       "Update HISTORY",         "Add version history to the HISTORY in keyman and kmdev help"
ECHECKEXEC:   "notepad version.txt",            "notepad version.txt",    "Set the new build version"
ECHECKEXEC:   "cvs -q commit",                  "cvs -q commit",          "Commit changes to CVS"
ECHECKEXEC:   "cmd",                            "cvs tag Release-5-0-x-0 .", "Tag all files with new release version"
ECHECKEXEC:   "make release-increment",         "make release-increment", "Set the new build version"
ENDEXECLIST

ENDSECTION

ENDBODY

PAGENAV:      "Prerequisites", "prereq.html", "Testing", "testing.html"

FOOTER

ENDPAGE
ENDFORMAT
);
?>
