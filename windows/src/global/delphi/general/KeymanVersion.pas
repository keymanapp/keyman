(*
  Name:             KeymanVersion
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      26 Jun 2012

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit KeymanVersion;

interface

// We should not need to add to these hard-coded version numbers for future versions, unless
// we have a back-compat need to refer to a specific version.

{$I KeymanVersionInfo.inc}

const
  SKeymanVersion140 = '14.0';
  SKeymanVersion130 = '13.0';
  SKeymanVersion120 = '12.0';
  SKeymanVersion110 = '11.0';
  SKeymanVersion100 = '10.0';
  SKeymanVersion90 = '9.0';
  SKeymanVersion80 = '8.0';
  SKeymanVersion70 = '7.0';
  SKeymanVersion60 = '6.0';
  SKeymanVersion50 = '5.0';

{$I keymanversion_build.inc}

  SKeymanKeyboardVersion = SKeymanVersion100;

  SKeymanInstallerComponentCode = '{C289B903-7EE8-49C7-B186-BE98259EC540}';

  //
  // Minimum Keyman versions for specific features
  //

  // Evergreen: major upgrades will be offered through online update; the
  // upgrade policy for Keyman will become more enthusiastic over time.
  SKeymanVersion_Min_Evergreen       = '14.0';

  // When the user specifies a language through the bootstrap installer
  // (for example, by choosing it in Install Options, or having it provided
  // via filename or parameter), then this option will only be applied if
  // the installed Keyman version is this version or later.
  SKeymanVersion_Min_SpecifyLanguage = '14.0.104';

const
  SKeymanDeveloperName = 'Keyman Developer';
  SKeymanDesktopName = 'Keyman';
  // Keyman Desktop was renamed to Keyman [for Windows] in 14.0

implementation

end.
