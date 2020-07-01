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
  SKeymanMinEvergreenVersion = '14.0'; // Major upgrades are always supported from this version on

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

const
  SKeymanDeveloperName = 'Keyman Developer';
  SKeymanDesktopName = 'Keyman Desktop';

implementation

end.
