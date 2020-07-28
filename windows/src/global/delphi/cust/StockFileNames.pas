(*
  Name:             StockFileNames
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    4 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Remove StockFileName_Macros
                    14 Sep 2006 - mcdurdin - Add StockFileName_Fixed_Welcome
                    04 Dec 2006 - mcdurdin - Add cfgicon and appicon
*)
unit StockFileNames;

interface

const StockFileName_TrayIcon = 'trayicon.ico'; // do not localise
//const StockFileName_Messages = 'messages.txt'; // do not localise
const StockFileName_Menu     = 'menu.txt';     // do not localise

const StockFileName_AppIcon = 'appicon.ico';    // do not localise
const StockFileName_ConfigIcon = 'cfgicon.ico'; // do not localise

const SStockFileNames: array[0..3] of string = (
  StockFileName_TrayIcon,
//  StockFileName_Messages,
  StockFileName_Menu,
  StockFileName_AppIcon,
  StockFileName_ConfigIcon);

// Following file names are standard names within a .kmp file

const
  PackageFile_KMPInf   = 'kmp.inf';
  PackageFile_KMPJSON  = 'kmp.json';
  PackageFile_FontsInf = 'fonts.inf';   // created on installation so we know which fonts were installed with the package
  PackageFile_Welcome  = 'welcome.htm'; // do not localise
  PackageFile_Usage    = 'usage.htm';   // do not localise
  PackageFile_Options  = 'options.htm'; // do not localise

  PackageFile_Welcome_Prefix = 'welcome-';  // e.g. welcome-<lang>.htm
  PackageFile_Options_Prefix = 'options-';
  PackageFile_Usage_Prefix = 'usage-';

implementation

end.

