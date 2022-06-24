(*
  Name:             wininet5
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jun 2008

  Modified Date:    4 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jun 2008 - mcdurdin - I1465 - Initial version
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit wininet5;

interface

uses
  Windows;

type
  INTERNET_PER_CONN_OPTION = record
    dwOption: DWORD;
    case Integer of
      1: (dwValue: DWord);
      2: (pszValue: LPWSTR);  // I3310
      3: (ftValue: FILETIME);
  end;

  PINTERNET_PER_CONN_OPTION = ^INTERNET_PER_CONN_OPTION;

  INTERNET_PER_CONN_OPTION_LIST = record
    dwSize: DWORD;
    pszConnection: LPWSTR;  // I3310
    dwOptionCount: DWORD;
    dwOptionError: DWORD;
    pOptions: PINTERNET_PER_CONN_OPTION;
  end;

  PINTERNET_PER_CONN_OPTION_LIST = ^INTERNET_PER_CONN_OPTION_LIST;

const
//
// Options used in INTERNET_PER_CONN_OPTON struct
//
 INTERNET_PER_CONN_FLAGS                         =1;
 INTERNET_PER_CONN_PROXY_SERVER                  =2;
 INTERNET_PER_CONN_PROXY_BYPASS                  =3;
 INTERNET_PER_CONN_AUTOCONFIG_URL                =4;
 INTERNET_PER_CONN_AUTODISCOVERY_FLAGS           =5;
 INTERNET_PER_CONN_AUTOCONFIG_SECONDARY_URL      =6;
 INTERNET_PER_CONN_AUTOCONFIG_RELOAD_DELAY_MINS  =7;
 INTERNET_PER_CONN_AUTOCONFIG_LAST_DETECT_TIME   =8;
 INTERNET_PER_CONN_AUTOCONFIG_LAST_DETECT_URL    =9;

//
// PER_CONN_FLAGS
//
 PROXY_TYPE_DIRECT                               = $00000001;   // direct to net
 PROXY_TYPE_PROXY                                = $00000002;   // via named proxy
 PROXY_TYPE_AUTO_PROXY_URL                       = $00000004;   // autoproxy URL
 PROXY_TYPE_AUTO_DETECT                          = $00000008;   // use autoproxy detection

//
// PER_CONN_AUTODISCOVERY_FLAGS
//
 AUTO_PROXY_FLAG_USER_SET                        = $00000001;   // user changed this setting
 AUTO_PROXY_FLAG_ALWAYS_DETECT                   = $00000002;   // force detection even when its not needed
 AUTO_PROXY_FLAG_DETECTION_RUN                   = $00000004;   // detection has been run
 AUTO_PROXY_FLAG_MIGRATED                        = $00000008;   // migration has just been done
 AUTO_PROXY_FLAG_DONT_CACHE_PROXY_RESULT         = $00000010;   // don't cache result of host=proxy name
 AUTO_PROXY_FLAG_CACHE_INIT_RUN                  = $00000020;   // don't initalize and run unless URL expired
 AUTO_PROXY_FLAG_DETECTION_SUSPECT               = $00000040;   // if we're on a LAN & Modem, with only one IP, bad?!?

 INTERNET_OPTION_PER_CONNECTION_OPTION = 75;
implementation

end.
