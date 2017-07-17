(*
  Name:             Upload_Settings
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add CRM callbacks
                    04 Dec 2006 - mcdurdin - Add Activate and ViewCustomer URLs
                    04 Jan 2007 - mcdurdin - Add buykeymandeveloper URL
                    20 Jun 2007 - mcdurdin - Widestrings
                    25 May 2009 - mcdurdin - I1995 - Add download locale URL
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    15 Apr 2015 - mcdurdin - I4658 - V9.0 - Add Keep in Touch screen
*)
unit Upload_Settings;

interface

const
  // https://api.keyman.com/ - programmatic endpoints
  API_Path_UpdateCheck = '/desktop/10.0/update';
  API_Path_DownloadLocale = '/desktop/10.0/locale';
  API_Path_Crash = '/desktop/10.0/exception'; // also used for Developer and Engine
  API_Path_SubmitDiag = '/desktop/10.0/submitdiag';
  API_Path_IsOnline = '/desktop/10.0/isonline';

  // https://www.keyman.com/ - web pages
  URLPath_KeymanLanguageLookup = '/go/100/language-lookup';
  URLPath_CreateTranslation = '/go/100/create-locale';
  URLPath_KeymanException = '/go/100/view-exception?id=%s';
  URLPath_KeepInTouch = '/go/100/keep-in-touch';
  URLPath_KeymanDeveloperDocumentation = '/go/100/keyman-developer-documentation';

  URLPath_KeymanDeveloperHome = '/go/100/keyman-developer-home';
  URLPath_KeymanHome = '/go/100/keyman-desktop-home';
  URLPath_ArchivedDownloads = '/go/100/archived-downloads';
  URLPath_Support = '/go/100/support';
  URLPath_Privacy = '/go/100/privacy';

function API_Protocol: string; // = 'https';
function API_Server: string; // = 'api.keyman.com';

function API_UserAgent: string; // = 'Keyman Desktop/10.0...'
function API_UserAgent_Developer: string; // = 'Keyman Developer/10.0...'
function API_UserAgent_Diagnostics: string;

function KeymanCom_Protocol_Server: string; // = 'https://keyman.com';

function MakeAPIURL(path: string): string;

function MakeKeymanURL(const path: string): string;

implementation

uses
  DebugPaths, ErrorControlledRegistry, RegistryKeys, Windows,
  VersionInfo;

const
  S_UserAgent = 'Keyman Desktop';
  S_UserAgent_Developer = 'Keyman Developer';
  S_UserAgent_Diagnostics = 'Keyman Desktop Diagnostics';

  S_KeymanCom = 'https://keyman.com';
  S_APIProtocol = 'https';
  S_APIServer = 'api.keyman.com';

function API_UserAgent: string;
begin
  Result := S_UserAgent + '/' + GetVersionString;
end;

function API_UserAgent_Developer: string;
begin
  Result := S_UserAgent_Developer + '/' + GetVersionString;
end;

function API_UserAgent_Diagnostics: string;
begin
  Result := S_UserAgent_Diagnostics + '/' + GetVersionString;
end;


function MakeKeymanURL(const path: string): string;
begin
  Result := KeymanCom_Protocol_Server + path;
end;

function KeymanCom_Protocol_Server: string; // = 'https://keyman.com';
begin
  Result := GetDebugPath('Debug_KeymanCom', S_KeymanCom, False);
end;

function API_Protocol: string; // = 'https';
begin
  Result := GetDebugPath('Debug_APIProtocol', S_APIProtocol, False);
end;

function API_Server: string; // = 'api.keyman.com';
begin
  Result := GetDebugPath('Debug_APIServer', S_APIServer, False);
end;

function MakeAPIURL(path: string): string;
begin
  Result := API_Protocol + '://' + API_Server + path;
end;

end.
