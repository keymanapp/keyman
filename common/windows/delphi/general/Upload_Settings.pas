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

uses
  KeymanVersion;

const
  // https://api.keyman.com/ - programmatic endpoints
  API_Path_UpdateCheck_Windows = '/windows/14.0/update'; // version will only update when the api changes
  API_Path_UpdateCheck_Developer = '/developer/14.0/update'; // version will only update when the api changes

  // TODO: use /windows/ instead of /desktop/
  API_Path_SubmitDiag = '/desktop/13.0/submitdiag'; // version will only update when the api changes
  API_Path_IsOnline = '/desktop/13.0/isonline'; // version will only update when the api changes

  // https://www.keyman.com/ - web pages
  URLPath_CreateTranslation = '/go/windows/'+SKeymanVersion+'/create-locale';
  URLPath_KeepInTouch = '/go/windows/'+SKeymanVersion+'/keep-in-touch';
  URLPath_KeymanHome = '/go/windows/'+SKeymanVersion+'/home';
  URLPath_ArchivedDownloads = '/go/windows/'+SKeymanVersion+'/archived-downloads';

  URLPath_KeymanLanguageLookup = '/go/developer/'+SKeymanVersion+'/language-lookup';
  URLPath_KeymanDeveloperDocumentation = '/go/developer/'+SKeymanVersion+'/docs';
  URLPath_KeymanDeveloperHome = '/go/developer/'+SKeymanVersion+'/home';
  URLPath_KeymanDeveloperHome_Presentation = '/developer';

  URLPath_KeymanDeveloper_HelpKeyboards = '/go/developer/'+SKeymanVersion+'/help-keyboards';
  URLPath_KeymanDeveloper_HelpPackages = '/go/developer/'+SKeymanVersion+'/help-packages';
  URLPath_KeymanDeveloper_HelpMobile = '/go/developer/'+SKeymanVersion+'/help-mobile';
  URLPath_KeymanDeveloper_KeymanWeb = '/go/developer/'+SKeymanVersion+'/keymanweb';
  URLPath_KeymanDeveloper_KeymanEngineHome = '/go/developer/'+SKeymanVersion+'/keyman-engine-home';

  URLPath_KeymanDeveloper_KeymanForAndroidDownload = '/go/developer/'+SKeymanVersion+'/android-app';
  URLPath_KeymanDeveloper_KeymanForIosDownload = '/go/developer/'+SKeymanVersion+'/ios-app';

  URLPath_KeymanDeveloper_KeyboardSearchForCloneKeymanCloud = '/go/developer/'+SKeymanVersion+'/clone-keyboard';

  URLPath_Support = '/go/'+SKeymanVersion+'/support';
  URLPath_Privacy = '/go/'+SKeymanVersion+'/privacy';
  URLPath_Privacy_Presentation = '/privacy';
  URLPath_Community = '/go/'+SKeymanVersion+'/community';

  // Keyboard download and installation
  URLPath_RegEx_MatchKeyboardsInstall = '^http(?:s)?://keyman(?:-staging)?\.com(?:\.local)?/keyboards/install/([^?/]+)(?:\?(.+))?$';
                                  // e.g. https://keyman.com/keyboards/install/foo
  UrlPath_RegEx_MatchKeyboardsRoot = '^http(?:s)?://keyman(?:-staging)?\.com(?:\.local)?/keyboards([/?].*)?$';
                               // e.g. http://keyman.com.local/keyboards/foo
  UrlPath_RegEx_MatchKeyboardsGo = '^http(?:s)?://keyman(?:-staging)?\.com(?:\.local)?/go/windows/[^/]+/download-keyboards';
                             // e.g. https://keyman-staging.com/go/windows/14.0/download-keyboards?version=14.0.146.0

  // Cloning keyboards - Keyman Developer
  URLSubPath_KeymanDeveloper_Clone_Keyboards = '/keyboards/';
  URLSubPath_KeymanDeveloper_Clone_Keyboards_Custom = '/keyboards/h/';

function URLPath_PackageDownload(const PackageID, BCP47: string; IsUpdate: Boolean): string;

function API_Protocol: string; // = 'https';
function API_Server: string; // = 'api.keyman.com';

function API_UserAgent: string; // = 'Keyman for Windows/<ver>...'
function API_UserAgent_Developer: string; // = 'Keyman Developer/<ver>...'
function API_UserAgent_Diagnostics: string;

function KeymanCom_Protocol_Server: string; // = 'https://keyman.com';

function MakeAPIURL(path: string): string;

function MakeKeymanURL(const path: string): string;

function URL_KmcMessage(id: string): string;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  DebugPaths,
  ErrorControlledRegistry,
  RegistryKeys,
  utilhttp,
  VersionInfo;

const
  S_UserAgent = 'Keyman for Windows';
  S_UserAgent_Developer = 'Keyman Developer';
  S_UserAgent_Diagnostics = 'Keyman for Windows Diagnostics';

  S_HelpKeymanCom = 'https://help.keyman.com';
  S_KeymanCom = 'https://keyman.com';
  S_APIProtocol = 'https';
  S_APIServer = 'api.keyman.com';

  // Alpha versions will work against the staging server so that they
  // can access new APIs etc that will only be available there. The staging
  // servers have resource constraints but should be okay for limited use.
  S_KeymanCom_Staging = 'https://keyman.com';  // #7227 disabling: 'https://keyman-staging.com';
  S_APIServer_Staging = 'api.keyman.com';  // #7227 disabling: 'api.keyman-staging.com';

const
  URLPath_PackageDownload_Format = '/go/package/download/%0:s?platform=windows&tier=%1:s&bcp47=%2:s&update=%3:d';

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
  if (CKeymanVersionInfo.Tier = TIER_ALPHA) or
    (CKeymanVersionInfo.Tier = TIER_BETA)
    then Result := S_KeymanCom_Staging
    else Result := S_KeymanCom;
  Result := GetDebugPath('Debug_KeymanCom', Result, False);
end;

function API_Protocol: string; // = 'https';
begin
  Result := GetDebugPath('Debug_APIProtocol', S_APIProtocol, False);
end;

function API_Server: string; // = 'api.keyman.com';
begin
  if (CKeymanVersionInfo.Tier = TIER_ALPHA) or
    (CKeymanVersionInfo.Tier = TIER_BETA)
    then Result := S_APIServer_Staging
    else Result := S_APIServer;
  Result := GetDebugPath('Debug_APIServer', Result, False);
end;

function MakeAPIURL(path: string): string;
begin
  Result := API_Protocol + '://' + API_Server + path;
end;

function URLPath_PackageDownload(const PackageID, BCP47: string; IsUpdate: Boolean): string;
var
  IsUpdateInt: Integer;
begin
  if IsUpdate then IsUpdateInt := 1 else IsUpdateInt := 0;

  Result := Format(URLPath_PackageDownload_Format, [URLEncode(PackageID), URLEncode(CKeymanVersionInfo.Tier), URLEncode(BCP47), IsUpdateInt]);
end;

function URL_KmcMessage(id: string): string;
begin
   Result := S_HelpKeymanCom + '/developer/'+GetMajorMinorVersionString+'/reference/messages/'+id.ToLower;
end;

end.
