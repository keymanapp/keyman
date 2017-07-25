//**************************************************************
//                                                             *
//                          SecurityManager                    *                                                      *
//                     For Delphi 5 to XE                      *
//                     Freeware Component                      *
//                            by                               *
//                     Per Lindsø Larsen                       *
//                   per.lindsoe@larsen.dk                     *
//                                                             *
//  Contributions:                                             *
//  Eran Bodankin (bsalsa) bsalsa@gmail.com                    *
//         -  D2005 update                                     *
//                                                             *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: SecurityManager.pas,v 1.2 2006/11/15 21:01:44 sergev Exp $

unit SecurityManager;

interface

{$I EWB.inc}

uses
  Activex, UrlMon, Windows, SysUtils, Classes, EwbIEConst;
  
const
  DefaultActions: array[0..24] of DWORD = (
    $00001001, $00001004, $00001200, $00001201, $00001400, $00001402,
    $00001405, $00001406, $00001407, $00001601, $00001604, $00001605,
    $00001606, $00001607, $00001800, $00001802, $00001803, $00001804,
    $00001805, $00001A00, $00001A02, $00001A03, $00001A04, $00001C00,
    $00001E05);

type
  TJavaPolicyOption = (Prohibited, High, Medium, Low, Custom);
  TAllowDisAllowPolicyOption = (Allow, Disallow);

const
  JavaPolicyValues: array[0..4] of Cardinal =
  (URLPOLICY_JAVA_PROHIBIT,
    URLPOLICY_JAVA_HIGH,
    URLPOLICY_JAVA_MEDIUM,
    URLPOLICY_JAVA_LOW,
    URLPOLICY_JAVA_CUSTOM);
  AllowDisallowValues: array[0..1] of Cardinal = (URLPOLICY_ALLOW, URLPOLICY_DISALLOW);

type
  TActiveXOptions = TAllowDisAllowPolicyOption;
  TJavaPermissionsOptions = TJavaPolicyOption;
  TScriptOptions = TAllowDisallowPolicyOption;
  TSubmitFormsOptions = TAllowDisallowPolicyOption;
  TCrossDomainDataOptions = TAllowDisallowPolicyOption;

  TUrlPolicyOptions = class(TPersistent)
  private
    FActiveX: TActiveXOptions;
    FCrossDomainData: TCrossDomainDataOptions;
    FJavaPermissions: TJavaPermissionsOptions;
    FSubmitForms: TSubmitFormsOptions;
    FScriptOptions: TScriptOptions;
  published
    property ActiveX: TActiveXOptions read FActiveX write FActiveX;
    property CrossDomainData: TCrossDomainDataOptions read FCrossDomainData write FCrossDomainData;
    property SubmitForms: TSubmitFormsOptions read FSubmitForms write FSubmitForms;
    property JavaPermissions: TJavaPermissionsOptions read FJavaPermissions write FJavaPermissions;
    property Scripts: TScriptOptions read FScriptOptions write FScriptOptions;
  end;
  TSetSecuritySiteEvent = function(Site: IInternetSecurityMgrSite): HResult of object;
  TGetSecuritySiteEvent = function(out Site: IInternetSecurityMgrSite): HResult of object;
  TMapUrlToZoneEvent = function(pwszUrl: LPCWSTR; out dwZone: DWORD;
    dwFlags: DWORD): HResult of object;
  TGetSecurityIdEvent = function(pwszUrl: LPCWSTR; pbSecurityId: Pointer;
    var cbSecurityId: DWORD; dwReserved: DWORD): HResult of object;
  TProcessUrlActionEvent = function(pwszUrl: LPCWSTR; dwAction: DWORD;
    pPolicy: Pointer; cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
    dwFlags, dwReserved: DWORD): HResult of object;
  TQueryCustomPolicyEvent = function(pwszUrl: LPCWSTR; const guidKey: TGUID;
    out pPolicy: Pointer; out cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
    dwReserved: DWORD): HResult of object;
  TSetZoneMappingEvent = function(dwZone: DWORD; lpszPattern: LPCWSTR;
    dwFlags: DWORD): HResult of object;
  TGetZoneMappingsEvent = function(dwZone: DWORD; out enumString: IEnumString;
    dwFlags: DWORD): HResult of object;
  TSecurityManager = class(TComponent, IInternetSecurityManager)
  private
    { Private declarations }
    FSetSecuritySite: TSetSecuritySiteEvent;
    FGetSecuritySite: TGetSecuritySiteEvent;
    FMapUrlToZone: TMapUrlToZoneEvent;
    FGetSecurityID: TGetSecurityIDEvent;
    FProcessUrlAction: TProcessUrlActionEvent;
    FQueryCustomPolicy: TQueryCustomPolicyEvent;
    FSetZoneMapping: TSetZoneMappingEvent;
    FGetZoneMappings: TGetZoneMappingsEvent;
    FUrlPolicyOptions: TUrlPolicyOptions;
  protected
    { Protected declarations }
  public
    { Public declarations }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    function SetSecuritySite(Site: IInternetSecurityMgrSite): HResult; stdcall;
    function GetSecuritySite(out Site: IInternetSecurityMgrSite): HResult; stdcall;
    function MapUrlToZone(pwszUrl: LPCWSTR; out dwZone: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetSecurityId(pwszUrl: LPCWSTR; pbSecurityId: Pointer;
      var cbSecurityId: DWORD; dwReserved: DWORD): HResult; stdcall;
    function ProcessUrlAction(pwszUrl: LPCWSTR; dwAction: DWORD;
      pPolicy: Pointer; cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
      dwFlags, dwReserved: DWORD): HResult; stdcall;
    function QueryCustomPolicy(pwszUrl: LPCWSTR; const guidKey: TGUID;
      out pPolicy: Pointer; out cbPolicy: DWORD; pContext: Pointer; cbContext: DWORD;
      dwReserved: DWORD): HResult; stdcall;
    function SetZoneMapping(dwZone: DWORD; lpszPattern: LPCWSTR;
      dwFlags: DWORD): HResult; stdcall;
    function GetZoneMappings(dwZone: DWORD; out enumString: IEnumString;
      dwFlags: DWORD): HResult; stdcall;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property OnSetSecuritySite: TSetSecuritySiteEvent read FSetSecuritySite write FSetSecuritySite;
    property OnGetSecuritySite: TGetSecuritySiteEvent read FGetSecuritySite write FGetSecuritySite;
    property OnMapUrlToZone: TMapUrlToZoneEvent read FMapUrlToZone write FMapUrlToZone;
    property OnGetSecurityID: TGetSecurityIDEvent read FGetSecurityID write FGetSecurityID;
    property OnProcessUrlAction: TProcessUrlActionEvent read FProcessUrlAction write FProcessUrlAction;
    property OnQueryCustomPolicy: TQueryCustomPolicyEvent read FQueryCustomPolicy write FQueryCustomPolicy;
    property OnSetZoneMapping: TSetZoneMappingEvent read FSetZoneMapping write FSetZoneMapping;
    property OnGetZoneMappings: TGetZoneMappingsEvent read FGetZoneMappings write FGetZoneMappings;
    property UrlPolicy: TUrlPolicyOptions read FUrlPolicyOptions write FUrlPolicyOptions;
  end;

function DisplayAction(UrlAction: DWORD): string;
function DisplayPolicy(UrlAction, UrlPolicy: DWORD): string;

implementation

// Helper/debug function

function DisplayPolicy(UrlAction, UrlPolicy: DWORD): string;
begin
  case UrlPolicy of
    URLPOLICY_ALLOW: Result := 'URLPOLICY_ALLOW';
    URLPOLICY_DISALLOW: Result := 'URLPOLICY_DISALLOW';
    URLPOLICY_QUERY: Result := 'URLPOLICY_QUERY';
    URLPOLICY_ACTIVEX_CHECK_LIST: Result := 'URLPOLICY_ACTIVEX_CHECK_LIST';
    URLPOLICY_MASK_PERMISSIONS: Result := 'URLPOLICY_MASK_PERMISSIONS';
    URLPOLICY_LOG_ON_DISALLOW: Result := 'URLPOLICY_LOG_ON_DISALLOW';
    URLPOLICY_LOG_ON_ALLOW: Result := 'URLPOLICY_LOG_ON_ALLOW';
    URLPOLICY_NOTIFY_ON_DISALLOW: Result := 'URLPOLICY_NOTIFY_ON_DISALLOW';
    URLPOLICY_NOTIFY_ON_ALLOW: Result := 'URLPOLICY_NOTIFY_ON_ALLOW';
  end;
  if UrlAction = URLACTION_CREDENTIALS_USE then
  begin
    if UrlPolicy = URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY then
      Result := 'URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY'
    else
      if UrlPolicy = URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT then
        Result := 'URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT'
      else
        if UrlPolicy = URLPOLICY_CREDENTIALS_MUST_PROMPT_USER then
          Result := 'URLPOLICY_CREDENTIALS_MUST_PROMPT_USER'
        else
          if UrlPolicy = URLPOLICY_CREDENTIALS_SILENT_LOGON_OK then
            Result := 'URLPOLICY_CREDENTIALS_SILENT_LOGON_OK';
  end
  else
    if UrlAction = URLACTION_CHANNEL_SOFTDIST_PERMISSIONS then
    begin
      if UrlPolicy = URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL then
        Result := 'URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL'
      else
        if UrlPolicy = URLPOLICY_CHANNEL_SOFTDIST_PRECACHE then
          Result := 'URLPOLICY_CHANNEL_SOFTDIST_PRECACHE'
        else
          if UrlPolicy = URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT then
            Result := 'URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT'
          else
    end
    else
      if UrlAction = URLACTION_JAVA_PERMISSIONS then
      begin
        if UrlPolicy = URLPOLICY_JAVA_CUSTOM then
          Result := 'URLPOLICY_JAVA_CUSTOM'
        else
          if UrlPolicy = URLPOLICY_JAVA_MEDIUM then
            Result := 'URLPOLICY_JAVA_MEDIUM'
          else
            if UrlPolicy = URLPOLICY_JAVA_LOW then
              Result := 'URLPOLICY_JAVA_LOW'
            else
              if UrlPolicy = URLPOLICY_JAVA_HIGH then
                Result := 'URLPOLICY_JAVA_HIGH'
              else
                if UrlPolicy = URLPOLICY_JAVA_PROHIBIT then
                  Result := 'URLPOLICY_JAVA_PROHIBIT';
      end;
end;

// Helper/debug function

function DisplayAction(UrlAction: DWORD): string;
begin
  case
    UrlAction of
    URLACTION_DOWNLOAD_SIGNED_ACTIVEX: Result := 'URLACTION_DOWNLOAD_SIGNED_ACTIVEX';
    URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX: Result := 'URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX';
    URLACTION_ACTIVEX_RUN: Result := 'URLACTION_ACTIVEX_RUN';
    URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY: Result := 'URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY';
    URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY: Result := 'URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY';
    URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY: Result := 'URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY';
    URLACTION_SCRIPT_OVERRIDE_SAFETY: Result := 'URLACTION_SCRIPT_OVERRIDE_SAFETY';
    URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY: Result := 'URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY';
    URLACTION_ACTIVEX_TREATASUNTRUSTED: Result := 'URLACTION_ACTIVEX_TREATASUNTRUSTED';
    URLACTION_CROSS_DOMAIN_DATA: Result := 'URLACTION_CROSS_DOMAIN_DATA';
    URLACTION_HTML_SUBFRAME_NAVIGATE: Result := 'URLACTION_HTML_SUBFRAME_NAVIGATE';
    URLACTION_HTML_USERDATA_SAVE: Result := 'URLACTION_HTML_USERDATA_SAVE';
    URLACTION_COOKIES: Result := 'URLACTION_COOKIES';
    URLACTION_COOKIES_SESSION: Result := 'URLACTION_COOKIES_SESSION';
    URLACTION_SCRIPT_PASTE: Result := 'URLACTION_SCRIPT_PASTE';
    URLACTION_SCRIPT_RUN: Result := 'URLACTION_SCRIPT_RUN';
    URLACTION_SCRIPT_JAVA_USE: Result := 'URLACTION_SCRIPT_JAVA_USE';
    URLACTION_SCRIPT_SAFE_ACTIVEX: Result := 'URLACTION_SCRIPT_SAFE_ACTIVEX';
    URLACTION_HTML_SUBMIT_FORMS: Result := 'URLACTION_HTML_SUBMIT_FORMS';
    URLACTION_HTML_SUBMIT_FORMS_FROM: Result := 'URLACTION_HTML_SUBMIT_FORMS_FROM';
    URLACTION_HTML_SUBMIT_FORMS_TO: Result := 'URLACTION_HTML_SUBMIT_FORMS_TO';
    URLACTION_HTML_FONT_DOWNLOAD: Result := 'URLACTION_HTML_FONT_DOWNLOAD';
    URLACTION_HTML_JAVA_RUN: Result := 'URLACTION_HTML_JAVA_RUN';
    URLACTION_SHELL_INSTALL_DTITEMS: Result := 'URLACTION_SHELL_INSTALL_DTITEMS';
    URLACTION_SHELL_MOVE_OR_COPY: Result := 'URLACTION_SHELL_MOVE_OR_COPY';
    URLACTION_SHELL_FILE_DOWNLOAD: Result := 'URLACTION_SHELL_FILE_DOWNLOAD';
    URLACTION_SHELL_VERB: Result := 'URLACTION_SHELL_VERB';
    URLACTION_SHELL_WEBVIEW_VERB: Result := 'URLACTION_SHELL_WEBVIEW_VERB';
    URLACTION_CREDENTIALS_USE: Result := 'URLACTION_CREDENTIALS_USE';
    URLACTION_CLIENT_CERT_PROMPT: Result := 'URLACTION_CLIENT_CERT_PROMPT';
    URLACTION_AUTHENTICATE_CLIENT: Result := 'URLACTION_AU:TICATE_CLIENT';
    URLACTION_JAVA_PERMISSIONS: Result := 'URLACTION_JAVA_PERMISSIONS';
    URLACTION_INFODELIVERY_NO_ADDING_CHANNELS: Result := 'URLACTION_INFODELIVERY_NO_ADDING_CHANNELS';
    URLACTION_INFODELIVERY_NO_EDITING_CHANNELS: Result := 'URLACTION_INFODELIVERY_NO_EDITING_CHANNELS';
    URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS: Result := 'URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS';
    URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS: Result := 'URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS';
    URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS: Result := 'URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS';
    URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS: Result := 'URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS';
    URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING: Result := 'URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING';
    URLACTION_CHANNEL_SOFTDIST_PERMISSIONS: Result := 'URLACTION_CHANNEL_SOFTDIST_PERMISSIONS';
  end;
end;

{ TSecurityManager }

constructor TSecurityManager.Create(Owner: TComponent);
begin
  inherited;
  FUrlPolicyOptions := TUrlPolicyOptions.Create;
end;

destructor TSecurityManager.Destroy;
begin
  FUrlPolicyOptions.Free;
  inherited;
end;

function TSecurityManager.GetSecurityId(pwszUrl: LPCWSTR;
  pbSecurityId: Pointer; var cbSecurityId: DWORD;
  dwReserved: DWORD): HResult;
begin
  if Assigned(FGetSecurityID) then
    Result := FGetSecurityID(pwszUrl, pbSecurityID, cbSecurityID, dwReserved)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.GetSecuritySite(
  out Site: IInternetSecurityMgrSite): HResult;
begin
  if Assigned(FGetSecuritySite) then
    Result := FGetSecuritySite(site)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.GetZoneMappings(dwZone: DWORD;
  out enumString: IEnumString; dwFlags: DWORD): HResult;
begin
  if Assigned(FGetZoneMappings) then
    Result := FGetZoneMappings(dwZone, enumString, dwFlags)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.MapUrlToZone(pwszUrl: LPCWSTR; out dwZone: DWORD;
  dwFlags: DWORD): HResult;
begin
  if Assigned(FMapUrlToZone) then
    Result := FMapUrlToZone(pwszUrl, dwZone, dwFlags)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.ProcessUrlAction(pwszUrl: LPCWSTR;
  dwAction: DWORD; pPolicy: Pointer; cbPolicy: DWORD; pContext: Pointer;
  cbContext, dwFlags, dwReserved: DWORD): HResult;
var
  dwPolicy: DWORD;
begin
  if Assigned(FProcessUrlAction) then
    Result := FProcessUrlAction(pwszUrl, dwAction, pPolicy, cbPolicy, pContext, cbContext, dwFlags, dwReserved)
  else
  begin
    Result := S_FALSE;
    dwPolicy := URLPOLICY_ALLOW;
    if (dwAction <= URLACTION_ACTIVEX_MAX) and (dwAction >= URLACTION_ACTIVEX_MIN)
      then
      dwPolicy := AllowDisallowValues[Ord(FUrlPolicyOptions.FActiveX)]
    else

      if ((dwAction <= URLACTION_JAVA_MAX) and (dwAction >= URLACTION_JAVA_MIN)) or
        (dwAction = URLACTION_HTML_JAVA_RUN) then
        dwPolicy := JavaPolicyValues[Ord(FUrlPolicyOptions.FJavaPermissions)]
      else

        if ((dwAction = URLACTION_HTML_SUBMIT_FORMS_TO) or (dwAction = URLACTION_HTML_SUBMIT_FORMS_FROM)) then
          dwPolicy := AllowDisallowValues[Ord(FUrlPolicyOptions.FSubmitforms)]
        else

          if (dwAction = URLACTION_CROSS_DOMAIN_DATA) then
            dwPolicy := AllowDisallowValues[Ord(FUrlPolicyOptions.FCrossDomainData)]
          else

            if (dwAction <= URLACTION_SCRIPT_MAX) and (dwAction >= URLACTION_SCRIPT_MIN) then
              dwPolicy := AllowDisallowValues[Ord(FUrlPolicyOptions.FScriptOptions)]
            else

              Result := INET_E_DEFAULT_ACTION;
    if (Result = S_FALSE) and (cbPolicy >= SizeOf(DWORD)) then
    begin
      Dword(ppolicy^) := dwpolicy;
      Result := S_OK;
    end;
  end;
end;

function TSecurityManager.QueryCustomPolicy(pwszUrl: LPCWSTR;
  const guidKey: TGUID; out pPolicy: Pointer; out cbPolicy: DWORD;
  pContext: Pointer; cbContext, dwReserved: DWORD): HResult;
begin
  if Assigned(FQueryCustomPolicy) then
    Result := FQueryCustomPolicy(pwszUrl, guidKey, pPolicy, cbPolicy, pContext, cbContext, dwReserved)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited QueryInterface(iid, obj);
end;

function TSecurityManager.SetSecuritySite(
  Site: IInternetSecurityMgrSite): HResult;
begin
  if Assigned(FSetSecuritySite) then
    Result := FSetSecuritySite(site)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

function TSecurityManager.SetZoneMapping(dwZone: DWORD;
  lpszPattern: LPCWSTR; dwFlags: DWORD): HResult;
begin
  if Assigned(FSetZoneMapping) then
    Result := FSetZoneMapping(dwZone, lpszPattern, dwFlags)
  else
    Result := INET_E_DEFAULT_ACTION;
end;

end.
