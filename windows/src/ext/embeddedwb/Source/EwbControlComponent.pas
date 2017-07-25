//***********************************************************
//                       EwbControl component               *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                          (smot)                          *
//                                                          *
//  Documentation and updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************
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
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EwbControlComponent;

interface

{$I EWB.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, EWBMouseHook;

{============================================================================}
// Mouse WheelFix
{============================================================================}
type
  TMouseWheelFix = class(TPersistent)
  private
    FActive: Boolean;
    FActiveFormOnly: Boolean;
    FEWBMouseHook: TEWBMouseHook;
    FDesignMode: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetActiveFormOnly(const Value: Boolean);
  public
    OnMouseWheel: TMouseWheelEvent;
  published
    property Active: Boolean read FActive write SetActive default True;
    property ActiveFormOnly: Boolean read FActiveFormOnly write SetActiveFormOnly
      default False;
  end;

  {============================================================================}
  // FocusControl
  {============================================================================}
type
  TFocusControl = class(TPersistent)
  private
    FActive: Boolean;
    FDesignMode: Boolean;
    procedure SetActive(const Value: Boolean);
  published
    property Active: Boolean read FActive write SetActive default True;
  end;

  {============================================================================}
  // OnMessage Handler
  {============================================================================}
{
type
  TMessageHandler = class(TPersistent)
  private
    FActive: Boolean;
    FDesignMode: Boolean;
    FOnMessage: TMessageEvent;
    procedure SetActive(const Value: Boolean);
  published
    property Active: Boolean read FActive write SetActive default True;
  end;
}

{$IFDEF Enable_InternetFeatures}
  {============================================================================}
  { Feature Controls }
  { http://msdn.microsoft.com/en-us/library/ms537169(VS.85).aspx }
  { TInternetFeatures = (
    FEATURE_OBJECT_CACHING
    FEATURE_ZONE_ELEVATION
    FEATURE_MIME_HANDLING
    FEATURE_MIME_SNIFFING
    FEATURE_WINDOW_RESTRICTIONS
    FEATURE_WEBOC_POPUPMANAGEMENT
    FEATURE_BEHAVIORS
    FEATURE_DISABLE_MK_PROTOCOL
    FEATURE_LOCALMACHINE_LOCKDOWN
    FEATURE_SECURITYBAND
    FEATURE_RESTRICT_ACTIVEXINSTALL
    FEATURE_VALIDATE_NAVIGATE_URL
    FEATURE_RESTRICT_FILEDOWNLOAD
    FEATURE_ADDON_MANAGEMENT
    FEATURE_PROTOCOL_LOCKDOWN
    FEATURE_HTTP_USERNAME_PASSWORD_DISABLE
    FEATURE_SAFE_BINDTOOBJECT
    FEATURE_UNC_SAVEDFILECHECK
    FEATURE_GET_URL_DOM_FILEPATH_UNENCODED
    FEATURE_TABBED_BROWSING
    FEATURE_SSLUX
    FEATURE_DISABLE_NAVIGATION_SOUNDS
    FEATURE_DISABLE_LEGACY_COMPRESSION
    FEATURE_FORCE_ADDR_AND_STATUS
    FEATURE_XMLHTTP
    FEATURE_DISABLE_TELNET_PROTOCOL
    FEATURE_FEEDS
    FEATURE_BLOCK_INPUT_PROMPTS
    FEATURE_ENTRY_COUNT);
  }

  TInternetFeatureList = (
    ObjectCaching, ZoneElevation, MimeHandling, MimeSniffing, WindowRestrictions,
    WebocPopupManagement, Behaviors, DisableMkProtocol, LocalMachineLockDown,
    Securityband, RestrictActivexInstall, ValidateNavigateUrl, RestrictFileDownload,
    AddonManagement, ProtocolLockdown, HttpUsernamePasswordDisable, SafeBindToObject,
    UncSavedFileCheck, GetUrlDomFilePathUnencoded, TabbedBrowsing, Sslux,
    DisableNavigationSounds, DisableLegacyCompression, ForceAddrAndStatus,
    XmlHttp, DisableTelnetProtocol, Feeds, BlockInputPrompts, EntryCount
    );
  TInternetFeatures = set of TInternetFeatureList;
{$ENDIF Enable_InternetFeatures}

{============================================================================}
// TEwbControl
{============================================================================}

type
  TEwbControl = class(TComponent)
  private
    { Private declarations }
    FMouseWheelFix: TMouseWheelFix;
    FFocusControl: TFocusControl;
    //  FMessageHandler: TMessageHandler;
    //  FOnMessage: TMessageEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FDesignMode: Boolean;
{$IFDEF Enable_InternetFeatures}
    FInternetFeatures: TInternetFeatures;
    procedure SetInternetFeatures(const Value: TInternetFeatures);
{$ENDIF}
    //  procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
  protected
    { Protected declarations }
    //  procedure ProcessWBEvents(var Msg: TMsg; var Handled: Boolean);
{$IFDEF Enable_InternetFeatures}
    procedure UpdateInternetFeatures;
    procedure SetDefaultInternetFeatures;
{$ENDIF}

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    { Published declarations }
    property MouseWheelFix: TMouseWheelFix read FMouseWheelFix write
      FMouseWheelFix;
    property FocusControl: TFocusControl read FFocusControl write FFocusControl;
    // property MessageHandler: TMessageHandler read FMessageHandler write FMessageHandler;
    // property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write
      FOnMouseWheel;
{$IFDEF Enable_InternetFeatures}
    property InternetFeatures: TInternetFeatures read FInternetFeatures
      write SetInternetFeatures default [];
{$ENDIF}
  end;

implementation

uses
  EwbFocusControl, EwbAcc, EwbIEConst;

procedure TFocusControl.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not FDesignMode then
    TEWBFocusControl.Activate(Value);
end;

{procedure TMessageHandler.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not FDesignMode then
  begin
    if FActive then
      Application.OnMessage := FOnMessage;
  end;
end; }

procedure TMouseWheelFix.SetActiveFormOnly(const Value: Boolean);
begin
  FActiveFormOnly := Value;
  if Assigned(FEWBMouseHook) then
    FEWBMouseHook.FActiveFormOnly := FActiveFormOnly;
end;

procedure TMouseWheelFix.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not FDesignMode then
    if Value then
    begin
      if FEWBMouseHook = nil then
      begin
        FEWBMouseHook := TEWBMouseHook.Create;
        FEWBMouseHook.OnMouseWheel := OnMouseWheel;
        FEWBMouseHook.FActiveFormOnly := FActiveFormOnly;
        FEWBMouseHook.Activate;
      end;
    end
    else
    begin
      if Assigned(FEWBMouseHook) then
      begin
        try
          FEWBMouseHook.Deactivate;
        finally
          FreeAndNil(FEWBMouseHook);
        end;
      end;
    end;
end;

procedure TEwbControl.Loaded;
begin
  inherited Loaded;
  if Assigned(OnMouseWheel) then
    FMouseWheelFix.OnMouseWheel := OnMouseWheel;
  {  if Assigned(FMessageHandler) then
      FMessageHandler.FOnMessage := DoMessage; }
end;

constructor TEwbControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDesignMode := (csDesigning in ComponentState);

  FMouseWheelFix := TMouseWheelFix.Create;
  FMouseWheelFix.FDesignMode := FDesignMode;
  FMouseWheelFix.FActive := True;

  FFocusControl := TFocusControl.Create;
  FFocusControl.FDesignMode := FDesignMode;
  FFocusControl.FActive := True;

{$IFDEF Enable_InternetFeatures}
  if FDesignMode then
    SetDefaultInternetFeatures;
{$ENDIF}

  {  FMessageHandler := TMessageHandler.Create;
    FMessageHandler.FDesignMode := FDesignMode;
    FMessageHandler.FOnMessage := DoMessage;

    FMessageHandler.FActive := True; }
end;

destructor TEwbControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
  {  if Assigned(FMessageHandler) then
    begin
      FMessageHandler.Active := False;
      Application.OnMessage := nil;
      FreeAndNil(FMessageHandler);
    end;  }
    if Assigned(FMouseWheelFix) then
    begin
      FMouseWheelFix.Active := False;
      FreeAndNil(FMouseWheelFix);
    end;
    if Assigned(FFocusControl) then
    begin
      FFocusControl.Active := False;
      FreeAndNil(FFocusControl);
    end;
  end;
  inherited Destroy;
end;

{$IFDEF Enable_InternetFeatures}

procedure TEwbControl.UpdateInternetFeatures;
var
  dco: TInternetFeatureList;
begin
  if (FInternetFeatures <> []) then
    for dco := Low(TInternetFeatureList) to High(TInternetFeatureList) do
      CoInternetSetFeatureEnabled(TInternetFeature(dco), FEATURE_FROM_PROCESS, (dco in FInternetFeatures));
end;

procedure TEwbControl.SetInternetFeatures(const Value: TInternetFeatures);
begin
  FInternetFeatures := Value;
  UpdateInternetFeatures;
end;

procedure TEwbControl.SetDefaultInternetFeatures;
var
  dco: TInternetFeatureList;
begin
  FInternetFeatures := [];
  for dco := Low(TInternetFeatureList) to High(TInternetFeatureList) do
    if CoInternetIsFeatureEnabled(TInternetFeature(dco), FEATURE_FROM_PROCESS) = S_OK then
      FInternetFeatures := FInternetFeatures + [TInternetFeatureList(dco)];
end;

{$ENDIF}

{
procedure TEwbControl.ProcessWBEvents(var Msg: TMsg; var Handled: Boolean);
begin
end;

procedure TEwbControl.DoMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(Msg, Handled);
  end;
  ProcessWBEvents(Msg, Handled);
end;
}

end.
