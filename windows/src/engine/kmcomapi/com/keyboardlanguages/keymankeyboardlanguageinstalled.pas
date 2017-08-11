(*
  Name:             keymankeyboardlanguageinstalled
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Apr 2014

  Modified Date:    4 Nov 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    17 Aug 2014 - mcdurdin - I4376 - V9.0 - Unticked keyboards in configuration should be removed from language profile
                    04 Nov 2014 - mcdurdin - I4494 - Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError]
*)
unit keymankeyboardlanguageinstalled;   // I4169

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,

  keymanautoobject,
  keymanapi_TLB,
  internalinterfaces,
  keymancontext,
  msctf;

type
  TKeymanKeyboardLanguageInstalled = class(TKeymanAutoObject, IKeymanKeyboardLanguageInstalled, IIntKeymanKeyboardLanguage)   // I4376
  private
    FContext: TKeymanContext;
    FOwner: IKeymanKeyboardInstalled;
    FBCP47Code: string;
    FLangID: Integer;
    FProfileGUID: TGUID;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
      override;

    { IKeymanKeyboardLanguageInstalled }
    function Get_BCP47Code: WideString; safecall;
    function Get_LangID: Integer; safecall;
    function Get_OwnerKeyboard: IKeymanKeyboardInstalled; safecall;
    function Get_ProfileGUID: TGUID; safecall;

    procedure Uninstall; safecall;

    { IIntKeymanKeyboardLanguageInstalled }
    procedure ApplyEnabled(pInputProcessorProfiles: ITfInputProcessorProfiles; AEnabled: Boolean);   // I4376
  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardInstalled; ABCP47Code: string; ALangID: Integer; AProfileGUID: TGUID);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,

  glossary,
  keymanerrorcodes,
  kpuninstallkeyboardlanguageprofiles,
  utiltsf,
  utilxml;

{ TKeymanKeyboardLanguageInstalled }

procedure TKeymanKeyboardLanguageInstalled.ApplyEnabled(
  pInputProcessorProfiles: ITfInputProcessorProfiles; AEnabled: Boolean);   // I4376
var
  AEnabledInt: Integer;
begin
  if AEnabled then AEnabledInt := 1 else AEnabledInt := 0;
  try   // I4494
    OleCheck(pInputProcessorProfiles.EnableLanguageProfile(c_clsidKMTipTextService,
      FLangID, FProfileGUID, AEnabledInt));
  except
    on E:EOleException do
    begin
      FContext.Errors.AddFmt(KMN_W_TSF_COMError, VarArrayOf(['EOleException: '+E.Message+' ('+E.Source+', '+IntToHex(E.ErrorCode,8)+')']), kesWarning);
    end;
    on E:EOleSysError do
    begin
      FContext.Errors.AddFmt(KMN_W_TSF_COMError, VarArrayOf(['EOleSysError: '+E.Message+' ('+IntToHex(E.ErrorCode,8)+')']), kesWarning);
    end;
    on E:Exception do
    begin
      FContext.Errors.AddFmt(KMN_W_TSF_COMError, VarArrayOf([E.Message]), kesWarning);
    end;
  end;
end;

constructor TKeymanKeyboardLanguageInstalled.Create(AContext: TKeymanContext;
  AOwner: IKeymanKeyboardInstalled; ABCP47Code: string; ALangID: Integer; AProfileGUID: TGUID);
begin
  FContext := AContext;
  FOwner := AOwner;
  FBCP47Code := ABCP47Code;
  FLangID := ALangID;
  FProfileGUID := AProfileGUID;
  inherited Create(AContext, IKeymanKeyboardLanguageInstalled);
end;

function TKeymanKeyboardLanguageInstalled.Get_BCP47Code: WideString;
begin
  Result := FBCP47Code;
end;

function TKeymanKeyboardLanguageInstalled.Get_LangID: Integer;
begin
  Result := FLangID;
end;

function TKeymanKeyboardLanguageInstalled.Get_OwnerKeyboard: IKeymanKeyboardInstalled;
begin
  Result := FOwner;
end;

function TKeymanKeyboardLanguageInstalled.Get_ProfileGUID: TGUID;
begin
  Result := FProfileGUID;
end;

function TKeymanKeyboardLanguageInstalled.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
var
  i: Integer;
  Language: IKeymanLanguage;
  LangName: string;
begin
  for i := 0 to (Context.Languages as IKeymanLanguages).Count - 1 do
  begin
    Language := (Context.Languages as IKeymanLanguages)[i];
    if Language.LangID = Get_LangID then
    begin
      LangName := Language.LocaleName;
      Break;
    end;
  end;

  if LangName = '' then
    {ignore result := } GetLanguageName(Get_LangID, LangName);

  Result := XMLFormat([
    'langid', IntToHex(Get_LangID, 4),
    'profileguid', GUIDToString(FProfileGUID),
    'bcp47code', FBCP47Code,
    'langname', LangName]);
end;

procedure TKeymanKeyboardLanguageInstalled.Uninstall;
begin
  with TKPUninstallKeyboardLanguageProfiles.Create(Context) do
  try
    Execute(FOwner.ID, FBCP47Code);
  finally
    Free;
  end;
end;

end.
