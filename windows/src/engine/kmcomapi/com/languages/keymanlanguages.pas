(*
  Name:             keymanlanguages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add AutoApplyKeyman call
                    04 Dec 2006 - mcdurdin - Disable range checks
                    05 Nov 2007 - mcdurdin - I1087 - Language switching hotkeys
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    18 Mar 2011 - mcdurdin - I2780 - Spurious languages appearing
                    05 Apr 2011 - mcdurdin - I2864 - TSF interfaces not supported on XP, check fails
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    03 May 2011 - mcdurdin - I2870 - Crash when reading list of languages and unexpected number of languages
                    24 Jan 2012 - mcdurdin - I3149 - If TSF is not installed then Keyman crashes on startup
                    04 Nov 2012 - mcdurdin - I3539 - V9.0 - Merge of I3149 - If TSF is not installed then Keyman crashes on startup
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
*)
unit keymanlanguages;

{$R-}

interface

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  keymanapi_TLB,
  System.Win.StdVcl,
  keymancontext,
  keymanautoobject,
  internalinterfaces,
  keymanlanguage,
  Windows8LanguageList;

type
  TLanguageList = TAutoObjectList;

  TKeymanLanguages = class(TKeymanAutoCollectionObject, IKeymanLanguages)
  private
    FLanguages: TLanguageList;
    FWin8Languages: TWindows8LanguageList;
  protected
    procedure DoRefresh; override;

    { IKeymanLanguages }
    function Get_Items(Index: Integer): IKeymanLanguage; safecall;
    procedure Apply; safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  System.Variants,
  ComServ,
  Windows,
  Classes,
  KLog,
  Winapi.msctf,
  keyman_msctf,
  System.StrUtils,
  SysUtils,
  ErrorControlledRegistry,
  Registry,
  RegistryKeys,
  keymanerrorcodes,
  tempfilemanager,
  utilexecute,
  utilhandleexception;

function TKeymanLanguages.Get_Items(Index: Integer): IKeymanLanguage;
begin
  if (Index < Get_Count) and (Index >= 0) then
    Result := FLanguages[Index] as IKeymanLanguage
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([Index]));
end;

   // I4220
procedure TKeymanLanguages.DoRefresh;
var
  i: LongWord;
  pLangID: PWord;
  ulCount: LongWord;
  FProfiles: ITfInputProcessorProfiles;
  FProfileMgr: keyman_msctf.ITfInputProcessorProfileMgr;
  profile: keyman_msctf.TF_INPUTPROCESSORPROFILE;
  pcFetch: LongWord;
  ppEnum: keyman_msctf.IEnumTfInputProcessorProfiles;
  FCaption: WideString;
  FWin8Language: TWindows8Language;
  j: Integer;
  pLangIDRoot: PWord;
begin
  KL.MethodEnter(Self,'DoRefresh',[]);

  try
    FWin8Languages.Refresh;

    FLanguages.Clear;
    FProfiles := CreateComObject(CLASS_TF_InputProcessorProfiles) as ITfInputProcessorProfiles;
    FProfileMgr := FProfiles as keyman_msctf.ITfInputProcessorProfileMgr;

    pLangIDRoot := nil; // I2870
    ulCount := 0; // I2870
    FProfiles.GetLanguageList(@pLangIDRoot, ulCount);
    if ulCount > 0 then // I2870
    begin
      for I := 0 to ulCount - 1 do
      begin
        pLangID := pLangIDRoot;
        Inc(pLangID, I);

        FWin8Language := nil;
        if FWin8Languages.IsSupported then
        begin
          // Find the corresponding Win8 language registry entry
          // If it is missing, it usually means the language is
          // dynamically loaded, but even in that situation we can
          // still provide information for hotkeys, etc
          for j := 0 to FWin8Languages.Count - 1 do
            if FWin8Languages[J].LangID = pLangID^ then
            begin
              FWin8Language := FWin8Languages[j];
              Break;
            end;
        end;

        FProfileMgr.EnumProfiles(pLangID^, ppEnum);
        while ppEnum.Next(1, profile, pcFetch) = S_OK do
        begin
          if (profile.dwFlags and TF_IPP_FLAG_ENABLED) = 0 then
          begin
            // Keyman 14 no longer disables profiles when it is not running, so
            // we can always skip keyboards that are not enabled.
            Continue;
          end;

          if profile.dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
          begin
            FLanguages.Add(TKeymanLanguage.Create(Context, FWin8Language, profile, ''));
          end
          else if profile.catid = GUID_TFCAT_TIP_KEYBOARD then
          begin
            if Succeeded(FProfiles.GetLanguageProfileDescription(profile.clsid, profile.langid, profile.guidProfile, FCaption)) then
              FLanguages.Add(TKeymanLanguage.Create(Context, FWin8Language, profile, FCaption));
          end;
        end;
      end;
    end;

    if Assigned(pLangIDRoot) then
      CoTaskMemFree(pLangIDRoot);  // I2870
  except
    on E:Exception do
    begin
      LogException('TKeymanLanguages.DoRefresh', E, ExceptAddr);
      raise;
    end;
  end;

  KL.MethodExit(Self,'DoRefresh');
end;

constructor TKeymanLanguages.Create(AContext: TKeymanContext);
begin
  FLanguages := TLanguageList.Create;
  FWin8Languages := TWindows8LanguageList.Create(False);
  inherited Create(AContext, IKeymanLanguages, FLanguages);
  Refresh;
end;

destructor TKeymanLanguages.Destroy;
begin
  FLanguages.Free;
  inherited Destroy;
end;

procedure TKeymanLanguages.Apply;
var
  i: Integer;
  str: TStringList;
begin
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey('\'+SRegKey_LanguageHotkeys_CU, True) then
    begin
      GetValueNames(str);
      for i := 0 to str.Count - 1 do DeleteValue(str[i]);

      for i := 0 to FLanguages.Count - 1 do
        with (FLanguages[i] as IKeymanLanguage) do
        begin
          if Hotkey.RawValue <> 0 then
          begin
            if IsEqualGuid(ProfileGUID, EmptyGuid)
              then WriteString(IntToHex(HKL, 8), IntToStr(Hotkey.RawValue))
              else WriteString(GUIDToString(ProfileGUID), IntToStr(Hotkey.RawValue));
          end;
        end;
    end;
  finally
    Free;
    str.Free;
  end;
  Context.Control.AutoApplyKeyman;
end;

{$R+}

end.
