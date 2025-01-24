(*
  Name:             utilkeymanoption
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    6 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Remove descriptions (localized)
                    16 May 2007 - mcdurdin - I819 - Add Test Keyman Functioning option
                    30 May 2007 - mcdurdin - I765 - Add support for releasing shift keys after key press
                    30 May 2007 - mcdurdin - Refactor OS version references for newer version checks
                    27 Mar 2008 - mcdurdin - I1287 - Switch language and keyboard together
                    27 Mar 2008 - mcdurdin - I1288 - Auto open OSK
                    27 Mar 2008 - mcdurdin - I1375 - Auto switch OSK pages
                    27 Mar 2008 - mcdurdin - I1256 - Hint system
                    12 Aug 2008 - mcdurdin - Avoid crash with missing option
                    27 Jan 2009 - mcdurdin - I1817 - Keyman Uniscribe manager integration
                    06 Apr 2010 - mcdurdin - I2286 - Enable One keyboard for system option
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3331 - V9.0 - Remove koRunElevatedInVista option
                    01 Dec 2012 - mcdurdin - I3620 - V9.0 - Remove obsolete koTurnOnSurrogates and koUnknownLanguage options
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    28 May 2014 - mcdurdin - I4243 - V9.0 - Remove kmuspmgr and keymanuc
                    16 Jun 2014 - mcdurdin - I4271 - V9.0 - Switch language for all applications is not working
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman D_esktop Free Edition polish
                    14 Nov 2014 - mcdurdin - I4515 - V9.0 - Switch for all apps is not disabled in Win 8
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
*)
unit utilkeymanoption;  // I3306   // I4220   // I4243

interface

uses
  System.Classes,
  System.Contnrs,
  System.Win.Registry,
  keymanapi_TLB,
  KeymanContext,
  KeymanOptionNames,
  ErrorControlledRegistry,
  RegistryKeys;

type
  TUtilKeymanOptionEntry = class
  private
    FRegistryName: string;
    Fopt: TUtilKeymanOption;
    FEnabled: Boolean;
    FGroup: string;
    FDefaultStringValue: string;
    FDefaultIntValue: Integer;
    FBoolValue: Boolean;
    FStringValue: string;
    FIntValue: Integer;
    FOptionType: KeymanOptionType;
    FDefaultBoolValue: Boolean;
    procedure Load(reg: TRegistryErrorControlled);  // I2890
    procedure Save(AContext: TKeymanContext; reg: TRegistryErrorControlled);  // I2890
    function GetID: string;
    procedure CustomLoad;
    procedure CustomSave;
  public
    property opt: TUtilKeymanOption read Fopt;
    property Enabled: Boolean read FEnabled;
    property OptionType: KeymanOptionType read FOptionType;
    property Group: string read FGroup;
    property ID: string read GetID;
    property StringValue: string read FStringValue write FStringValue;
    property IntValue: Integer read FIntValue write FIntValue;
    property BoolValue: Boolean read FBoolValue write FBoolValue;
    property DefaultStringValue: string read FDefaultStringValue;
    property DefaultIntValue: Integer read FDefaultIntValue;
    property DefaultBoolValue: Boolean read FDefaultBoolValue;
  end;

  TUtilKeymanOptionEntryList = class(TObjectList)
  protected
    function Get(Index: Integer): TUtilKeymanOptionEntry;
    procedure Put(Index: Integer; Item: TUtilKeymanOptionEntry);
  public
    property Items[Index: Integer]: TUtilKeymanOptionEntry read Get write Put; default;
    function Add(Item: TUtilKeymanOptionEntry): Integer;
    function IndexOfOpt(opt: TUtilKeymanOption): Integer;
  end;

  TUtilKeymanOptions = class
  private
    FOptions: TUtilKeymanOptionEntryList;
    function GetOption(opt: TUtilKeymanOption): TUtilKeymanOptionEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save(AContext: TKeymanContext);
    function OptionByName(const Name: string): TUtilKeymanOptionEntry;
    property Option[opt: TUtilKeymanOption]: TUtilKeymanOptionEntry read GetOption; default;
  end;

type
  TKeymanOptionInfo = record
    opt: TUtilKeymanOption;
    RegistryName: string;   // if blank, then don't read setting from registry -- will be custom processed

    OptionType: KeymanOptionType;

    BoolValue: Boolean;       // default state if entry is not found
    StringValue: string;
    IntValue: Integer;

    GroupName: string;
  end;

const KeymanOptionInfo: array[0..17] of TKeymanOptionInfo = (  // I3331   // I3620   // I4552
  // Global options

  (opt: koKeyboardHotkeysAreToggle;          RegistryName: SRegValue_KeyboardHotkeysAreToggle;         OptionType: kotBool; BoolValue: False; GroupName: 'kogGeneral'),
  (opt: koSwitchLanguageForAllApplications;  RegistryName: SRegValue_SwitchLanguageForAllApplications; OptionType: kotBool; BoolValue: True;  GroupName: 'kogGeneral'), // I2277   // I4393
  (opt: koAltGrCtrlAlt;                      RegistryName: SRegValue_AltGrCtrlAlt;                     OptionType: kotBool; BoolValue: False; GroupName: 'kogGeneral'),
  (opt: koRightModifierHK;                   RegistryName: SRegValue_AllowRightModifierHotKey;           OptionType: kotBool; BoolValue: False; GroupName: 'kogGeneral'),
  (opt: koShowHints;                         RegistryName: SRegValue_EnableHints;                      OptionType: kotBool; BoolValue: True;  GroupName: 'kogGeneral'),
  (opt: koBaseLayout;                        RegistryName: SRegValue_UnderlyingLayout;                 OptionType: kotLong; IntValue:  0;     GroupName: 'kogGeneral'),
  (opt: koAutomaticUpdate;                   RegistryName: SRegValue_AutomaticUpdates;                 OptionType: kotBool; BoolValue: True;  GroupName: 'kogGeneral'),

  (opt: koAutomaticallyReportErrors;         RegistryName: SRegValue_AutomaticallyReportErrors;        OptionType: kotBool; BoolValue: True;  GroupName: 'kogGeneral'),   // I4393
  (opt: koAutomaticallyReportUsage;          RegistryName: SRegValue_AutomaticallyReportUsage;         OptionType: kotBool; BoolValue: True;  GroupName: 'kogGeneral'),   // I4393

  // OSK options

  (opt: koReleaseShiftKeysAfterKeyPress;     RegistryName: SRegValue_ReleaseShiftKeysAfterKeyPress;    OptionType: kotBool; BoolValue: False; GroupName: 'kogOSK'),
  (opt: koAutoOpenOSK;                       RegistryName: SRegValue_AutoOpenOSK;                      OptionType: kotBool; BoolValue: True;  GroupName: 'kogOSK'),
  (opt: koAutoSwitchOSKPages;                RegistryName: SRegValue_AutoSwitchOSKPages;               OptionType: kotBool; BoolValue: True;  GroupName: 'kogOSK'),

  // Startup options

  (opt: koStartWithWindows;                  RegistryName: '';                                         OptionType: kotBool; BoolValue: False; GroupName: 'kogStartup'),
  (opt: koShowStartup;                       RegistryName: SRegValue_ShowStartup;                      OptionType: kotBool; BoolValue: True;  GroupName: 'kogStartup'),
  (opt: koCheckForUpdates;                   RegistryName: SRegValue_CheckForUpdates;                  OptionType: kotBool; BoolValue: True;  GroupName: 'kogStartup'),
  (opt: koTestKeymanFunctioning;             RegistryName: SRegValue_TestKeymanFunctioning;            OptionType: kotBool; BoolValue: True;  GroupName: 'kogStartup'),

  // Advanced options

  (opt: koDeadkeyConversion;                 RegistryName: SRegValue_DeadkeyConversionMode;            OptionType: kotBool; BoolValue: True;  GroupName: 'kogAdvanced'),   // I4552
  (opt: koDebugging;                         RegistryName: SRegValue_KeymanDebug;                      OptionType: kotBool; BoolValue: False; GroupName: 'kogAdvanced')   // I4393
);

implementation

uses
  System.TypInfo,
  System.Variants,
  GetOSVersion,
  keymanerrorcodes,
  KeymanPaths,
  SysUtils,
  Windows;

{ TUtilKeymanOptions }

constructor TUtilKeymanOptions.Create;
var
  i: Integer;
  o: TUtilKeymanOptionEntry;
begin
  FOptions := TUtilKeymanOptionEntryList.Create;

  for i := Low(KeymanOptionInfo) to High(KeymanOptionInfo) do
  begin
    o := TUtilKeymanOptionEntry.Create;
    o.Fopt := KeymanOptionInfo[i].opt;
    o.FRegistryName := KeymanOptionInfo[i].RegistryName;
    o.FOptionType := KeymanOptionInfo[i].OptionType;

    o.FDefaultStringValue := KeymanOptionInfo[i].StringValue;
    o.FStringValue := KeymanOptionInfo[i].StringValue;

    o.FDefaultBoolValue := KeymanOptionInfo[i].BoolValue;
    o.FBoolValue := KeymanOptionInfo[i].BoolValue;

    o.FDefaultIntValue := KeymanOptionInfo[i].IntValue;
    o.FIntValue := KeymanOptionInfo[i].IntValue;

    o.FEnabled := True;

    if (KeymanOptionInfo[i].opt = koSwitchLanguageForAllApplications) and
      not (GetOs in [osVista, osWin7]) then
    begin
      o.FEnabled := False;   // I4271
      o.FBoolValue := False;   // I4515
      o.FDefaultBoolValue := False;   // I4515
    end;

    o.FGroup := KeymanOptionInfo[i].GroupName;

    FOptions.Add(o);
  end;
end;

destructor TUtilKeymanOptions.Destroy;
begin
  inherited;
  FOptions.Free;
end;

function TUtilKeymanOptions.GetOption(opt: TUtilKeymanOption): TUtilKeymanOptionEntry;
var
  n: Integer;
begin
  n := FOptions.IndexOfOpt(opt);
  if n = -1
    then Result := nil
    else Result := FOptions[n];
end;

procedure TUtilKeymanOptions.Load;
var
  reg: TRegistryErrorControlled;  // I2890
  i: Integer;
begin
  reg := TRegistryErrorControlled.Create;  // I2890
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) then
      for i := 0 to FOptions.Count - 1 do
        FOptions[i].Load(reg);

    reg.CloseKey;
  finally
    reg.Free;
  end;
end;

procedure TUtilKeymanOptions.Save(AContext: TKeymanContext);
var
  reg: TRegistryErrorControlled;  // I2890
  i: Integer;
begin
  reg := TRegistryErrorControlled.Create;  // I2890
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(SRegKey_KeymanEngine_CU, True) then
      for i := 0 to FOptions.Count - 1 do
        FOptions[i].Save(AContext, reg);

    reg.CloseKey;
  finally
    reg.Free;
  end;
end;

function TUtilKeymanOptions.OptionByName(const Name: string): TUtilKeymanOptionEntry;
var
  i: TUtilKeymanOption;
begin
  i := TUtilKeymanOption(GetEnumValue(TypeInfo(TUtilKeymanOption), Name));
  if Ord(i) = -1 then
    Exit(nil);

  Result := Option[i];
end;

{ TUtilKeymanOptionEntryList }

function TUtilKeymanOptionEntryList.Add(Item: TUtilKeymanOptionEntry): Integer;
begin
  Result := inherited Add(Item);
end;

function TUtilKeymanOptionEntryList.Get(Index: Integer): TUtilKeymanOptionEntry;
begin
  Result := TUtilKeymanOptionEntry(inherited Get(Index));
end;

function TUtilKeymanOptionEntryList.IndexOfOpt(opt: TUtilKeymanOption): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].opt = opt then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TUtilKeymanOptionEntryList.Put(Index: Integer; Item: TUtilKeymanOptionEntry);
begin
  inherited Put(Index, Item);
end;

{ TUtilKeymanOptionEntry }

procedure TUtilKeymanOptionEntry.CustomLoad;
begin
  if Fopt = koStartWithWindows then
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      FBoolValue := OpenKeyReadOnly('\'+SRegKey_WindowsRun_CU) and ValueExists(SRegValue_WindowsRun_Keyman);
    finally
      Free;
    end;
  end;
end;

procedure TUtilKeymanOptionEntry.CustomSave;
begin
  if Fopt = koStartWithWindows then
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKey('\'+SRegKey_WindowsRun_CU, True) then
      begin
        if FBoolValue then
          WriteString(SRegValue_WindowsRun_Keyman, '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell)+'" -boot')
        else if ValueExists(SRegValue_WindowsRun_Keyman) then
          DeleteValue(SRegValue_WindowsRun_Keyman);
      end;
    finally
      Free;
    end;
  end;
end;

function TUtilKeymanOptionEntry.GetID: string;
begin
  Result := GetEnumName(TypeInfo(TUtilKeymanOption), Ord(Fopt));
end;

procedure TUtilKeymanOptionEntry.Load(reg: TRegistryErrorControlled);  // I2890
begin
  if FRegistryName = '' then
  begin
    CustomLoad;
    Exit;
  end;

  if reg.ValueExists(FRegistryName) then
    case FOptionType of
      kotBool: FBoolValue := reg.ReadBool(FRegistryName);
      kotLong: FIntValue := StrToIntDef('$'+reg.ReadString(FRegistryName), 0);
      kotString: FStringValue := reg.ReadString(FRegistryName);
    end
  else
    case FOptionType of
      kotBool: FBoolValue := FDefaultBoolValue;
      kotLong: FIntValue := FDefaultIntValue;
      kotString: FStringValue := FDefaultStringValue;
    end
end;

procedure TUtilKeymanOptionEntry.Save(AContext: TKeymanContext; reg: TRegistryErrorControlled);  // I2890
begin
  try
    if FRegistryName = '' then
    begin
      CustomSave;
      Exit;
    end;

    case FOptionType of
      kotBool:
        if FBoolValue <> FDefaultBoolValue then
        begin
          reg.WriteBool(FRegistryName, FBoolValue);
          Exit;
        end;
      kotLong:
        if FIntValue <> FDefaultIntValue then
        begin
          reg.WriteString(FRegistryName, IntToHex(FIntValue, 8));
          Exit;
        end;
      kotString:
        if FStringValue <> FDefaultStringValue then
        begin
          reg.WriteString(FRegistryName, FStringValue);
          Exit;
        end;
    end;
    // If we get here, then the value must be default, so
    // remove from registry
    if reg.ValueExists(FRegistryName) then
      reg.DeleteValue(FRegistryName);
  except
    on E:ERegistryException do
    begin
      AContext.Errors.AddFmt(KMN_W_Options_UnableToSaveValue, VarArrayOf([GetID]), kesWarning);
      // Add an error entry
    end;
  end;
end;

end.

