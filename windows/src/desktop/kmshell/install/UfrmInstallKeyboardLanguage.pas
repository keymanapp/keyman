(*
  Name:             UfrmInstallKeyboardLanguage
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      28 May 2014

  Modified Date:    1 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 May 2014 - mcdurdin - I4222 - V9.0 - Deprecate osWin2000, osWinXP, osWin2003Server
                    03 Aug 2014 - mcdurdin - I4322 - V9.0 - Specify custom languages (med-high) ? fixup language association dialog, also in installer.
                    01 Sep 2014 - mcdurdin - I4394 - V9.0 - Keyman Desktop Free Edition polish
                    01 Sep 2014 - mcdurdin - I4395 - V9.0 - Restrict associated languages to 1 for Light Edition
                    
*)
unit UfrmInstallKeyboardLanguage;   // I4322

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmKeymanBase, Vcl.OleCtrls,
  keymanapi_TLB,
  Vcl.StdCtrls, System.Generics.Collections, System.Generics.Defaults, Vcl.Grids;

type
  TInstLanguage = class;
  TInstLanguageVariant = class;

  TInstLanguageList = TObjectList<TInstLanguage>;
  TInstLanguageVariantList = TObjectList<TInstLanguageVariant>;

  TInstLanguageComparer = class(TComparer<TInstLanguage>)
  public
    function Compare(const Left, Right: TInstLanguage): Integer; override;
  end;

  TInstLanguageVariantComparer = class(TComparer<TInstLanguageVariant>)
  public
    function Compare(const Left, Right: TInstLanguageVariant): Integer; override;
  end;

  TInstLanguage = class
  strict private
    FVariants: TInstLanguageVariantList;
    FLocalName: string;
    FName: string;
    FCode: string;
  private
    FScript: string;
    FIsSuggested: Boolean;
  public
    constructor Create(AIsSuggested: Boolean; const ACode, AScript, AName: string);
    destructor Destroy; override;
    function Matches(const ASearchText: string): Boolean;
    property IsSuggested: Boolean read FIsSuggested;
    property Name: string read FName;
    property LocalName: string read FLocalName;
    property Script: string read FScript;
    property Code: string read FCode;
    property Variants: TInstLanguageVariantList read FVariants;
  end;

  TInstLanguageVariant = class
  strict private
    FName: string;
    FBCP47Tag: string;
    FLocalName: string;
    FCountryName: string;
    FLocalCountryName: string;
    FScript: string;
  public
    constructor Create(const ACode, ABCP47Tag, AScript, AName: string);
    function Matches(const ASearchText: string): Boolean;
    property Name: string read FName;
    property Code: string read FBCP47Tag;
    property Script: string read FScript;
    property CountryName: string read FCountryName;
    property LocalName: string read FLocalName;
    property LocalCountryName: string read FLocalCountryName;
  end;

  TfrmInstallKeyboardLanguage = class(TfrmKeymanBase)
    cmdCancel: TButton;
    cmdOK: TButton;
    lblLanguage: TLabel;
    editSearch: TEdit;
    gridLanguages: TStringGrid;
    gridLanguageVariants: TStringGrid;
    procedure cmdOKClick(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure gridLanguagesClick(Sender: TObject);
    procedure editSearchChange(Sender: TObject);
    procedure gridLanguagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private type
    TLanguageGridRowType = (lgrtNormal, lgrtSuggested, lgrtHeading);
    TCustomLanguage = record
      FullName, LanguageName, ScriptName, RegionName: string;
      Tag: string;
    end;
  private
    FKeyboard: IKeymanKeyboardInstalled;
    FLanguages: TInstLanguageList;
    FCustomLanguage: TCustomLanguage;
    procedure SetKeyboard(const Value: IKeymanKeyboardInstalled);
    procedure AddLocale(lpLocaleString: PWideChar);
    procedure EnableControls;
    procedure FillLanguageGrid;
    function GetGridRowType(ARow: Integer): TLanguageGridRowType;
  public
    property Keyboard: IKeymanKeyboardInstalled read FKeyboard write SetKeyboard;
  end;

function InstallKeyboardLanguage(Owner: TForm; const KeyboardID, ISOCode: string; Silent: Boolean): Boolean;

implementation

uses
  System.Types,
  Vcl.Themes,

  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LanguageCodeUtils,

  BCP47Tag,
  GetOSVersion,
  kmint,
  UtilWaitForTSF,
  utilkmshell;

{$R *.dfm}

{ TfrmInstallKeyboardLanguage }

function InstallKeyboardLanguage(Owner: TForm; const KeyboardID, ISOCode: string; Silent: Boolean): Boolean;
var
  n: Integer;
begin
  n := kmcom.Keyboards.IndexOf(KeyboardID);
  if n < 0 then
    Exit(False);

  kmcom.Keyboards[n].Languages.Install(ISOCode);

  Result := True;
end;


type
  PLOCALE_ENUMPROCEX = function(lpLocaleString: PWideChar; dwFlags: DWORD; lParam: LPARAM): LONG; stdcall;

function EnumSystemLocalesEx(pLocaleEnumProcEx: PLOCALE_ENUMPROCEX; dwFlags: DWORD; lParam: LPARAM; lpReserved: Pointer): BOOL; stdcall; external 'kernel32.dll';

const
  LOCALE_SUPPLEMENTAL = 2;
  LOCALE_WINDOWS = 1;

function SystemLocalesEnumProc(lpLocaleString: PWideChar; dwFlags: DWORD; lParam: LPARAM): LONG; stdcall;
var
  pForm: TfrmInstallKeyboardLanguage;
begin
  Result := 1;
  if lpLocaleString <> '' then
  begin
    pForm := TfrmInstallKeyboardLanguage(lParam);
    pForm.AddLocale(lpLocaleString);
  end;
end;

procedure TfrmInstallKeyboardLanguage.AddLocale(lpLocaleString: PWideChar);
var
  ScriptsBuf: array[0..MAX_PATH] of char;
  sLang: string;
  Found: Boolean;
  n: Integer;

    procedure AddVariant(Lang: TInstLanguage);
    var
      FLanguageVariant: TInstLanguageVariant;
      i: Integer;
    begin
      with TBCP47Tag.Create(lpLocaleString) do
      try
        Script := Lang.Script;
        Canonicalize;
        for i := 0 to Lang.Variants.Count - 1 do
          if SameText(Lang.Variants[i].Code, Tag) then
            Exit;
        FLanguageVariant := TInstLanguageVariant.Create(lpLocaleString, Tag, Lang.Script, '');
      finally
        Free;
      end;
      Lang.Variants.Add(FLanguageVariant);
    end;

    procedure AddLanguage;
    var
      FLanguage: TInstLanguage;
      Scripts: string;
    begin
      // Add a language entry for every script
      Scripts := ScriptsBuf;
      while Scripts <> '' do
      begin
        FLanguage := TInstLanguage.Create(False, sLang, Copy(Scripts, 1, 4), '');
        FLanguages.Add(FLanguage);

        if sLang <> lpLocaleString then
          AddVariant(FLanguage);
        Delete(Scripts, 1, 5);
      end;
    end;
begin
  // Extract the language name out of the tag
  with TBCP47Tag.Create(lpLocaleString) do
  try
    sLang := Language;
  finally
    Free;
  end;
  if sLang = '' then
    Exit;

  // Find all the scripts supported for the language
  if GetLocaleInfoEx(PWidechar(sLang), LOCALE_SSCRIPTS, ScriptsBuf, MAX_PATH) = 0 then
    RaiseLastOSError;

  // Find if the base languages are already there, and add a variant if so
  Found := False;
  for n := 0 to FLanguages.Count-1 do
    if (FLanguages[n].Code = sLang) then
    begin
      Found := True;
      if sLang <> lpLocaleString then
        AddVariant(FLanguages[n]);
    end;

  // If the base language has not yet been added, then add it + variants
  if not Found then
    AddLanguage;
end;

procedure TfrmInstallKeyboardLanguage.cmdOKClick(Sender: TObject);
var
  FLanguageVariant: TInstLanguageVariant;
  FCode: string;
  n: Integer;
  FKeyboardID: string;
begin
  FLanguageVariant := gridLanguageVariants.Objects[0, gridLanguageVariants.Row] as TInstLanguageVariant;
  if not Assigned(FLanguageVariant)
    then FCode := FCustomLanguage.Tag // Using a custom code
    else FCode := FLanguageVariant.Code;

  if not kmcom.SystemInfo.IsAdministrator then
  begin
    WaitForElevatedConfiguration(Handle, '-ikl "'+FKeyboard.ID+'" "'+FCode+'"');
  end
  else
    InstallKeyboardLanguage(Self, FKeyboard.ID, FCode, False);

  FKeyboardID := FKeyboard.ID;
  FKeyboard := nil;

  kmcom.Languages.Refresh;
  kmcom.Keyboards.Refresh;
  n := kmcom.Keyboards.IndexOf(FKeyboardID);
  if n >= 0
    then FKeyboard := kmcom.Keyboards[n]
    else FKeyboard := nil;

  if Assigned(FKeyboard) then
  begin
    // Because the TSF component is async we have to wait
    TWaitForTSF.WaitForLanguageProfilesToBeApplied(FKeyboard);
    FKeyboard := nil;
    kmcom.Keyboards.Refresh;  // Get updated language profile name after it is loaded
  end;
  ModalResult := mrOk;
end;

procedure TfrmInstallKeyboardLanguage.editSearchChange(Sender: TObject);
begin
  FillLanguageGrid;
end;

procedure TfrmInstallKeyboardLanguage.SetKeyboard(const Value: IKeymanKeyboardInstalled);
var
  FLanguage: TInstLanguage;
  FVariantComparer: TInstLanguageVariantComparer;
  FComparer: TInstLanguageComparer;
  i: Integer;
begin
  FKeyboard := Value;

  { Add the keyboard-defined additional languages first }
  for i := 0 to FKeyboard.Languages.Count - 1 do
    if not FKeyboard.Languages[i].IsInstalled then
    begin
      //TODO:BCP47: split BCP-47 into language + script + region here.
      with TBCP47Tag.Create(FKeyboard.Languages[i].BCP47Code) do
      try
        Canonicalize;
        if Tag <> '' then
        begin
          FLanguage := TInstLanguage.Create(True, Language, Script, FKeyboard.Languages[i].Name);
          try
            FLanguage.Variants.Add(TInstLanguageVariant.Create(Language, Tag, Script, FKeyboard.Languages[i].Name));
          except
            on E:EOSError do
            begin
              // The language tag is not supported on this OS - probably Win7
              // Don't offer it as an option
              FLanguage.Free;
              Continue;
            end;
          end;
          FLanguages.Add(FLanguage);
        end;
      finally
        Free;
      end;
    end;

  { Add keyboard default language options (that are not already installed) to the list }
  if not EnumSystemLocalesEx(SystemLocalesEnumProc, LOCALE_WINDOWS or LOCALE_SUPPLEMENTAL, LPARAM(Self), nil) then
    RaiseLastOSError;

  FComparer := TInstLanguageComparer.Create;
  try
    FLanguages.Sort(FComparer);
  finally
    FComparer.Free;
  end;

  FVariantComparer := TInstLanguageVariantComparer.Create;
  try
    for FLanguage in FLanguages do
    begin
      if (FLanguage.Variants.Count = 0) and not FLanguage.IsSuggested then
        FLanguage.Variants.Add(TInstLanguageVariant.Create(FLanguage.Code, FLanguage.Code, '', ''));
      FLanguage.Variants.Sort(FVariantComparer);
    end;
  finally
    FComparer.Free;
  end;

  FillLanguageGrid;
end;

procedure TfrmInstallKeyboardLanguage.FillLanguageGrid;
var
  n: Integer;
  FLanguage: TInstLanguage;
  FVariant: TInstLanguageVariant;
  FText: string;
  FFoundCustomTag: Boolean;

  procedure AddRow(IsSuggested: Boolean; const Name, LocalName, Script, Code: string; Item: TInstLanguage);
  begin
    if (gridLanguages.Objects[1,n-1] = Pointer(lgrtSuggested)) and not IsSuggested then
    begin
      gridLanguages.Cells[0,n] := 'System languages';
      gridLanguages.Objects[1,n] := Pointer(lgrtHeading); // break row, only 1 guaranteed
      Inc(n);
    end;

    if IsSuggested
      then gridLanguages.Objects[1,n] := Pointer(lgrtSuggested)
      else gridLanguages.Objects[1,n] := Pointer(lgrtNormal);

    gridLanguages.Cells[0,n] := Name;
    gridLanguages.Cells[1,n] := LocalName;
    gridLanguages.Cells[2,n] := Script;
    gridLanguages.Cells[3,n] := Code;
    gridLanguages.Objects[0,n] := Item;
    Inc(n);
  end;
begin
  // We need to add suggested languages for the keyboard...

  gridLanguages.RowCount := FLanguages.Count + 3;

  FText := Trim(editSearch.Text);

  n := 1;

  FFoundCustomTag := False;

  for FLanguage in FLanguages do
  begin
    if FLanguage.Matches(FText) then
    begin
      AddRow(FLanguage.IsSuggested, FLanguage.Name, FLanguage.LocalName, FLanguage.Script, FLanguage.Code, FLanguage);
      if SameText(FText, FLanguage.Code) then
        FFoundCustomTag := True;
      for FVariant in FLanguage.Variants do
        if SameText(FText, FVariant.Code) then
          FFoundCustomTag := True;
    end;
  end;

  if IsValidLocaleName(PChar(FText)) and not (GetOs = osWin7) and not FFoundCustomTag then
  begin
    // Adding custom locales supported with Win8 and later
    FText := TCanonicalLanguageCodeUtils.FindBestTag(FText);
    if FText <> '' then
      with TBCP47Tag.Create(FText) do
      try
        // Let's lookup the lang - script - region and get a good name
        FCustomLanguage.Tag := Tag;
        if not TLanguageCodeUtils.BCP47Languages.TryGetValue(Language, FCustomLanguage.LanguageName) then
          FCustomLanguage.LanguageName := '';
        if not TLanguageCodeUtils.BCP47Scripts.TryGetValue(Script, FCustomLanguage.ScriptName) then
          FCustomLanguage.ScriptName := '';
        if not TLanguageCodeUtils.BCP47Regions.TryGetValue(Region, FCustomLanguage.RegionName) then
          FCustomLanguage.RegionName := '';
        FCustomLanguage.FullName := TLanguageCodeUtils.LanguageName(FCustomLanguage.LanguageName,
          FCustomLanguage.ScriptName, '');
        AddRow(
          False,
          FCustomLanguage.FullName,
          FCustomLanguage.FullName,
          FCustomLanguage.ScriptName,
          FCustomLanguage.Tag, nil);
      finally
        Free;
      end;
  end;

  gridLanguages.RowCount := n;

  if gridLanguages.RowCount > 1 then
    gridLanguages.FixedRows := 1;

  gridLanguagesClick(gridLanguages);
end;

procedure TfrmInstallKeyboardLanguage.EnableControls;
begin
  gridLanguages.Enabled := gridLanguages.RowCount > 1;
  gridLanguageVariants.Enabled := (gridLanguageVariants.RowCount > 1) and (GetGridRowType(gridLanguages.Row) <> lgrtHeading);
  cmdOK.Enabled := (gridLanguageVariants.Row > 0) and (GetGridRowType(gridLanguages.Row) <> lgrtHeading);
end;

procedure TfrmInstallKeyboardLanguage.gridLanguagesClick(Sender: TObject);
var
  n: Integer;
  FLanguage: TInstLanguage;
  FVariant: TInstLanguageVariant;
begin
  if gridLanguages.Row = 0 then
  begin
    gridLanguageVariants.RowCount := 1;
  end
  else
  begin
    FLanguage := gridLanguages.Objects[0, gridLanguages.Row] as TInstLanguage;
    if FLanguage = nil then
    begin
      // We have a custom language tag
      gridLanguageVariants.RowCount := 2;
      gridLanguageVariants.Objects[0, 1] := nil;
      gridLanguageVariants.Cells[0, 1] := FCustomLanguage.FullName;
      gridLanguageVariants.Cells[1, 1] := FCustomLanguage.RegionName;
      gridLanguageVariants.Cells[2, 1] := '';
      gridLanguageVariants.Cells[3, 1] := '';
      gridLanguageVariants.Cells[4, 1] := FCustomLanguage.Tag;
    end
    else
    begin
      gridLanguageVariants.RowCount := FLanguage.Variants.Count + 1;
      n := 1;
      for FVariant in FLanguage.Variants do
      begin
        if FVariant.Matches(editSearch.Text) then
        begin
          gridLanguageVariants.Cells[0, n] := FVariant.Name;
          gridLanguageVariants.Cells[1, n] := FVariant.CountryName;
          gridLanguageVariants.Cells[2, n] := FVariant.LocalName;
          gridLanguageVariants.Cells[3, n] := FVariant.LocalCountryName;
          gridLanguageVariants.Cells[4, n] := FVariant.Code;
          gridLanguageVariants.Objects[0, n] := FVariant;
          Inc(n);
        end;
      end;
      gridLanguageVariants.RowCount := n;
    end;

    if gridLanguageVariants.RowCount > 1 then
      gridLanguageVariants.FixedRows := 1;
  end;
  EnableControls;
end;

function TfrmInstallKeyboardLanguage.GetGridRowType(ARow: Integer): TLanguageGridRowType;
begin
   Result := TLanguageGridRowType(Integer(gridLanguages.Objects[1, ARow]));
end;

procedure TfrmInstallKeyboardLanguage.gridLanguagesDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  CCellNormal: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicCellNormal, tgCellNormal, tgGradientCellNormal);
  CCellSelected: array[TGridDrawingStyle] of TThemedGrid =
    (tgClassicCellSelected, tgCellSelected, tgGradientCellSelected);
var
  LText: string;
  LDetails: TThemedElementDetails;
  LRect: TRect;
begin
  if GetGridRowType(ARow) = lgrtHeading then
  begin
    with gridLanguages.Canvas do
    begin
      LRect := Rect;
      Inc(LRect.Bottom);
      Inc(LRect.Right);
      Font := gridLanguages.Font;
      Font.Style := [fsBold];
      Brush.Color := clWindow;
      FillRect(Rect);
      if (gdSelected in State) or (gdRowSelected in State) then
        LDetails := StyleServices.GetElementDetails(CCellSelected[gridLanguages.DrawingStyle])
      else
        LDetails := StyleServices.GetElementDetails(CCellNormal[gridLanguages.DrawingStyle]);
      StyleServices.DrawElement(Handle, LDetails, LRect, Rect);
      LText := gridLanguages.Cells[ACol, ARow];
      Brush.Style := bsClear;
      TextRect(Rect, Rect.Left+2,
        Rect.Top+((Rect.Height - Canvas.TextHeight(LText)) div 2), LText);
    end;
  end
  else
  begin
    gridLanguages.Canvas.Font.Style := [];
  end;
end;

procedure TfrmInstallKeyboardLanguage.TntFormCreate(Sender: TObject);
begin
  inherited;
  FLanguages := TInstLanguageList.Create;

  gridLanguages.Cells[0,0] := 'Language Name';
  gridLanguages.Cells[1,0] := 'Local Name';
  gridLanguages.Cells[2,0] := 'Script';
  gridLanguages.Cells[3,0] := 'Code';
  gridLanguages.ColWidths[0] := 150;
  gridLanguages.ColWidths[1] := 150;
  gridLanguages.ColWidths[2] := 100;
  gridLanguages.ColWidths[3] := 90;

  gridLanguageVariants.Cells[0,0] := 'Language Name';
  gridLanguageVariants.Cells[1,0] := 'Country';
  gridLanguageVariants.Cells[2,0] := 'Local Name';
  gridLanguageVariants.Cells[3,0] := 'Local Country';
  gridLanguageVariants.Cells[4,0] := 'Code';
  gridLanguageVariants.ColWidths[0] := 120;
  gridLanguageVariants.ColWidths[1] := 100;
  gridLanguageVariants.ColWidths[2] := 120;
  gridLanguageVariants.ColWidths[3] := 100;
  gridLanguageVariants.ColWidths[4] := 64;


end;

procedure TfrmInstallKeyboardLanguage.TntFormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FLanguages);
end;

{ TInstLanguage }

const
  LOCALE_SNATIVELANGUAGENAME = $00000004;
  LOCALE_SLOCALIZEDLANGUAGENAME = $0000006f;
  LOCALE_SLOCALIZEDCOUNTRYNAME = $00000006;
  LOCALE_SNATIVECOUNTRYNAME = $0000008;

constructor TInstLanguage.Create(AIsSuggested: Boolean; const ACode, AScript, AName: string);
var
  LanguageBuf: array[0..MAX_PATH-1] of WideChar;
begin
  inherited Create;

  FIsSuggested := AIsSuggested;

  FVariants := TInstLanguageVariantList.Create;

  FCode := ACode;
  FScript := AScript;

  if FIsSuggested then
  begin
    FName := AName;
    FLocalName := AName;
  end
  else
  begin
    if GetLocaleInfoEx(PWidechar(ACode), LOCALE_SLOCALIZEDLANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FName := LanguageBuf;

    if GetLocaleInfoEx(PWidechar(ACode), LOCALE_SNATIVELANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;

    FLocalName := LanguageBuf;
  end;
end;

destructor TInstLanguage.Destroy;
begin
  FreeAndNil(FVariants);
  inherited Destroy;
end;

function TInstLanguage.Matches(const ASearchText: string): Boolean;
var
  FVariant: TInstLanguageVariant;
begin
  if ASearchText = '' then
    Exit(True);

  for FVariant in FVariants do
    if FVariant.Matches(ASearchText) then
      Exit(True);

  Result := False;
end;

{ TInstLanguageVariantComparer }

function TInstLanguageVariantComparer.Compare(const Left, Right: TInstLanguageVariant): Integer;
begin
  Result := CompareText(Left.Name, Right.Name);
end;

{ TInstLanguageVariant }

constructor TInstLanguageVariant.Create(const ACode, ABCP47Tag, AScript, AName: string);
var
  LanguageBuf: array[0..MAX_PATH-1] of WideChar;
begin
  inherited Create;

  FBCP47Tag := ABCP47Tag;
  FScript := AScript;

  if AName = '' then
  begin
    if GetLocaleInfoEx(PChar(ACode), LOCALE_SLOCALIZEDLANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FName := LanguageBuf;
    if GetLocaleInfoEx(PChar(ACode), LOCALE_SNATIVELANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FLocalName := LanguageBuf;
  end
  else
  begin
    FName := AName;
    FLocalName := AName;
  end;

  if GetLocaleInfoEx(PChar(ACode), LOCALE_SLOCALIZEDCOUNTRYNAME, LanguageBuf, MAX_PATH) = 0 then
    RaiseLastOSError;
  FCountryName := LanguageBuf;

  if GetLocaleInfoEx(PChar(ACode), LOCALE_SNATIVECOUNTRYNAME, LanguageBuf, MAX_PATH) = 0 then
    RaiseLastOSError;
  FLocalCountryName := LanguageBuf;
end;

function TInstLanguageVariant.Matches(const ASearchText: string): Boolean;
begin
  if ASearchText = '' then
    Exit(True);
  if SameText(ASearchText, Copy(FScript, 1, Length(ASearchText))) then
    Exit(True);
  if SameText(ASearchText, Copy(FName, 1, Length(ASearchText))) then
    Exit(True);
  if SameText(ASearchText, Copy(FBCP47Tag, 1, Length(ASearchText))) then
    Exit(True);
  if SameText(ASearchText, Copy(FLocalName, 1, Length(ASearchText))) then
    Exit(True);
  Result := False;
end;

{ TInstLanguageComparer }

function TInstLanguageComparer.Compare(const Left, Right: TInstLanguage): Integer;
begin
  if Left.IsSuggested and not Right.IsSuggested then
    Result := -1
  else if Right.IsSuggested and not Left.IsSuggested then
    Result := 1
  else
    Result := CompareText(Left.Name, Right.Name);
end;

end.
