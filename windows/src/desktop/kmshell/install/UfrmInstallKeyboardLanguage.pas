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
                    01 Sep 2014 - mcdurdin - I4394 - V9.0 - Keyman  Free Edition polish
                    01 Sep 2014 - mcdurdin - I4395 - V9.0 - Restrict associated languages to 1 for Light Edition

*)
unit UfrmInstallKeyboardLanguage;   // I4322

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmKeymanBase,
  keymanapi_TLB,
  Vcl.StdCtrls, System.Generics.Collections, System.Generics.Defaults, Vcl.Grids;

type
  TInstLanguage = class;
  TInstLanguageVariant = class;

  TInstLanguageList = class(TObjectList<TInstLanguage>)
  public
    function Find(const Language, Script: string): Integer;
  end;

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
    FBCP47Tag: string;
  private
    FScript: string;
    FIsSuggested: Boolean;
  public
    constructor Create(AIsSuggested: Boolean; const ALookupCode, ALanguage, AScript, AName: string);
    destructor Destroy; override;
    function Matches(const ASearchText: string): Boolean;
    property IsSuggested: Boolean read FIsSuggested;
    property Name: string read FName;
    property LocalName: string read FLocalName;
    property Script: string read FScript;
    property Code: string read FBCP47Tag;
    property Variants: TInstLanguageVariantList read FVariants;
  end;

  TInstLanguageVariant = class
  strict private
    FName: string;
    FBCP47Tag: string;
    FLocalName: string;
    FCountryName: string;
    FScript: string;
  public
    constructor Create(const ABCP47Tag, AName: string);
    function Matches(const ASearchText: string): Boolean;
    property Name: string read FName;
    property BCP47Tag: string read FBCP47Tag;
    property Script: string read FScript;
    property CountryName: string read FCountryName;
    property LocalName: string read FLocalName;
  end;

  TfrmInstallKeyboardLanguage = class(TfrmKeymanBase)
    cmdCancel: TButton;
    cmdOK: TButton;
    lblLanguage: TLabel;
    editSearch: TEdit;
    gridLanguages: TStringGrid;
    gridLanguageVariants: TStringGrid;
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure gridLanguagesClick(Sender: TObject);
    procedure editSearchChange(Sender: TObject);
    procedure gridLanguagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure editSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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

  Keyman.Configuration.UI.MitigationForWin10_1803,
  Keyman.System.LanguageCodeUtils,
  Keyman.Configuration.System.TIPMaintenance,
  Keyman.UI.UfrmProgress,
  MessageIdentifierConsts,
  MessageIdentifiers,

  BCP47Tag,
  GetOSVersion,
  kmint,
  utilkmshell;

{$R *.dfm}

{ TfrmInstallKeyboardLanguage }

function InstallKeyboardLanguage(Owner: TForm; const KeyboardID, ISOCode: string; Silent: Boolean): Boolean;
begin
  Result := TTIPMaintenance.DoInstall(KeyboardID, ISOCode);
  if not Result then
  begin
    if not Silent then
      ShowMessage(MsgFromIdFormat(SKInstallLanguageTransientLimit, [ISOCode]));
    Exit;
  end
  else
    CheckForMitigationWarningFor_Win10_1803(Silent, '');

  kmcom.Apply;
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

    procedure AddVariant(Tag: string; Lang: TInstLanguage);
    var
      Variant: TInstLanguageVariant;
    begin
      for Variant in Lang.Variants do
        if SameText(Variant.BCP47Tag, Tag) then
          Exit;
      Lang.Variants.Add(TInstLanguageVariant.Create(Tag, ''));
    end;

var
  Language, Script: string;
  Tag: string;
  BCP47Tag: TBCP47Tag;
  n: Integer;
begin
  Tag := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(lpLocaleString);

  // Extract the language and script out of the tag
  BCP47Tag := TBCP47Tag.Create(Tag);
  try
    Language := BCP47Tag.Language;
    Script := BCP47Tag.Script;
  finally
    BCP47Tag.Free;
  end;

  if Language = '' then
    Exit;

  // Find if the base languages are already there, and add a variant if so
  n := FLanguages.Find(Language, Script);
  if n < 0 then
    n := FLanguages.Add(TInstLanguage.Create(False, lpLocaleString, Language, Script, ''));
  AddVariant(lpLocaleString, FLanguages[n]);
end;

procedure TfrmInstallKeyboardLanguage.cmdOKClick(Sender: TObject);
begin
  if TfrmProgress.Execute(Self,
    function(Manager: IProgressManager): Boolean
    var
      FLanguageVariant: TInstLanguageVariant;
      FCode: string;
      n: Integer;
      FKeyboardID: string;
    begin
      Manager.Title := 'Installing Language';
      Manager.CanCancel := False;
      Manager.UpdateProgress('Installing Language', 0, 0);
      FLanguageVariant := gridLanguageVariants.Objects[0, gridLanguageVariants.Row] as TInstLanguageVariant;
      if not Assigned(FLanguageVariant)
        then FCode := FCustomLanguage.Tag // Using a custom code
        else FCode := FLanguageVariant.BCP47Tag;

      if not TTIPMaintenance.DoInstall(FKeyboard.ID, FCode) then
      begin
        ShowMessage(MsgFromIdFormat(SKInstallLanguageTransientLimit, [FCode]));
        Exit(False);
      end;

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
        FKeyboard := nil;
        kmcom.Keyboards.Refresh;  // Get updated language profile name after it is loaded
      end;
      Result := True;
    end
  ) then
  begin
    kmcom.Apply;
    ModalResult := mrOk;
  end;
end;

procedure TfrmInstallKeyboardLanguage.editSearchChange(Sender: TObject);
begin
  FillLanguageGrid;
end;

procedure TfrmInstallKeyboardLanguage.editSearchKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  procedure MoveRow(n: Integer);
  begin
    n := gridLanguages.Row + n;
    if n < 1 then
      n := 1;
    if n >= gridLanguages.RowCount then
      n := gridLanguages.RowCount - 1;
    if n <> gridLanguages.Row then
    begin
      gridLanguages.Row := n;
      gridLanguagesClick(gridLanguages);
    end;
    Key := 0;
  end;
begin
  case Key of
    VK_DOWN: MoveRow(1);
    VK_UP: MoveRow(-1);
    VK_NEXT: MoveRow(gridLanguages.VisibleRowCount - 1);
    VK_PRIOR: MoveRow(-gridLanguages.VisibleRowCount - 1);
    VK_HOME: MoveRow(-gridLanguages.Row+1);
    VK_END: MoveRow(gridLanguages.RowCount - 1 - gridLanguages.Row);
  end;
end;

procedure TfrmInstallKeyboardLanguage.SetKeyboard(const Value: IKeymanKeyboardInstalled);
var
  FLanguage: TInstLanguage;
  FVariantComparer: TInstLanguageVariantComparer;
  FComparer: TInstLanguageComparer;
  n, i: Integer;
  BCP47Tag: TBCP47Tag;
  s: string;
begin
  FKeyboard := Value;

  { Add the keyboard-defined additional languages first }
  for i := 0 to FKeyboard.Languages.Count - 1 do
    if not FKeyboard.Languages[i].IsInstalled then
    begin
      BCP47Tag := TBCP47Tag.Create(FKeyboard.Languages[i].BCP47Code);
      try
        BCP47Tag.Tag := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(BCP47Tag.Tag);
        if BCP47Tag.Tag <> '' then
        begin
          s := BCP47Tag.Script;
          if (s = '') then
            if not TLanguageCodeUtils.SuppressScripts.TryGetValue(BCP47Tag.Language, s) then
              s := 'Unknown script';

          n := FLanguages.Find(BCP47Tag.Language, BCP47Tag.Script);
          if n >= 0 then
            FLanguage := FLanguages[n]
          else
          begin
            FLanguage := TInstLanguage.Create(True, BCP47Tag.Tag, BCP47Tag.Language, s, FKeyboard.Languages[i].Name);
            FLanguages.Add(FLanguage);
          end;

          try
            FLanguage.Variants.Add(TInstLanguageVariant.Create(BCP47Tag.Tag, FKeyboard.Languages[i].Name));
          except
            on E:EOSError do
            begin
              // The language tag is not supported on this OS - probably Win7
              // Don't offer it as an option
              FLanguages.Remove(FLanguage);
              Continue;
            end;
          end;
        end;
      finally
        BCP47Tag.Free;
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
        FLanguage.Variants.Add(TInstLanguageVariant.Create(FLanguage.Code, ''));
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
  FAllowableText: string;

  procedure AddRow(IsSuggested: Boolean; const Name, LocalName, Script, Code: string; Item: TInstLanguage);
  begin
    if (gridLanguages.Objects[1,n-1] = Pointer(lgrtSuggested)) and not IsSuggested then
    begin
      gridLanguages.Cells[0,n] := 'System languages';
      gridLanguages.Cells[1,n] := '';
      gridLanguages.Cells[2,n] := '';
      gridLanguages.Cells[3,n] := '';
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
        if SameText(FText, FVariant.BCP47Tag) then
          FFoundCustomTag := True;
    end;
  end;

  if not (GetOs = osWin7) and not FFoundCustomTag then
  begin
    if IsValidLocaleName(PChar(FText)) then
    begin
      // Adding custom locales supported with Win8 and later
      FAllowableText := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(FText);
      if FAllowableText <> '' then
      begin
        with TBCP47Tag.Create(FAllowableText) do
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
      gridLanguageVariants.Cells[3, 1] := FCustomLanguage.Tag;
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
          gridLanguageVariants.Cells[3, n] := FVariant.BCP47Tag;
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
  gridLanguageVariants.Cells[3,0] := 'Code';
  gridLanguageVariants.ColWidths[0] := 150;
  gridLanguageVariants.ColWidths[1] := 134;
  gridLanguageVariants.ColWidths[2] := 120;
  gridLanguageVariants.ColWidths[3] := 100;

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

constructor TInstLanguage.Create(AIsSuggested: Boolean; const ALookupCode, ALanguage, AScript, AName: string);
var
  LanguageBuf: array[0..MAX_PATH-1] of WideChar;
  v: string;
begin
  inherited Create;

  Assert(Pos('-', ALanguage) = 0);
  Assert(Pos('-', AScript) = 0);

  FIsSuggested := AIsSuggested;

  FVariants := TInstLanguageVariantList.Create;

  FBCP47Tag := ALanguage;
  FScript := AScript;
  if (FScript <> '') and
      (not TLanguageCodeUtils.SuppressScripts.TryGetValue(ALanguage, v) or not
      SameText(v, FScript)) then
    FBCP47Tag := FBCP47Tag + '-'+FScript;

  if FIsSuggested then
  begin
    FName := AName;
    FLocalName := AName;
  end
  else
  begin
    // We need to lookup the code with the script suppressed

    if GetLocaleInfoEx(PWidechar(ALookupCode), LOCALE_SLOCALIZEDLANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FName := LanguageBuf;

    if GetLocaleInfoEx(PWidechar(ALookupCode), LOCALE_SNATIVELANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
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

constructor TInstLanguageVariant.Create(const ABCP47Tag, AName: string);
var
  LanguageBuf: array[0..MAX_PATH-1] of WideChar;
  b: TBCP47Tag;
begin
  inherited Create;

  FBCP47Tag := ABCP47Tag;

  b := TBCP47Tag.Create(ABCP47Tag);
  try
    FScript := b.Script;
    if not TLanguageCodeUtils.BCP47Regions.TryGetValue(b.Region, FCountryName) then
    begin
      if GetLocaleInfoEx(PChar(ABCP47Tag), LOCALE_SLOCALIZEDCOUNTRYNAME, LanguageBuf, MAX_PATH) = 0 then
        RaiseLastOSError;
      FCountryName := LanguageBuf;
    end;
  finally
    b.Free;
  end;

  if AName = '' then
  begin
    if GetLocaleInfoEx(PChar(ABCP47Tag), LOCALE_SLOCALIZEDLANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FName := LanguageBuf;
    if GetLocaleInfoEx(PChar(ABCP47Tag), LOCALE_SNATIVELANGUAGENAME, LanguageBuf, MAX_PATH) = 0 then
      RaiseLastOSError;
    FLocalName := LanguageBuf;
  end
  else
  begin
    FName := AName;
    FLocalName := AName;
  end;
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

{ TInstLanguageList }

function TInstLanguageList.Find(const Language, Script: string): Integer;
var
  FTag, s: string;
begin
  FTag := Language;
  if (Script <> '') then
  begin
    if not TLanguageCodeUtils.SuppressScripts.TryGetValue(Language.ToLower, s) or
        not SameText(s, script) then
      FTag := FTag + '-' + Script.ToLower;
  end;

  for Result := 0 to Count-1 do
    if SameText(Items[Result].Code, FTag) then Exit;
  Result := -1;
end;

end.
