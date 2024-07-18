unit Keyman.Developer.System.ImportWindowsKeyboard;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.KeyboardProjectTemplate,
  UKeymanTargets;

type
  EImportWindowsKeyboard = class(Exception);

  TImportWindowsKeyboard = class
  private
    FBaseKeyboardID: string;
    FBaseName: string;
    FProjectFilename: string;

    { Options }
    FSourceKLID: string;
    FDestinationPath: string;
    FNameTemplate: string;
    FAuthor: string;
    FVersion: string;
    FKeyboardIDTemplate: string;
    FBCP47Tags: string;
    FCopyright: string;
    FTargets: TKeymanTargets;
    FFullCopyright: string;
    FDescription: string;
    function LoadKLIDDetails: Boolean;
    function ImportKeyboard(const DestinationFilename, DestinationKVKSFilename: string): Boolean;
    function GenerateIcon(const IconFilename: string): Boolean;
    procedure SetAuthor(const Value: string);
    procedure SetCopyright(const Value: string);
    procedure SetDestinationPath(const Value: string);
    procedure SetKeyboardIDTemplate(const Value: string);
    procedure SetBCP47Tags(const Value: string);
    procedure SetNameTemplate(const Value: string);
    procedure SetSourceKLID(const Value: string);
    procedure SetVersion(const Value: string);
    procedure InjectSystemStores(const KeyboardFilename, OSKFilename,
      IconFilename, TouchLayoutFilename: string);
    function ConvertOSKToTouchLayout(const OSKFilename, TouchLayoutFilename: string): Boolean;
    function FindBCP47TagForKLID: string; overload;
    function GetProjectFilename: string;
    procedure SetTargets(const Value: TKeymanTargets);
    procedure SetFullCopyright(const Value: string);
    procedure SetDescription(const Value: string);
 public
    function Execute: Boolean; overload;

    class function FindBCP47TagForKLID(KLID: string): string; overload;
    class function GetKLIDDetails(const KLID: string; var Name,
      KeyboardID: string): Boolean; static;

    property SourceKLID: string read FSourceKLID write SetSourceKLID;
    property DestinationPath: string read FDestinationPath write SetDestinationPath;
    property KeyboardIDTemplate: string read FKeyboardIDTemplate write SetKeyboardIDTemplate;
    property NameTemplate: string read FNameTemplate write SetNameTemplate;
    property Copyright: string read FCopyright write SetCopyright;
    property FullCopyright: string read FFullCopyright write SetFullCopyright;
    property Version: string read FVersion write SetVersion;
    property BCP47Tags: string read FBCP47Tags write SetBCP47Tags;
    property Author: string read FAuthor write SetAuthor;
    property Description: string read FDescription write SetDescription;
    property Targets: TKeymanTargets read FTargets write SetTargets;

    property ProjectFilename: string read GetProjectFilename;
  end;

implementation

uses
  System.Classes,
  System.Win.Registry,
  Winapi.Windows,

  BCP47Tag,
  Keyman.Developer.System.ImportKeyboardDLL,
  Keyman.Developer.System.GenerateKeyboardIcon,
  Keyman.Developer.System.VisualKeyboardToTouchLayoutConverter,
  Keyman.System.KeyboardUtils,
  KeymanVersion,
  KeyboardParser,
  kmxfileconsts,
  RegistryKeys,
  TempFileManager,
  utilfiletypes;

{ TImportWindowsKeyboard }

function Fail(m: string): Boolean;
begin
  Result := False;
  writeln(m);
end;

function TImportWindowsKeyboard.LoadKLIDDetails: Boolean;
begin
  Result := GetKLIDDetails(FSourceKLID, FBaseName, FBaseKeyboardID);
end;

class function TImportWindowsKeyboard.GetKLIDDetails(const KLID: string; var Name, KeyboardID: string): Boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    if not r.OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM + '\' + KLID) then
      Exit(False);

    if not r.ValueExists(SRegValue_KeyboardLayoutFile) or not
        r.ValueExists(SRegValue_KeyboardLayoutText) then
      Exit(False);

    KeyboardID := TKeyboardUtils.CleanKeyboardID(ChangeFileExt(r.ReadString(SRegValue_KeyboardLayoutFile), ''));
    Name := r.ReadString(SRegValue_KeyboardLayoutText);
  finally
    r.Free;
  end;
  Result := True;
end;

function TImportWindowsKeyboard.GetProjectFilename: string;
var
  FTemplate: TKeyboardProjectTemplate;
begin
  if FProjectFilename = '' then
  begin
    // Lookup the KLID in the registry and read basic details
    if not LoadKLIDDetails then
      raise EImportWindowsKeyboard.Create('The keyboard identified by '+FSourceKLID+' could not be found.');

    FTemplate := TKeyboardProjectTemplate.Create(FDestinationPath, Format(FKeyboardIDTemplate, [FBaseKeyboardID]), KMXKeymanTargets + [ktWeb]);
    try
      Result := FTemplate.ProjectFilename;
    finally
      FTemplate.Free;
    end;
  end
  else
    Result := FProjectFIlename;
end;

procedure TImportWindowsKeyboard.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TImportWindowsKeyboard.SetCopyright(const Value: string);
begin
  FCopyright := Value;
end;

procedure TImportWindowsKeyboard.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TImportWindowsKeyboard.SetDestinationPath(const Value: string);
begin
  FDestinationPath := IncludeTrailingPathDelimiter(Value);
end;

procedure TImportWindowsKeyboard.SetFullCopyright(const Value: string);
begin
  FFullCopyright := Value;
end;

procedure TImportWindowsKeyboard.SetKeyboardIDTemplate(const Value: string);
begin
  if Value = ''
    then FKeyboardIDTemplate := '%s'
    else FKeyboardIDTemplate := Value;
end;

procedure TImportWindowsKeyboard.SetBCP47Tags(const Value: string);
begin
  FBCP47Tags := Value;
end;

procedure TImportWindowsKeyboard.SetNameTemplate(const Value: string);
begin
  if Value = ''
    then FNameTemplate := '%s'
    else FNameTemplate := Value;
end;

procedure TImportWindowsKeyboard.SetSourceKLID(const Value: string);
begin
  FSourceKLID := Value;
end;

procedure TImportWindowsKeyboard.SetTargets(const Value: TKeymanTargets);
begin
  FTargets := Value;
end;

procedure TImportWindowsKeyboard.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

function TImportWindowsKeyboard.Execute: Boolean;
var
  FTemplate: TKeyboardProjectTemplate;
  DestinationOSKTempFile: TTempFile;
  DestinationOSKFilename: string;
begin
  if not ForceDirectories(FDestinationPath) then
    Exit(Fail('The destination path '+FDestinationPath+' could not be created.'));

  // Lookup the KLID in the registry and read basic details
  if not LoadKLIDDetails then
    Exit(Fail('The keyboard identified by '+FSourceKLID+' could not be found.'));

  // Create a new folder in destination path

  DestinationOSKTempFile := nil;
  FTemplate := TKeyboardProjectTemplate.Create(FDestinationPath, Format(FKeyboardIDTemplate, [FBaseKeyboardID]), FTargets);
  try
    //
    // These parameters apply to .kmn and .kps so set them even though
    // ImportKeyboard will overwrite the template .kmn
    //

    FTemplate.Name := Format(FNameTemplate, [FBaseName]);
    FTemplate.Copyright := FCopyright;
    FTemplate.FullCopyright := FFullCopyright;
    FTemplate.Author := FAuthor;
    FTemplate.Description := FDescription;
    FTemplate.Version := FVersion;
    FTemplate.IncludeIcon := True;

    //
    // Set languages in package
    //

    if FBCP47Tags = '' then
      FBCP47Tags := FindBCP47TagForKLID;
    FTemplate.BCP47Tags := FBCP47Tags;

    //
    // Creates a .kmn, .kvk, .ico, .kps and any other generic files
    //
    try
      FTemplate.Generate;
    except
      on E:EKeyboardProjectTemplate do
        Exit(Fail('Unable to generate template: '+E.Message));
    end;

    FProjectFilename := FTemplate.ProjectFilename;

    if FTemplate.OSKFilename = '' then
    begin
      // We need an OSK file in order to generate the touch layout. OSKFilename
      // will be empty if the user selects a touch-only target. So we'll use a
      // temp file in this scenario.
      DestinationOSKTempFile := TTempFileManager.Get(Ext_VisualKeyboardSource);
      DestinationOSKFilename := DestinationOSKTempFile.Name;
    end
    else
      DestinationOSKFilename := FTemplate.OSKFilename;


    // Run importkeyboard into destination file; this replaces the keyboard template
    // file that has been generated
    if not ImportKeyboard(FTemplate.KeyboardFilename, DestinationOSKFilename) then
      Exit(Fail('Unable to run importkeyboard on '+FSourceKLID));

    // Replace .ico with a new one based on the language id
    // TODO: this goes in the keyboard project template generation I think
    if FTemplate.IconFilename <> '' then
      if not GenerateIcon(FTemplate.IconFilename) then
        Exit(Fail('Unable to generate an icon for '+FTemplate.KeyboardFilename));

    // Load the source .kmn and add bitmap, copyright, visualkeyboard, touch layout fields
    // note, we use FTemplate.OSKFilename here deliberately, because we only
    // generate OSK-related fields if they are in use
    InjectSystemStores(FTemplate.KeyboardFilename, FTemplate.OSKFilename, FTemplate.IconFilename, FTemplate.TouchLayoutFilename);

    if FTemplate.TouchLayoutFilename <> '' then
    begin
      // Take the generated OSK and convert it into a default touch layout
      if not ConvertOSKToTouchLayout(DestinationOSKFilename, FTemplate.TouchLayoutFilename) then
        Exit(Fail('Unable to create a default touch layout based on the OSK for '+FTemplate.KeyboardFilename));
    end;
  finally
    FreeAndNil(FTemplate);
    FreeAndNil(DestinationOSKTempFile);
  end;

  Result := True;
end;

class function TImportWindowsKeyboard.FindBCP47TagForKLID(KLID: string): string;
var
  buf: array[0..8] of char;
  FLanguageID: Word;
begin
  FLanguageID := LOWORD(StrToInt('$'+KLID));
  if GetLocaleInfo(FLanguageID, LOCALE_SISO639LANGNAME, buf, 8) > 0
    then Result := buf
    else Result := '';
end;

function TImportWindowsKeyboard.FindBCP47TagForKLID: string;
begin
  Result := FindBCP47TagForKLID(FSourceKLID);
end;

procedure TImportWindowsKeyboard.InjectSystemStores(const KeyboardFilename, OSKFilename, IconFilename, TouchLayoutFilename: string);
var
  kp: TKeyboardParser;
  sl: TStringList;
begin
  kp := TKeyboardParser.Create;
  try
    kp.FileName := KeyboardFilename;
    kp.LoadFromFile(KeyboardFilename);
    if IconFilename <> '' then
      kp.Features.Add(kfIcon);
    if OSKFilename <> '' then
      kp.Features.Add(kfOSK);
    if (TouchKeymanTargets+[ktAny]) * FTargets <> [] then
      kp.Features.Add(kfTouchLayout);
    // TODO: Are these file settings actually doing anything? Or is it controlled
    // entirely by kp.Features.Add -- which could cause this to fall over a little
    // if we change filenames for any reason in the future
    kp.SetSystemStoreValue(ssName, Format(FNameTemplate, [FBaseName]));
    kp.SetSystemStoreValue(ssVersion, SKeymanKeyboardVersion);
    if OSKFilename <> '' then
      kp.SetSystemStoreValue(ssVisualKeyboard, ExtractFileName(OSKFilename));
    if IconFilename <> '' then
      kp.SetSystemStoreValue(ssBitmap, ExtractFilename(IconFilename));
    kp.SetSystemStoreValue(ssLayoutFile, ExtractFileName(TouchLayoutFilename));
    kp.SetSystemStoreValue(ssTargets, KeymanTargetsToString(FTargets));
    kp.SetSystemStoreValue(ssCopyright, FCopyright);
    if FVersion <> '' then
      kp.SetSystemStoreValue(ssKeyboardVersion, FVersion);
    if FAuthor <> '' then
      kp.InitialComment := kp.InitialComment + 'Run by: ' + FAuthor + #13#10;

    sl := TStringList.Create;
    try
      sl.Text := kp.KeyboardText;
      sl.SaveToFile(KeyboardFilename, TEncoding.UTF8);
    finally
      sl.Free;
    end;
  finally
    kp.Free;
  end;
end;

function TImportWindowsKeyboard.ImportKeyboard(const DestinationFilename, DestinationKVKSFilename: string): Boolean;
var
  ik: TImportKeyboardDLL;
  sl: TStringList;
  ss: TStringStream;
begin
  ik := TImportKeyboardDLL.Create(FSourceKLID);
  try
    try
      ik.Execute;
    except
      on E:EImportKeyboardDLL do
      begin
        writeln(E.Message);
        Exit(False);
      end;
    end;

    // Still need a BOM for .kmn ... for now
    sl := TStringList.Create;
    try
      sl.Text := ik.KMN;
      sl.SaveToFile(DestinationFilename, TEncoding.UTF8);
    finally
      sl.Free;
    end;

    // No BOM for .kvks
    ss := TStringStream.Create(ik.KVKS, TEncoding.UTF8);
    try
      ss.SaveToFile(DestinationKVKSFilename);
    finally
      ss.Free;
    end;

  finally
    ik.Free;
  end;

  Result := True;
end;

function TImportWindowsKeyboard.GenerateIcon(
  const IconFilename: string): Boolean;
begin
  Result := TKeyboardIconGenerator.GenerateIcon(FBCP47Tags, IconFilename, 0);
end;

function TImportWindowsKeyboard.ConvertOSKToTouchLayout(const OSKFilename, TouchLayoutFilename: string): Boolean;
var
  converter: TVisualKeyboardToTouchLayoutConverter;
  FNewLayout: string;
  ss: TStringStream;
begin
  converter := TVisualKeyboardToTouchLayoutConverter.Create(OSKFilename);
  try
    if converter.Execute(FNewLayout) then
    begin
      ss := TStringStream.Create(FNewLayout, TEncoding.UTF8);
      try
        ss.SaveToFile(TouchLayoutFilename);
      finally
        ss.Free;
      end;
    end
    else
      Exit(False);
  finally
    converter.Free;
  end;

  Result := True;
end;

end.
