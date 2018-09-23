unit Keyman.Developer.System.ImportWindowsKeyboard;

interface

uses
  Keyman.Developer.System.KeyboardProjectTemplate;

type
  TImportWindowsKeyboard = class
  private
    FBaseKeyboardID: string;
    FBaseName: string;

    { Options }
    FSourceKLID: string;
    FDestinationPath: string;
    FNameTemplate: string;
    FAuthor: string;
    FVersion: string;
    FKeyboardIDTemplate: string;
    FBCP47Tags: string;
    FCopyright: string;
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
    function FindBCP47TagForKLID: string;
  public
    function Execute: Boolean; overload;

    property SourceKLID: string read FSourceKLID write SetSourceKLID;
    property DestinationPath: string read FDestinationPath write SetDestinationPath;
    property KeyboardIDTemplate: string read FKeyboardIDTemplate write SetKeyboardIDTemplate;
    property NameTemplate: string read FNameTemplate write SetNameTemplate;
    property Copyright: string read FCopyright write SetCopyright;
    property Version: string read FVersion write SetVersion;
    property BCP47Tags: string read FBCP47Tags write SetBCP47Tags;
    property Author: string read FAuthor write SetAuthor;
  end;

implementation

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  System.Win.Registry,
  Vcl.Graphics,
  Winapi.Windows,

  BCP47Tag,
  Keyman.Developer.System.ImportKeyboardDLL,
  Keyman.Developer.System.TouchLayoutToVisualKeyboardConverter,
  Keyman.System.Util.RenderLanguageIcon,
  KeyboardParser,
  kmxfileconsts,
  RegistryKeys,
  UKeymanTargets,
  utilicon;

{ TImportWindowsKeyboard }

function Fail(m: string): Boolean;
begin
  Result := False;
  writeln(m);
end;

function TImportWindowsKeyboard.LoadKLIDDetails: Boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    if not r.OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM + '\' + FSourceKLID) then
      Exit(False);

    if not r.ValueExists(SRegValue_KeyboardLayoutFile) or not
        r.ValueExists(SRegValue_KeyboardLayoutText) then
      Exit(False);

    FBaseKeyboardID := ChangeFileExt(r.ReadString(SRegValue_KeyboardLayoutFile), '');
    FBaseName := r.ReadString(SRegValue_KeyboardLayoutText);
  finally
    r.Free;
  end;
  Result := True;
end;

procedure TImportWindowsKeyboard.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TImportWindowsKeyboard.SetCopyright(const Value: string);
begin
  FCopyright := Value;
end;

procedure TImportWindowsKeyboard.SetDestinationPath(const Value: string);
begin
  FDestinationPath := IncludeTrailingPathDelimiter(Value);
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

procedure TImportWindowsKeyboard.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

function TImportWindowsKeyboard.Execute: Boolean;
var
  FTemplate: TKeyboardProjectTemplate;
begin
  if not ForceDirectories(FDestinationPath) then
    Exit(Fail('The destination path '+FDestinationPath+' could not be created.'));

  // Lookup the KLID in the registry and read basic details
  if not LoadKLIDDetails then
    Exit(Fail('The keyboard identified by '+FSourceKLID+' could not be found.'));

  // Create a new folder in destination path

  FTemplate := TKeyboardProjectTemplate.Create(FDestinationPath, Format(FKeyboardIDTemplate, [FBaseKeyboardID]), KMXKeymanTargets + [ktWeb]);
  try
    //
    // These parameters apply to .kmn and .kps so set them even though
    // ImportKeyboard will overwrite the template .kmn
    //

    FTemplate.Name := Format(FNameTemplate, [FBaseName]);
    FTemplate.Copyright := FCopyright;
    FTemplate.Author := FAuthor;
    FTemplate.Version := FVersion;

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

    // Run importkeyboard into destination file; this replaces the keyboard template
    // file that has been generated
    if not ImportKeyboard(FTemplate.KeyboardFilename, FTemplate.OSKFilename) then
      Exit(Fail('Unable to run importkeyboard on '+FSourceKLID));

    // Replace .ico with a new one based on the language id
    // TODO: this goes in the keyboard project template generation I think
    if not GenerateIcon(FTemplate.IconFilename) then
      Exit(Fail('Unable to generate an icon for '+FTemplate.KeyboardFilename));

    // Load the source .kmn and add bitmap, copyright, visualkeyboard, touch layout fields
    InjectSystemStores(FTemplate.KeyboardFilename, FTemplate.OSKFilename, FTemplate.IconFilename, FTemplate.TouchLayoutFilename);

    // Take the generated OSK and convert it into a default touch layout
    if not ConvertOSKToTouchLayout(FTemplate.OSKFilename, FTemplate.TouchLayoutFilename) then
      Exit(Fail('Unable to create a default touch layout based on the OSK for '+FTemplate.KeyboardFilename));

  finally
    FreeAndNil(FTemplate);
  end;

  Result := True;
end;

function TImportWindowsKeyboard.FindBCP47TagForKLID: string;
var
  buf: array[0..8] of char;
  FLanguageID: Word;
begin
  FLanguageID := LOWORD(StrToInt('$'+FSourceKLID));
  if GetLocaleInfo(FLanguageID, LOCALE_SISO639LANGNAME, buf, 8) > 0 then
    FBCP47Tags := buf;
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
    kp.Features.Add(kfIcon);
    kp.Features.Add(kfOSK);
    kp.Features.Add(kfTouchLayout);
    // TODO: Are these file settings actually doing anything? Or is it controlled
    // entirely by kp.Features.Add -- which could cause this to fall over a little
    // if we change filenames for any reason in the future
    kp.SetSystemStoreValue(ssName, Format(FNameTemplate, [FBaseName]));
    kp.SetSystemStoreValue(ssVisualKeyboard, ExtractFileName(OSKFilename));
    kp.SetSystemStoreValue(ssBitmap, ExtractFilename(IconFilename));
    kp.SetSystemStoreValue(ssLayoutFile, ExtractFileName(TouchLayoutFilename));
    kp.SetSystemStoreValue(ssCopyright, FCopyright);
    if FVersion <> '' then
      kp.SetSystemStoreValue(ssVersion, FVersion);
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
var
  FTag: string;
  n: Integer;
  b: array[0..0] of Vcl.Graphics.TBitmap;
begin
  // We need to use the BCP47 tag that we have received and render that, for now

  n := Min(Pos(' ', FBCP47Tags), Pos('-', FBCP47Tags));
  if n > 0
    then FTag := Copy(FBCP47Tags, 1, n-1)
    else FTag := FBCP47Tags;

  b[0] := Vcl.Graphics.TBitmap.Create;
  try
    b[0].SetSize(16, 16);
    b[0].PixelFormat := pf32bit;
    // TODO multiple sizes?, colour?
    DrawLanguageIcon(b[0].Canvas, 0, 0, UpperCase(FTag));
    Result := ConvertBitmapsToAlphaIcon(b, IconFilename);
  finally
    b[0].Free;
  end;
end;

function TImportWindowsKeyboard.ConvertOSKToTouchLayout(const OSKFilename, TouchLayoutFilename: string): Boolean;
var
  converter: TTouchLayoutToVisualKeyboardConverter;
  FNewLayout: string;
  ss: TStringStream;
begin
  converter := TTouchLayoutToVisualKeyboardConverter.Create(OSKFilename);
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
