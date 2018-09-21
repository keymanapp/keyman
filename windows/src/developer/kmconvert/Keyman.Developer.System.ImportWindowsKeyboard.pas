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
    function ImportKeyboard(const DestinationFilename, DestinationKVKSFilename, Name: string): Boolean;
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
      IconFilename: string);
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
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  Keyman.Developer.System.ImportKeyboardDLL,
  KeyboardParser,
  kmxfileconsts,
  RegistryKeys,
  UKeymanTargets;

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
    FTemplate.Name := Format(FNameTemplate, [FBaseName]);
    FTemplate.Copyright := FCopyright;
    FTemplate.Author := FAuthor;
    FTemplate.Version := FVersion;
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
    if not ImportKeyboard(FTemplate.KeyboardFilename, FTemplate.OSKFilename, FTemplate.Name) then
      Exit(Fail('Unable to run importkeyboard on '+FSourceKLID));

    // Replace .ico with a new one based on the language id
    // TODO: this goes in the keyboard project template generation I think
    if not GenerateIcon(FTemplate.IconFilename) then
      Exit(Fail('Unable to generate an icon for '+FTemplate.KeyboardFilename));

    // Load the source .kmn and add bitmap, copyright, visualkeyboard fields
    InjectSystemStores(FTemplate.KeyboardFilename, FTemplate.OSKFilename, FTemplate.IconFilename);

    // Create .kpj
    //FTemplate.SaveProjectFile; <-- already done
  finally
    FreeAndNil(FTemplate);
  end;

  Result := True;
end;

procedure TImportWindowsKeyboard.InjectSystemStores(const KeyboardFilename, OSKFilename, IconFilename: string);
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
    kp.SetSystemStoreValue(ssVisualKeyboard, ExtractFileName(OSKFilename));
    kp.SetSystemStoreValue(ssBitmap, ExtractFilename(IconFilename));
    kp.SetSystemStoreValue(ssCopyright, FCopyright);
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

function TImportWindowsKeyboard.ImportKeyboard(const DestinationFilename, DestinationKVKSFilename, Name: string): Boolean;
var
  ik: TImportKeyboardDLL;
  sl: TStringList;
  ss: TStringStream;
begin
  ik := TImportKeyboardDLL.Create(FSourceKLID, Name);
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
  //TODO
  Result := True;
end;


end.
