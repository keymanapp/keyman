unit Keyman.Test.Console.KeymanAPITest_BCP47;

interface
uses
  DUnitX.TestFramework,
  Keyman.Test.Console.KeymanAPITest_Base,
  keymanapi_tlb;

type

  [TestFixture]
  TKeymanAPITest_BCP47 = class(TKeymanAPITest_Base)
  private
    k: IKeyman;
  public
    constructor Create;

    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFile;

    [Test]
    procedure TestInstall;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Win.Registry,
  RegistryKeys;

constructor TKeymanAPITest_BCP47.Create;
begin
  // This shouldn't be needed but it seems the parent constructor doesn't run when
  // instantiated by DUnitX unless constructor is present in child class?
  inherited Create;
end;

procedure TKeymanAPITest_BCP47.Setup;
begin
  k := CoKeyman.Create;
  if not k.SystemInfo.IsAdministrator then
  begin
    Assert.Fail('Must be running as administrator in order to run elevated tests, to exclude use parameter --exclude:Elevated');
  end;
end;

procedure TKeymanAPITest_BCP47.TearDown;
begin
  k := nil;
end;

// Extracted from utilkeyman.pas.
function GetShortKeyboardName(const FileName: string): string;
begin
  if (LowerCase(ExtractFileExt(FileName)) = '.kmx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kxx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kmp')
    then Result := ChangeFileExt(ExtractFileName(FileName), '')
    else Result := FileName;
end;

// Extracted from utilkeyman.pas.
function GetRegistryKeyboardInstallKey_LM(const FileName: string): string;
begin
  Result := SRegKey_InstalledKeyboards_LM+'\'+GetShortKeyboardName(FileName);
end;

procedure TKeymanAPITest_BCP47.TestFile;
var
  kbd_file: IKeymanKeyboardFile;
  pkg_file: IKeymanPackageFile;
begin
  pkg_file := k.Packages.GetPackageFromFile(TestPath + 'bcp47.kmp');
  Assert.AreEqual(1, pkg_file.Keyboards.Count);
  kbd_file := pkg_file.Keyboards[0] as IKeymanKeyboardFile;
  Assert.AreEqual(2, kbd_file.Languages.Count);
  Assert.AreEqual('tpi', kbd_file.Languages[0].BCP47Code);
  Assert.AreEqual('Tok Pisin', kbd_file.Languages[0].Name);
  pkg_file := nil;
  kbd_file := nil;
end;

procedure TKeymanAPITest_BCP47.TestInstall;
var
  kbd_installed: IKeymanKeyboardInstalled;
  pkg_installed: IKeymanPackageInstalled;
begin
  k.Packages.Install(TestPath + 'bcp47.kmp', True);
  k.Packages.Refresh;
  k.Keyboards.Refresh;

  pkg_installed := k.Packages['bcp47'];
  Assert.IsNotNull(pkg_installed);

  kbd_installed := k.Keyboards['bcp47'];
  Assert.IsNotNull(kbd_installed);

  Assert.AreEqual(2, kbd_installed.Languages.Count, 'kbd.Languages.Count');
  Assert.AreEqual('tpi', kbd_installed.Languages[0].BCP47Code);
  Assert.AreEqual('Tok Pisin', kbd_installed.Languages[0].Name);
  Assert.AreNotEqual(0, kbd_installed.Languages[0].LangID);

  Assert.AreEqual('abg', kbd_installed.Languages[1].BCP47Code);
  Assert.AreEqual('Abaga', kbd_installed.Languages[1].Name);
  Assert.AreEqual(0, kbd_installed.Languages[1].LangID);

  Assert.IsTrue(kbd_installed.Languages[0].IsInstalled);
  Assert.IsFalse(kbd_installed.Languages[1].IsInstalled);

  kbd_installed := nil;

  // Test registry values
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('\'+GetRegistryKeyboardInstallKey_LM('bcp47')+'\'+SRegSubKey_SuggestedLanguages) then
    begin
      Assert.IsTrue(ValueExists('tpi'));
      Assert.AreEqual(ReadString('tpi'), 'Tok Pisin');

      Assert.IsTrue(ValueExists('abg'));
      Assert.AreEqual(ReadString('abg'), 'Abaga');
    end;
  finally
    Free;
  end;


  pkg_installed.Uninstall(True);
  k.Keyboards.Refresh;
  k.Keyboards.Apply;
end;

initialization
  TDUnitX.RegisterTestFixture(TKeymanAPITest_BCP47);
end.
