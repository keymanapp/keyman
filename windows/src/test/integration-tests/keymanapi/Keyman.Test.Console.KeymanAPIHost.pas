unit Keyman.Test.Console.KeymanAPIHost;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Winapi.ActiveX,
  System.Win.ComObj,
  System.SysUtils,
  keymanapi_tlb,
  Keyman.Test.Console.KeymanAPITest_Base;

type
  [TestFixture]
  [Category('Default')]
  TKeymanAPITest = class(TKeymanAPITest_Base)
  public
    constructor Create;

    [Setup]
    procedure Setup;

    [Teardown]
    procedure Teardown;

    [Test]
    procedure Test_IKeyman;
    [Test]
    procedure Test_IKeyman_Apply;
    [Test]
    procedure Test_IKeyman_Refresh;

    [Test]
    procedure Test_IKeymanCollection;

    [Test]
    procedure Test_IKeymanErrors;

    [Test]
    procedure Test_IKeymanHotkeys;

    [Test]
    procedure Test_IKeymanKeyboardLanguagesInstalled;

    [Test]
    procedure Test_IKeymanKeyboardOptions;

    [Test]
    procedure Test_IKeymanKeyboards;

    [Test]
    procedure Test_IKeymanKeyboardsInstalled;

    [Test]
    procedure Test_IKeymanPackageContentKeyboards;

    [Test]
    procedure Test_IKeymanLanguages;

    [Test]
    procedure Test_IKeymanOptions;

    [Test]
    procedure Test_IKeymanPackageContentFiles;

    [Test]
    procedure Test_IKeymanPackageContentFonts;

    [Test]
    procedure Test_IKeymanPackagesInstalled;

    [Test]
    procedure Test_IKeymanControl;

    [Test]
    procedure Test_IKeymanError;

    [Test]
    procedure Test_IKeymanHotkey;

    [Test]
    procedure Test_IKeymanKeyboard;

    [Test]
    procedure Test_IKeymanKeyboardFile;

    [Test]
    procedure Test_IKeymanKeyboardLanguageInstalled;

    [Test]
    procedure Test_IKeymanKeyboardOption;

    [Test]
    procedure Test_IKeymanLanguage;

    [Test]
    procedure Test_IKeymanOption;

    [Test]
    procedure Test_IKeymanPackage;

    [Test]
    procedure Test_IKeymanPackageFile;

    [Test]
    procedure Test_IKeymanPackageContentFile;

    [Test]
    procedure Test_IKeymanPackageContentFont;

    [Test]
    procedure Test_IKeymanSystemInfo;

    [Test]
    procedure Test_IKeymanUserInterface;

    [Test]
    procedure Test_IKeymanVisualKeyboard;
  end;

  [TestFixture]
  [Category('Elevated')]
  TKeymanAPITest_Elevated = class(TKeymanAPITest_Base)
  public
    constructor Create;

    [Setup]
    procedure Setup;

    [Teardown]
    procedure Teardown;

    [Test]
    procedure Test_IKeymanKeyboardInstalled;

    [Test]
    procedure Test_IKeymanPackageInstalled;
  end;

implementation

uses
  KeymanOptionNames,
  System.Variants;

//
// IKeyman
//

constructor TKeymanAPITest.Create;
begin
  // This shouldn't be needed but it seems the parent constructor doesn't run when
  // instantiated by DUnitX unless constructor is present in child class?
  inherited Create;
end;

procedure TKeymanAPITest.Setup;
begin
  k := CoKeyman.Create;
end;

procedure TKeymanAPITest.Teardown;
begin
  k := nil;
end;

procedure TKeymanAPITest.Test_IKeyman;
begin
  // Implicitly tests each interface _Add/_Release as well,
  // verifying that they are legit objects
  Assert.IsNotNull(k.Keyboards);
  Assert.IsNotNull(k.Languages);
  Assert.IsNotNull(k.Packages);
  Assert.IsNotNull(k.SystemInfo);
  Assert.IsNotNull(k.Errors);
  Assert.IsNotNull(k.Options);
  Assert.IsNotNull(k.Hotkeys);
  Assert.IsNotNull(k.Control);

  // Test: should not crash
  k.AutoApply := True;
  k.AutoApply := False;
end;

procedure TKeymanAPITest.Test_IKeyman_Apply;
begin
  // Test: should not crash
  k.Apply;
end;

procedure TKeymanAPITest.Test_IKeyman_Refresh;
begin
  // Test: Should not crash
  k.Refresh;
end;

//
// IKeymanCollection
//

procedure TKeymanAPITest.Test_IKeymanCollection;
var
  i, n, j: Integer;
begin
  // We'll run basic tests against the IKeymanCollection implementation as
  // presented by IKeymanOptions
  n := k.Options.Count;
  for i := 0 to n - 1 do
  begin
    Assert.IsNotNull(k.Options.Items[i]);
    j := k.Options.IndexOf(k.Options.Items[i].ID);
    Assert.AreEqual(j, i);
    Assert.IsNotNull(k.Options.Items[k.Options.Items[i].ID]);
  end;
end;

//
// IKeymanControl
//

procedure TKeymanAPITest.Test_IKeymanControl;
begin
  Assert.IsNotNull(k.Control.ActiveLanguage);

  // Test against crash
  Assert.IsTrue(k.Languages.Count > 0);
  k.Control.ActiveLanguage := k.Languages[0];

  Assert.AreEqual(k.Languages[0].BCP47Code, k.Control.ActiveLanguage.BCP47Code);

  // These functions may return true or false as we won't currently test the
  // functionality, just that they don't crash
  k.Control.IsConfigurationOpen;
  k.Control.IsKeymanRunning;
  k.Control.IsOnlineUpdateCheckOpen;
  k.Control.IsTextEditorOpen;
  k.Control.IsVisualKeyboardOpen;

{$IFDEF WIN64}
  try
    k.Control.LastActiveWindow;
    Assert.Fail('Control.LastActiveWindow is not implemented on Win64');
  except
    on E:EOleException do
      Assert.AreEqual(Cardinal(E.ErrorCode), Cardinal($80004001));
    else
      raise;
  end;
  try
    k.Control.LastFocusWindow;
    Assert.Fail('Control.LastFocusWindow is not implemented on Win64');
  except
    on E:EOleException do
      Assert.AreEqual(Cardinal(E.ErrorCode), Cardinal($80004001));
    else
      raise;
  end;
{$ELSE}
  k.Control.LastActiveWindow;
  k.Control.Get_LastFocusWindow;
{$ENDIF}

  // We won't test OpenXXXX because we (a) it's asynchronous, and (b)
  // complex to close the window that we open -- we could do this with
  // a helper wrapper, but let's focus on the other tests first

  //k.Control.OpenConfiguration;
  //k.Control.OpenDiagnostics;
  //k.Control.OpenHelp(const Topic: WideString);
  //k.Control.OpenTextEditor;
  //k.Control.OpenUpdateCheck;
  //k.Control.ShowKeyboardWelcome(<kbd>);

  // In the future, we can do an integration test run that includes starting
  // and stopping Keyman and controlling the visual keyboard
  //k.Control.StartKeyman;
  //k.Control.StartVisualKeyboard;
  //k.Control.StopKeyman;
  //k.Control.StopVisualKeyboard;
end;

//
// IKeymanErrors
//

procedure TKeymanAPITest.Test_IKeymanError;
begin
  // We can't easily test this at present;
  // see the test in IKeymanErrors for zero errors
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanErrors;
begin
  // This test is not great because it depends on
  // environment -- there may have been a previous
  // error...
//  Assert.AreEqual(0, k.Errors.Count);
end;

//
// IKeymanHotkeys
//

procedure TKeymanAPITest.Test_IKeymanHotkey;
begin
  // Tested in IKeymanHotkeys
  Assert.Pass
end;

procedure TKeymanAPITest.Test_IKeymanHotkeys;
begin
  Assert.IsTrue(k.Hotkeys.Count > 0);
  k.Hotkeys[0].IsEmpty;
  k.Hotkeys[0].Modifiers;
  k.Hotkeys[0].VirtualKey;
  k.Hotkeys[0].RawValue;
  Assert.IsTrue(k.Hotkeys[0].Target <= kh__High);
end;

//
// IKeymanKeyboard*
//

procedure TKeymanAPITest.Test_IKeymanKeyboard;
begin
  // Tested in IKeymanKeyboardFile and IKeymanKeyboardInstalled
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardFile;
var
  kbd: IKeymanKeyboardFile;
begin
  kbd := k.Keyboards.GetKeyboardFromFile(TestPath + 'test.kmx');
  CheckKeyboardProperties(kbd);
  // kbd.Install()... Todo: test installation (forced installation is tested in IKeymanKeyboardInstalled)
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardLanguageInstalled;
begin
  // Tested in Test_IKeymanKeyboardInstalled
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardLanguagesInstalled;
begin
  // Tested in Test_IKeymanKeyboardInstalled
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardOption;
begin
  //TODO
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardOptions;
begin
  //TODO
end;

procedure TKeymanAPITest.Test_IKeymanKeyboards;
begin
  // Tested in Test_IKeymanKeyboardInstalled
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardsInstalled;
begin
  // Tested in Test_IKeymanKeyboardInstalled
  Assert.Pass;
end;

//
// IKeymanLanguage*
//

procedure TKeymanAPITest.Test_IKeymanLanguage;
begin
  Assert.IsTrue(k.Languages.Count > 0);

  k.Languages[0].BCP47Code;
  k.Languages[0].HKL;
  k.Languages[0].Hotkey;
  k.Languages[0].KeymanKeyboardLanguage;
  k.Languages[0].LangID;
  k.Languages[0].LayoutName;
  k.Languages[0].LocaleName;
  k.Languages[0].ProfileGUID;
  k.Languages[0].ClassID;
end;

procedure TKeymanAPITest.Test_IKeymanLanguages;
begin
  // Tested in Test_IKeymanLanguage
  Assert.Pass;
end;

//
// IKeymanOption*
//

procedure TKeymanAPITest.Test_IKeymanOption;
var
  n: string;
  o: IKeymanOption;
  v: Boolean;
begin
  // We'll test the values for a single property
  //TODO: test KeymanOptionInfo from utilkeymanoption to test all properties instead?

  n := KeymanOptionName(koAltGrCtrlAlt);
  o := k.Options[n];
  Assert.IsNotNull(o);


  Assert.AreEqual(False, Boolean(o.DefaultValue));
  Assert.AreEqual(True, o.Enabled);
  Assert.AreEqual('kogGeneral', o.Group);
  Assert.AreEqual(n, o.ID);

  v := o.Value;

  o.Value := True;
  Assert.AreEqual(True, Boolean(o.Value));
  k.Options.Apply;
  k.Options.Refresh;
  o := k.Options[n];
  Assert.IsNotNull(o);
  Assert.AreEqual(True, Boolean(o.Value));

  o.Value := False;
  Assert.AreEqual(False, Boolean(o.Value));
  k.Options.Apply;
  k.Options.Refresh;
  o := k.Options[n];
  Assert.IsNotNull(o);
  Assert.AreEqual(False, Boolean(o.Value));

  o.Value := v;
  k.Options.Apply;
  k.Options.Refresh;
end;

procedure TKeymanAPITest.Test_IKeymanOptions;
begin
  // Tested in Test_IKeymanOption, Test_IKeymanCollection
  Assert.Pass;
end;

//
// IKeymanPackage*
//

procedure TKeymanAPITest.Test_IKeymanPackage;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFile;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFiles;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFont;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFonts;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentKeyboards;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanPackageFile;
var
  pkg: IKeymanPackageFile;
begin
  pkg := k.Packages.GetPackageFromFile(TestPath + 'test.kmp');
  Assert.IsNotNull(pkg);
  Assert.AreEqual('test.kmp', ExtractFileName(pkg.Filename));
  CheckPackageProperties(pkg);
end;

procedure TKeymanAPITest.Test_IKeymanPackagesInstalled;
begin
  // Tested in Test_IKeymanPackageFile, Test_IKeymanPackageInstalle
  Assert.Pass;
end;

//
// IKeymanSystemInfo
//

procedure TKeymanAPITest.Test_IKeymanSystemInfo;
begin
  Assert.IsNotEmpty(k.SystemInfo.EngineInstallPath);
  Assert.IsNotEmpty(k.SystemInfo.EngineVersion);
  k.SystemInfo.IsAdministrator;
  k.SystemInfo.RebootRequired;
  // Don't set k.SystemInfo.SetReboot -- too disruptive
end;

procedure TKeymanAPITest.Test_IKeymanUserInterface;
begin
  // Nothing to test
  Assert.Pass;
end;

procedure TKeymanAPITest.Test_IKeymanVisualKeyboard;
begin
  // Tested in Test_IKeymanKeyboardInstalled
  Assert.Pass;
end;

//
// The following tests require Administrator
//

constructor TKeymanAPITest_Elevated.Create;
begin
  // This shouldn't be needed but it seems the parent constructor doesn't run when
  // instantiated by DUnitX unless constructor is present in child class?
  inherited Create;
end;

procedure TKeymanAPITest_Elevated.Setup;
begin
  k := CoKeyman.Create;
  if not k.SystemInfo.IsAdministrator then
  begin
    Assert.Fail('Must be running as administrator in order to run elevated tests, to exclude use parameter --exclude:Elevated');
  end;
end;

procedure TKeymanAPITest_Elevated.Teardown;
begin
  k := nil;
end;

procedure TKeymanAPITest_Elevated.Test_IKeymanKeyboardInstalled;
var
  kbd: IKeymanKeyboardInstalled;
begin
  k.Keyboards.Install(TestPath + 'test.kmx', True);
  k.Keyboards.Refresh;
  kbd := k.Keyboards['test'];
  Assert.IsNotNull(kbd);

  CheckKeyboardProperties(kbd);

  Assert.IsNotEmpty(kbd.IconFilename);
  Assert.AreEqual(1, kbd.Languages.Count);
  Assert.AreEqual(kbd.KeymanID, kbd.Languages[0].OwnerKeyboard.KeymanID);
  Assert.AreEqual('fr-FR', kbd.Languages[0].BCP47Code);
  Assert.AreEqual($040C, kbd.Languages[0].LangID);
  //kbd.Loaded;
  //kbd.Options
  Assert.IsNull(kbd.OwnerPackage);
//  Assert.IsNull(kbd.VisualKeyboard);

//  kbd.InstallVisualKeyboard(const Filename: WideString); //todo
  kbd.Uninstall;
  k.Keyboards.Refresh;
  k.Keyboards.Apply;
end;

procedure TKeymanAPITest_Elevated.Test_IKeymanPackageInstalled;
var
  pkg: IKeymanPackageInstalled;
  kbd: IKeymanKeyboardInstalled;
begin
  k.Packages.Install(TestPath + 'test.kmp', True);
  k.Packages.Refresh;
  k.Keyboards.Refresh;
  pkg := k.Packages['test'];
  Assert.IsNotNull(pkg);
  Assert.AreEqual('kmp.inf', ExtractFileName(pkg.Filename));
  CheckPackageProperties(pkg);

  kbd := k.Keyboards['test'];
  Assert.IsNotNull(kbd);
  CheckKeyboardProperties(kbd);
  Assert.IsNotNull(kbd.OwnerPackage);
  Assert.AreEqual('test', kbd.OwnerPackage.ID);
  Assert.IsNotNull(kbd.VisualKeyboard);

  pkg.Uninstall(True);
  k.Packages.Refresh;
  k.Keyboards.Refresh;
  k.Keyboards.Apply;
end;


initialization
  TDUnitX.RegisterTestFixture(TKeymanAPITest);
  TDUnitX.RegisterTestFixture(TKeymanAPITest_Elevated);
end.
