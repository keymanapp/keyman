unit Keyman.Test.Console.KeymanAPIHost;

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  Winapi.ActiveX,
  System.Win.ComObj,
  System.SysUtils,
  keymanapi_tlb;

type
  [TestFixture]
  TKeymanAPITest = class
  private
    FTestPath: string;
    k: IKeyman;
  public
    constructor Create;
    destructor Destroy; override;

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
    procedure Test_IKeymanKeyboardInstalled;

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
    procedure Test_IKeymanPackageInstalled;

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

implementation

constructor TKeymanAPITest.Create;
begin
  inherited Create;
  FTestPath := ExtractFilePath(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + 'test\';
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  k := CoKeyman.Create;
end;

destructor TKeymanAPITest.Destroy;
begin
  k := nil;
  CoUninitialize;
  inherited Destroy;
end;

//
// IKeyman
//

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

  // We won't test OpenXXXX because we have no programatic way to close the
  // window that we open again

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

procedure TKeymanAPITest.Test_IKeymanError;
begin
  // We can't easily test this at present;
  // see the test in IKeymanErrors for zero errors
end;

procedure TKeymanAPITest.Test_IKeymanErrors;
begin
  Assert.AreEqual(0, k.Errors.Count);
end;

procedure TKeymanAPITest.Test_IKeymanHotkey;
begin
  // We will test this in IKeymanHotkeys
end;

procedure TKeymanAPITest.Test_IKeymanHotkeys;
begin
  Assert.IsTrue(k.Hotkeys.Count > 0);
  k.Hotkeys[0].IsEmpty;
  k.Hotkeys[0].Modifiers;
  k.Hotkeys[0].VirtualKey;
  k.Hotkeys[0].RawValue;
  Assert.IsTrue(k.Hotkeys[0].Target <> 0);
end;

procedure TKeymanAPITest.Test_IKeymanKeyboard;
begin
  // Nothing to test in the base class; we will
  // test IKeymanKeyboardFile and IKeymanKeyboardInstalled
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardFile;
var
  f: IKeymanKeyboardFile;
begin
  f := k.Keyboards.GetKeyboardFromFile(FTestPath + 'test.kmx');
  Assert.AreEqual('กขฃ', f.GetCharsUsed);
  Assert.AreEqual('(C) 2017 SIL International', f.Copyright);
  Assert.AreEqual('eng', f.DefaultBCP47Languages);
  Assert.AreEqual(1036, f.DefaultPrimaryLanguage);
  Assert.AreEqual('x040C x040C', f.DefaultWindowsLanguages);
//  Assert.IsNotNull(f.DefaultHotkey);  TODO: Resolve DefaultHotkey implementation -- remove from reg, read from kmx in --installed, read from kmx in --file as well
//  Assert.AreEqual(False, f.DefaultHotkey.IsEmpty);
//  Assert.AreEqual(HK_ALT or HK_CTRL, f.DefaultHotkey.Modifiers);
//  Assert.AreEqual(Ord('T'), f.DefaultHotkey.VirtualKey);
//  Assert.AreEqual(0, f.DefaultHotkey.RawValue);
//  Assert.AreEqual(0, f.DefaultHotkey.Target);
  Assert.AreEqual(keUnicode, f.Encodings);
  Assert.AreEqual(FTestPath + 'test.kmx', f.Filename);
  Assert.AreEqual('test', f.ID);
  Assert.AreEqual(kltPositional, f.LayoutType);
  Assert.AreEqual('Test Message', f.Message);
  Assert.AreEqual('Test Keyboard', f.Name);
  Assert.AreEqual('1.2.3', f.Version);
  Assert.IsNotNull(f.Bitmap);
//  Assert.AreEqual(f.SerializeXML(...), False);
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardLanguageInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardLanguagesInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardOption;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardOptions;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboards;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanKeyboardsInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanLanguage;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanLanguages;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanOption;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanOptions;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackage;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFile;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFiles;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFont;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentFonts;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageContentKeyboards;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageFile;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackageInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanPackagesInstalled;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanSystemInfo;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanUserInterface;
begin
  Assert.Fail();
end;

procedure TKeymanAPITest.Test_IKeymanVisualKeyboard;
begin
  Assert.Fail();
end;

initialization
  TDUnitX.RegisterTestFixture(TKeymanAPITest);
end.
