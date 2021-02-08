unit Keyman.Test.System.KeyboardJSInfoTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TKeyboardJSInfoTest = class(TObject) 
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestJSWithoutDebugCode;

    [Test]
    procedure TestJSWithDebugCode;
  end;

implementation

uses
  System.SysUtils,

  Keyman.System.KeyboardJSInfo;

procedure TKeyboardJSInfoTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

procedure TKeyboardJSInfoTest.TearDown;
begin
end;

procedure TKeyboardJSInfoTest.TestJSWithDebugCode;
begin
  with TKeyboardJSInfo.Create(ExtractFilePath(ParamStr(0))+'../../test/balochi_phonetic-1.1.js') do
  try
    Assert.AreEqual('Balochi Phonetic', Name);
    Assert.AreEqual('1.1', Version);
    Assert.AreEqual(True, RTL, 'RTL');
    Assert.AreEqual(False, Mnemonic, 'Mnemonic');
  finally
    Free;
  end;
end;

procedure TKeyboardJSInfoTest.TestJSWithoutDebugCode;
begin
  with TKeyboardJSInfo.Create(ExtractFilePath(ParamStr(0))+'../../test/balochi_phonetic-1.1-debug.js') do
  try
    Assert.AreEqual('Balochi Phonetic', Name);
    Assert.AreEqual('1.1', Version);
    Assert.AreEqual(True, RTL, 'RTL');
    Assert.AreEqual(False, Mnemonic, 'Mnemonic');
  finally
    Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TKeyboardJSInfoTest);
end.
