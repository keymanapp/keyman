unit Keyman.Test.System.KMXFileLanguagesTest;

interface

uses
  System.Types,
  DUnitX.TestFramework,
  Keyman.System.KMXFileLanguages;

type

  [TestFixture]
  TKMXFileLanguagesTest = class(TObject)
  private
  public
    [Setup]
    procedure Setup;

    [Test]
    [TestCase('TestEnglish','eng,en')]
    [TestCase('TestAmharic','amh,am')]
    [TestCase('TestTokPisin','tpi,tpi')]
    procedure TestISO6393ToBCP47(const iso6393, bcp47: string);

    [Test]
    procedure TestWindowsLanguageListToArray;

    [Test]
    procedure TestEthnologueCodeListToArray;

    [Test]
    [TestCase('TestEthnologueCode1','amh,amh')]
    [TestCase('TestEthnologueCode2','amh ,amh')]
    [TestCase('TestEthnologueCode3','amh  ,amh')]
    [TestCase('TestEthnologueCode4',' amh ,amh')]
    [TestCase('TestEthnologueCode5','  amh,amh')]
    [TestCase('TestEthnologueCode6','  amh  ,amh')]
    procedure TestEthnologueCodeListToArray2(code1, code2: string);

    [Test]
    [TestCase('TestLCIDToBCP47-1', '0409,en-US')]
    [TestCase('TestLCIDToBCP47-2', '0c09,en-AU')]
    [TestCase('TestLCIDToBCP47-3', '0454,lo-LA')]
    procedure TestLCIDToBCP47(code1, code2: string);
  end;

implementation

uses
  System.SysUtils;

procedure TKMXFileLanguagesTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

procedure TKMXFileLanguagesTest.TestEthnologueCodeListToArray;
var
  s: TStringDynArray;
begin
  s := TKMXFileLanguages.EthnologueCodeListToArray('amh eng fra');
  Assert.AreEqual(3, Length(s), 'Length of ethnologue language list');
  Assert.AreEqual('amh', s[0]);
  Assert.AreEqual('eng', s[1]);
  Assert.AreEqual('fra', s[2]);
end;

procedure TKMXFileLanguagesTest.TestEthnologueCodeListToArray2(code1, code2: string);
var
  s: TStringDynArray;
begin
  s := TKMXFileLanguages.EthnologueCodeListToArray(code1);
  Assert.AreEqual(1, Length(s), 'Length of ethnologue list');
  Assert.AreEqual(code2, s[0]);
end;

procedure TKMXFileLanguagesTest.TestISO6393ToBCP47(const iso6393, bcp47: string);
begin
  Assert.AreEqual(bcp47, TKMXFileLanguages.TranslateISO6393ToBCP47(iso6393));
end;

procedure TKMXFileLanguagesTest.TestLCIDToBCP47(code1, code2: string);
begin
  Assert.AreEqual(code2, TKMXFileLanguages.TranslateWindowsLanguagesToBCP47(StrToInt('$'+code1)));
end;

procedure TKMXFileLanguagesTest.TestWindowsLanguageListToArray;
var
  s: TIntegerDynArray;
begin
  s := TKMXFileLanguages.WindowsLanguageListToArray('x0409 x0401 x04d0');
  Assert.AreEqual(3, Length(s), 'Length of windows language list');
  Assert.AreEqual($409, s[0]);
  Assert.AreEqual($401, s[1]);
  Assert.AreEqual($4d0, s[2]);

  s := TKMXFileLanguages.WindowsLanguageListToArray('x0409');
  Assert.AreEqual(1, Length(s), 'Length of windows language list');
  Assert.AreEqual($409, s[0]);
end;

initialization
  TDUnitX.RegisterTestFixture(TKMXFileLanguagesTest);
end.
