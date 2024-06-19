unit Keyman.Test.System.CanonicalLanguageCodeUtilsTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TCanonicalLanguageCodeUtilsTest = class(TObject)
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure TestGlkTag;
    [Test]
    procedure TestSqtTag;
    [Test]
    procedure TestSomeTags;
    [Test]
    procedure TestRawTag;
    [Test]
    procedure TestKmTag;
    [Test]
    procedure TestThTag;
    [Test]
    procedure TestTagsWithScript;
    [Test]
    procedure TestFonipaTags;
    [Test]
    procedure TestAzTag;
  end;

implementation

uses
  Keyman.System.CanonicalLanguageCodeUtils;

{ TCanonicalLanguageCodeUtilsTest }

procedure TCanonicalLanguageCodeUtilsTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

procedure TCanonicalLanguageCodeUtilsTest.TestGlkTag;
begin
  // #3485 - Gilaki (Latin) script
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk', True, True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab', True, True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab-IR', True, True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-IR', True, True));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn', True, True));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn-IR', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestSqtTag;
begin
  // #1719
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt', True, True));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-YE', True, True));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Arab', True, True));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn', True, True));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn-YE', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestSomeTags;
begin
  Assert.AreEqual('sa-Latn', TCanonicalLanguageCodeUtils.FindBestTag('sa-Latn', True, True));
  Assert.AreEqual('he-Latn', TCanonicalLanguageCodeUtils.FindBestTag('he-Latn', True, True));
  Assert.AreEqual('hi-Latn-IN', TCanonicalLanguageCodeUtils.FindBestTag('hi-Latn', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestRawTag;
begin
  // #1282
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw', True, True));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-MM', True, True));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-Latn', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestKmTag;
begin
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km', True, True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-kh', True, True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr', True, True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr-kh', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestThTag;
begin
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th', True, True));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-th', True, True));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-thai-th', True, True));

  // note casing shows that the following lang tag was returned unmodified because
  // it was not found in our canonicalization tables. This is expected behaviour.
  // (Remember that BCP 47 tags are not case sensitive)
  Assert.AreEqual('th-Latn-DE', TCanonicalLanguageCodeUtils.FindBestTag('th-latn-de', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestTagsWithScript;
begin
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr', True, True));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-FR', True, True));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-Latn-fr', True, True));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn', True, True));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn-cl', True, True));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se', True, True));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se-NO', True, True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma', True, True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn', True, True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn-gh', True, True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi', True, True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-PG', True, True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-PG', True, True));
  Assert.AreEqual('sv-SE', TCanonicalLanguageCodeUtils.FindBestTag('sv', True, True));
  Assert.AreEqual('en-US', TCanonicalLanguageCodeUtils.FindBestTag('en', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestFonipaTags;
begin
  // fonipa
  Assert.AreEqual('en-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('en-fonipa', True, True));
  Assert.AreEqual('tpi-Latn-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-fonipa', True, True));
  Assert.AreEqual('se-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-fonipa', True, True));
  Assert.AreEqual('se-NO-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-no-fonipa', True, True));
  Assert.AreEqual('fr-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('fr-fonipa', True, True));
  Assert.AreEqual('und-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('und-Latn-fonipa', True, True));
  Assert.AreEqual('und-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('und-Latn-fonipa-x-test', True, True));
end;

procedure TCanonicalLanguageCodeUtilsTest.TestAzTag;
begin
  // az-Cyrl
  Assert.AreEqual('az-Cyrl-RU', TCanonicalLanguageCodeUtils.FindBestTag('az-Cyrl', True, True));
end;

initialization
  TDUnitX.RegisterTestFixture(TCanonicalLanguageCodeUtilsTest);
end.
