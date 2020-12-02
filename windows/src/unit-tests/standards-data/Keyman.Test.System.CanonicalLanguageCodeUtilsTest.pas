unit Keyman.Test.System.CanonicalLanguageCodeUtilsTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TCanonicalLanguageCodeUtilsTest = class(TObject)
  public
    [Test]
    procedure TestSomeTags;
  end;

implementation

uses
  Keyman.System.CanonicalLanguageCodeUtils;

{ TCanonicalLanguageCodeUtilsTest }

procedure TCanonicalLanguageCodeUtilsTest.TestSomeTags;
begin
  // #3485 - Gilaki (Latin) script
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk', True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab', True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab-IR', True));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-IR', True));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn', True));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn-IR', True));

  // #1719
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt', True));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-YE', True));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Arab', True));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn', True));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn-YE', True));

  Assert.AreEqual('sa-Latn-IN', TCanonicalLanguageCodeUtils.FindBestTag('sa-Latn', True));
  Assert.AreEqual('hi-Latn-IN', TCanonicalLanguageCodeUtils.FindBestTag('hi-Latn', True));

  // #1282
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw', True));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-MM', True));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-Latn', True));

  // Various extended tags and tests
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km', True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-kh', True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr', True));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr-kh', True));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th', True));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-th', True));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-thai-th', True));

  // note casing shows that the following lang tag was returned unmodified because
  // it was not found in our canonicalization tables. This is expected behaviour.
  // (Remember that BCP 47 tags are not case sensitive)
  Assert.AreEqual('th-latn-de', TCanonicalLanguageCodeUtils.FindBestTag('th-latn-de', True));

  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr', True));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-FR', True));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-Latn-fr', True));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn', True));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn-cl', True));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se', True));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se-NO', True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma', True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn', True));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn-gh', True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi', True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-PG', True));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-PG', True));
  Assert.AreEqual('sv-SE', TCanonicalLanguageCodeUtils.FindBestTag('sv', True));
  Assert.AreEqual('en-US', TCanonicalLanguageCodeUtils.FindBestTag('en', True));

  // fonipa
  Assert.AreEqual('en-US-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('en-fonipa', True));
  Assert.AreEqual('tpi-PG-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-fonipa', True));
  Assert.AreEqual('se-Latn-NO-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-fonipa', True));
  Assert.AreEqual('se-Latn-NO-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-no-fonipa', True));
  Assert.AreEqual('fr-FR-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('fr-fonipa', True));
end;

initialization
  TDUnitX.RegisterTestFixture(TCanonicalLanguageCodeUtilsTest);
end.
