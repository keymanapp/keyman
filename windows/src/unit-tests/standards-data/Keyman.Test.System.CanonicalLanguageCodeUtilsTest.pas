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
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk'));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab'));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Arab-IR'));
  Assert.AreEqual('glk-Arab-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-IR'));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn'));
  Assert.AreEqual('glk-Latn-IR', TCanonicalLanguageCodeUtils.FindBestTag('glk-Latn-IR'));

  // #1719
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt'));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-YE'));
  Assert.AreEqual('sqt-Arab-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Arab'));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn'));
  Assert.AreEqual('sqt-Latn-YE', TCanonicalLanguageCodeUtils.FindBestTag('sqt-Latn-YE'));

  Assert.AreEqual('sa-Latn-IN', TCanonicalLanguageCodeUtils.FindBestTag('sa-Latn'));
  Assert.AreEqual('hi-Latn-IN', TCanonicalLanguageCodeUtils.FindBestTag('hi-Latn'));

  // #1282
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw'));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-MM'));
  Assert.AreEqual('raw-Latn-MM', TCanonicalLanguageCodeUtils.FindBestTag('raw-Latn'));

  // Various extended tags and tests
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km'));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-kh'));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr'));
  Assert.AreEqual('km-KH', TCanonicalLanguageCodeUtils.FindBestTag('km-khmr-kh'));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th'));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-th'));
  Assert.AreEqual('th-TH', TCanonicalLanguageCodeUtils.FindBestTag('th-thai-th'));

  // note casing shows that the following lang tag was returned unmodified because
  // it was not found in our canonicalization tables. This is expected behaviour.
  // (Remember that BCP 47 tags are not case sensitive)
  Assert.AreEqual('th-latn-de', TCanonicalLanguageCodeUtils.FindBestTag('th-latn-de'));

  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr'));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-FR'));
  Assert.AreEqual('fr-FR', TCanonicalLanguageCodeUtils.FindBestTag('fr-Latn-fr'));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn'));
  Assert.AreEqual('arn-Latn-CL', TCanonicalLanguageCodeUtils.FindBestTag('arn-cl'));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se'));
  Assert.AreEqual('se-Latn-NO', TCanonicalLanguageCodeUtils.FindBestTag('se-NO'));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma'));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn'));
  Assert.AreEqual('kma-Latn-GH', TCanonicalLanguageCodeUtils.FindBestTag('kma-latn-gh'));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi'));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-PG'));
  Assert.AreEqual('tpi-PG', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-PG'));
  Assert.AreEqual('sv-SE', TCanonicalLanguageCodeUtils.FindBestTag('sv'));
  Assert.AreEqual('en-US', TCanonicalLanguageCodeUtils.FindBestTag('en'));

  // fonipa
  Assert.AreEqual('en-US-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('en-fonipa'));
  Assert.AreEqual('tpi-PG-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('tpi-Latn-fonipa'));
  Assert.AreEqual('se-Latn-NO-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-fonipa'));
  Assert.AreEqual('se-Latn-NO-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('se-no-fonipa'));
  Assert.AreEqual('fr-FR-fonipa', TCanonicalLanguageCodeUtils.FindBestTag('fr-fonipa'));
end;

initialization
  TDUnitX.RegisterTestFixture(TCanonicalLanguageCodeUtilsTest);
end.
