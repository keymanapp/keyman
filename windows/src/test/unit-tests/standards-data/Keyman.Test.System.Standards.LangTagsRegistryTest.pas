unit Keyman.Test.System.Standards.LangTagsRegistryTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TLangTagsTest = class(TObject)
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure TestTokPisinTag;
    [Test]
    procedure TestKhmerTag;
    [Test]
    procedure TestAkqTag;
    [Test]
    procedure TestAllTags;
    [Test]
    procedure TestUndTag;
  end;

implementation

uses
  Keyman.System.Standards.LangTagsRegistry;

{ TLangTagsTest }

procedure TLangTagsTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

procedure TLangTagsTest.TestTokPisinTag;
begin
  // Tok Pisin
  Assert.AreEqual('tpi', TLangTagsMap.LangTags['tpi'].tag);
  Assert.AreEqual('tpi-Latn-PG', TLangTagsMap.LangTags['tpi'].full);
  Assert.AreEqual('tpi', TLangTagsMap.LangTags['tpi'].iso639_3);
  Assert.AreEqual('PG', TLangTagsMap.LangTags['tpi'].region);
  Assert.AreEqual('Tok Pisin', TLangTagsMap.LangTags['tpi'].name);
  Assert.AreEqual('Latn', TLangTagsMap.LangTags['tpi'].script);
  Assert.AreEqual(True, TLangTagsMap.LangTags['tpi'].suppress);
  Assert.AreEqual('tpi', TLangTagsMap.LangTags['tpi'].windows);
end;

procedure TLangTagsTest.TestKhmerTag;
begin
  // Khmer
  Assert.AreEqual('km', TLangTagsMap.LangTags['km'].tag);
  Assert.AreEqual('km-Khmr-KH', TLangTagsMap.LangTags['km'].full);
  Assert.AreEqual('khm', TLangTagsMap.LangTags['km'].iso639_3);
  Assert.AreEqual('KH', TLangTagsMap.LangTags['km'].region);
  Assert.AreEqual('Khmer', TLangTagsMap.LangTags['km'].name);
  Assert.AreEqual('Khmr', TLangTagsMap.LangTags['km'].script);
  Assert.AreEqual(True, TLangTagsMap.LangTags['km'].suppress);
  Assert.AreEqual('km', TLangTagsMap.LangTags['km'].windows);
end;

procedure TLangTagsTest.TestAkqTag;
begin
  // Ak
  Assert.AreEqual('akq', TLangTagsMap.LangTags['akq'].tag);
  Assert.AreEqual('akq-Latn-PG', TLangTagsMap.LangTags['akq'].full);
  Assert.AreEqual('akq', TLangTagsMap.LangTags['akq'].iso639_3);
  Assert.AreEqual('PG', TLangTagsMap.LangTags['akq'].region);
  Assert.AreEqual(False, TLangTagsMap.LangTags['akq'].suppress);
  Assert.AreEqual('akq-Latn', TLangTagsMap.LangTags['akq'].windows);
end;

procedure TLangTagsTest.TestAllTags;
begin
  // All Tags
  Assert.AreEqual('arx', TLangTagsMap.AllTags['arx-BR']);
  Assert.AreEqual('bfi-Zzzz-x-stokoe', TLangTagsMap.AllTags['sgn-bfi-GB-x-stokoe']);
  Assert.AreEqual('bfy', TLangTagsMap.AllTags['ppa-Deva-IN']);
end;

procedure TLangTagsTest.TestUndTag;
begin
  // und
  Assert.AreEqual('und', TLangTagsMap.LangTags['und'].tag);
  Assert.AreEqual('und-Zyyy-001', TLangTagsMap.LangTags['und'].full);
  Assert.AreEqual('und', TLangTagsMap.LangTags['und'].iso639_3);
  Assert.AreEqual('001', TLangTagsMap.LangTags['und'].region);
  Assert.AreEqual('Undetermined', TLangTagsMap.LangTags['und'].name);
  Assert.AreEqual('Zyyy', TLangTagsMap.LangTags['und'].script);
  Assert.AreEqual(False, TLangTagsMap.LangTags['und'].suppress);
  Assert.AreEqual('und-Zyyy', TLangTagsMap.LangTags['und'].windows);
end;

initialization
  TDUnitX.RegisterTestFixture(TLangTagsTest);
end.
