unit Keyman.Test.System.Standards.LangTagsRegistryTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TLangTagsTest = class(TObject)
  public
    [Test]
    procedure TestSomeTags;
  end;

implementation

uses
  Keyman.System.Standards.LangTagsRegistry;

{ TLangTagsTest }

procedure TLangTagsTest.TestSomeTags;
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

  // Khmer
  Assert.AreEqual('km', TLangTagsMap.LangTags['km'].tag);
  Assert.AreEqual('km-Khmr-KH', TLangTagsMap.LangTags['km'].full);
  Assert.AreEqual('khm', TLangTagsMap.LangTags['km'].iso639_3);
  Assert.AreEqual('KH', TLangTagsMap.LangTags['km'].region);
  Assert.AreEqual('Khmer', TLangTagsMap.LangTags['km'].name);
  Assert.AreEqual('Khmr', TLangTagsMap.LangTags['km'].script);
  Assert.AreEqual(True, TLangTagsMap.LangTags['km'].suppress);
  Assert.AreEqual('km', TLangTagsMap.LangTags['km'].windows);

  // Ak
  Assert.AreEqual('akq', TLangTagsMap.LangTags['akq'].tag);
  Assert.AreEqual('akq-Latn-PG', TLangTagsMap.LangTags['akq'].full);
  Assert.AreEqual('akq', TLangTagsMap.LangTags['akq'].iso639_3);
  Assert.AreEqual('PG', TLangTagsMap.LangTags['akq'].region);
  Assert.AreEqual(False, TLangTagsMap.LangTags['akq'].suppress);
  Assert.AreEqual('akq-Latn', TLangTagsMap.LangTags['akq'].windows);

  // All Tags

  Assert.AreEqual('arx', TLangTagsMap.AllTags['arx-BR']);
  Assert.AreEqual('bfi-x-stokoe', TLangTagsMap.AllTags['sgn-bfi-GB-x-stokoe']);
  Assert.AreEqual('bfy', TLangTagsMap.AllTags['ppa-Deva-IN']);
end;

initialization
  TDUnitX.RegisterTestFixture(TLangTagsTest);
end.
