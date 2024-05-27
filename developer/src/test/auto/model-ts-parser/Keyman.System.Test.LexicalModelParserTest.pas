unit Keyman.System.Test.LexicalModelParserTest;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TLexicalModelParserTest = class(TObject)
  private
    m: TStringList;
    AssetRootPath: string;
    mcustom: TStringList;
    mnocomment: TStrings;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRead;

    [Test]
    procedure TestWrite;

    [Test]
    procedure TestWriteDeleteComment;

    [Test]
    procedure TestWriteNewComment;
  end;

implementation

uses
  Keyman.Developer.System.LexicalModelParser,
  Keyman.Developer.System.LexicalModelParserTypes;

procedure TLexicalModelParserTest.Setup;
begin
  Assert.IgnoreCaseDefault := False;

  // Running from bin/platform/configuration/ folder has
  // assets two levels up.
  if FileExists('..\..\..\assets\nrc.en.mtnt.model.ts') then AssetRootPath := '..\..\..\assets\'
  // Running from project folder
  else if FileExists('assets\nrc.en.mtnt.model.ts') then AssetRootPath := 'assets\'
  // You must know what you are doing
  else AssetRootPath := '';

  m := TStringList.Create;
  m.LoadFromFile(AssetRootPath + 'nrc.en.mtnt.model.ts');

  mcustom := TStringList.Create;
  mcustom.LoadFromFile(AssetRootPath + 'custom.model.ts');

  mnocomment := TStringList.Create;
  mnocomment.LoadFromFile(AssetRootPath + 'nocomment.model.ts');
end;

procedure TLexicalModelParserTest.TearDown;
begin
  m.Free;
  mcustom.Free;
  mnocomment.Free;
end;

procedure TLexicalModelParserTest.TestRead;
var
  lm: TLexicalModelParser;
begin
  lm := TLexicalModelParser.Create(m.Text);
  try
    Assert.IsTrue(lm.Format = lmfTrie10, 'Expected lmfTrie10');
    Assert.IsTrue(lm.WordBreaker = lmwbDefault, 'Expected lmwbDefault');
    Assert.AreEqual(1, lm.Wordlists.Count, 'Expected 1 wordlist');
    Assert.AreEqual('mtnt.tsv', lm.Wordlists[0], 'Wordlist name');
    Assert.AreEqual(#13#10' Sample model for testing parser'#13#10, lm.Comment);
  finally
    lm.Free;
  end;
end;

procedure TLexicalModelParserTest.TestWrite;
var
  lm: TLexicalModelParser;
begin
  lm := TLexicalModelParser.Create(m.Text);
  try
    lm.Comment := 'Testing';
    lm.Format := lmfCustom10;
    lm.WordBreaker := lmwbAscii;
    lm.Wordlists.Clear;
    lm.Wordlists.Add('foo.tsv');
    lm.Wordlists.Add('bar.tsv');
    Assert.AreEqual(mcustom.Text.Trim, lm.Text.Trim); // Ignoring whitespace before/after
  finally
    lm.Free;
  end;
end;

procedure TLexicalModelParserTest.TestWriteNewComment;
var
  lm: TLexicalModelParser;
begin
  lm := TLexicalModelParser.Create(mnocomment.Text);
  try
    lm.Comment := #13#10' Sample model for testing parser'#13#10;
    Assert.AreEqual(m.Text.Trim, lm.Text.Trim); // Ignoring whitespace before/after
  finally
    lm.Free;
  end;
end;

procedure TLexicalModelParserTest.TestWriteDeleteComment;
var
  lm: TLexicalModelParser;
begin
  lm := TLexicalModelParser.Create(m.Text);
  try
    lm.Comment := '';
    Assert.AreEqual(mnocomment.Text.Trim, lm.Text.Trim); // Ignoring whitespace before/after
  finally
    lm.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLexicalModelParserTest);
end.
