unit Keyman.Developer.System.Test.KMConvertParametersTest;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TKMConvertParametersTest = class(TObject)
  private
  public
    [Test]
    procedure TestKeyboardId;
    [Test]
    procedure TestAuthor;
    [Test]
    procedure TestBCP47Tags;
    [Test]
    procedure TestCopyright;
    [Test]
    procedure TestDestination;
    [Test]
    procedure TestFullCopyright;
    [Test]
    procedure TestKLID;
    [Test]
    procedure TestModelIdAuthor;
    [Test]
    procedure TestModelIdLanguage;
    [Test]
    procedure TestModelIdUniq;
    [Test]
    procedure TestName;
    [Test]
    procedure TestTargets;
    [Test]
    procedure TestVersion;
  end;

implementation

uses
  Keyman.Developer.System.KMConvertParameters;

procedure TKMConvertParametersTest.TestKLID;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-klid', '0000040c']));
  Assert.IsFalse(k.CheckParams(['template', '-klid', '0x0000040c']));
  Assert.IsFalse(k.CheckParams(['template', '-klid', 'foo']));
end;

procedure TKMConvertParametersTest.TestDestination;
begin
  Assert.Pass;  // no validation
end;

procedure TKMConvertParametersTest.TestKeyboardId;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-id', 'foo']));
  Assert.IsFalse(k.CheckParams(['template', '-id', 'FooBar']));
  Assert.IsFalse(k.CheckParams(['template', '-id', 'foo$']));
  Assert.IsFalse(k.CheckParams(['template', '-id', 'foo-bar']));
  Assert.IsFalse(k.CheckParams(['template', '-id', '0123']));
  Assert.IsFalse(k.CheckParams(['template', '-id', '0foo']));
  Assert.IsTrue(k.CheckParams(['template', '-id', 'foo_123']));
end;

procedure TKMConvertParametersTest.TestName;
begin
  Assert.Pass;  // no validation
end;

procedure TKMConvertParametersTest.TestCopyright;
begin
  Assert.Pass;  // no validation
end;

procedure TKMConvertParametersTest.TestFullCopyright;
begin
  Assert.Pass;  // no validation
end;

procedure TKMConvertParametersTest.TestVersion;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-version', '1.2.3']));
  Assert.IsFalse(k.CheckParams(['template', '-version', '1.2.3a']));
  Assert.IsFalse(k.CheckParams(['template', '-version', '1.0-alpha']));
  Assert.IsFalse(k.CheckParams(['template', '-version', 'evergreen']));
end;

procedure TKMConvertParametersTest.TestBCP47Tags;
var
  k: TKMConvertParameters;
  Found: Boolean;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-languages', 'en fr']));
  Assert.IsFalse(k.CheckParams(['template', '-languages', 'english']));
  Assert.IsFalse(k.CheckParams(['template', '-languages', '$$$']));

  Found := False;
  k.OnOutputText :=
    procedure(s: string) begin
      Log(TLogLevel.Information, s);
      Assert.AreEqual(s, 'Warning: -languages: ''en-us'' is a valid tag but is not canonical: it should be ''en''');
      Found := True;
    end;
  Assert.IsTrue(k.CheckParams(['template', '-languages', 'en-us']));
  Assert.IsTrue(Found);
end;

procedure TKMConvertParametersTest.TestAuthor;
begin
  Assert.Pass;  // no validation
end;

procedure TKMConvertParametersTest.TestTargets;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-targets', 'any']));
  Assert.IsTrue(k.CheckParams(['template', '-targets', 'windows macos']));
  Assert.IsFalse(k.CheckParams(['template', '-targets', 'pdp-11']));
end;

procedure TKMConvertParametersTest.TestModelIdAuthor;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-id-author', 'myself']));
  Assert.IsTrue(k.CheckParams(['template', '-id-author', 'my_self']));
  Assert.IsFalse(k.CheckParams(['template', '-id-author', 'my-self']));
  Assert.IsFalse(k.CheckParams(['template', '-id-author', 'Myself']));
  Assert.IsFalse(k.CheckParams(['template', '-id-author', '$myself$']));
end;

procedure TKMConvertParametersTest.TestModelIdLanguage;
var
  k: TKMConvertParameters;
  Found: Boolean;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-id-language', 'en']));
  Assert.IsFalse(k.CheckParams(['template', '-id-language', 'english']));
  Assert.IsFalse(k.CheckParams(['template', '-id-language', '$$$']));

  Found := False;
  k.OnOutputText :=
    procedure(s: string) begin
      Log(TLogLevel.Information, s);
      Assert.AreEqual(s, 'Warning: -id-language: ''en-us'' is a valid tag but is not canonical: it should be ''en''');
      Found := True;
    end;
  Assert.IsTrue(k.CheckParams(['template', '-id-language', 'en-us']));
  Assert.IsTrue(Found);
end;

procedure TKMConvertParametersTest.TestModelIdUniq;
var
  k: TKMConvertParameters;
begin
  k.OnOutputText := procedure(s: string) begin Log(TLogLevel.Information, s); end;
  Assert.IsTrue(k.CheckParams(['template', '-id-uniq', 'myself']));
  Assert.IsTrue(k.CheckParams(['template', '-id-uniq', 'my_self']));
  Assert.IsFalse(k.CheckParams(['template', '-id-uniq', 'my-self']));
  Assert.IsFalse(k.CheckParams(['template', '-id-uniq', 'Myself']));
  Assert.IsFalse(k.CheckParams(['template', '-id-uniq', '$myself$']));
end;

initialization
  TDUnitX.RegisterTestFixture(TKMConvertParametersTest);
end.
