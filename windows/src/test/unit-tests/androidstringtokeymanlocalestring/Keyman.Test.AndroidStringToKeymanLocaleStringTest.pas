unit Keyman.Test.AndroidStringToKeymanLocaleStringTest;

interface

uses
  DUnitX.TestFramework,
  Keyman.System.AndroidStringToKeymanLocaleString;

type
  [TestFixture]
  TAndroidStringToKeymanLocaleStringTest = class(TObject)
  public
    [Test]
    procedure TestTransform;
  end;

implementation

{ TAndroidStringToKeymanLocaleStringTest }

procedure TAndroidStringToKeymanLocaleStringTest.TestTransform;
begin
  Assert.AreEqual('', TAndroidStringToKeymanLocaleString.Transform(''));
  Assert.AreEqual('text', TAndroidStringToKeymanLocaleString.Transform('text'));
  Assert.AreEqual('text', TAndroidStringToKeymanLocaleString.Transform('"text"'));
  Assert.AreEqual('te"xt', TAndroidStringToKeymanLocaleString.Transform('te\"xt'));
  Assert.AreEqual('te'#13#10'xt', TAndroidStringToKeymanLocaleString.Transform('te\nxt'));
  Assert.AreEqual('1\2?3@4', TAndroidStringToKeymanLocaleString.Transform('1\\2\?3\@4'));
  Assert.AreEqual('%2:s text %0:d %1:s', TAndroidStringToKeymanLocaleString.Transform('%3$s text %1$d %2$s'));
  Assert.AreEqual('%2s text %0:d %1:s', TAndroidStringToKeymanLocaleString.Transform('%2s text %1$d %2$s'));
  Assert.AreEqual('te''xt', TAndroidStringToKeymanLocaleString.Transform('te\''xt'));
  Assert.AreEqual(' text', TAndroidStringToKeymanLocaleString.Transform('" text"'));
  Assert.AreEqual('  text  ', TAndroidStringToKeymanLocaleString.Transform('"  text  "'));
  Assert.AreEqual('text', TAndroidStringToKeymanLocaleString.Transform(' text   '));
end;

end.
