unit Keyman.System.Test.UILanguageManagerTest;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TUILanguageManagerTest = class(TObject)
  public
    [Test]
    procedure TestFind;
  end;

implementation

uses
  Keyman.System.UILanguageManager;

{ TUILanguageManagerTest }

procedure TUILanguageManagerTest.TestFind;
var
  ktags, utags: TStringList;
begin
  ktags := TStringList.Create;
  utags := TStringList.Create;

  ktags.CommaText := 'en,de,fr,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  utags.CommaText := 'en-US';
  Assert.AreEqual('en', TUILanguageManager.Find(ktags, utags));

  ktags.CommaText := 'en,en-au,de,fr,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  Assert.AreEqual('en', TUILanguageManager.Find(ktags, utags));

  utags.CommaText := 'en-AU';
  Assert.AreEqual('en-au', TUILanguageManager.Find(ktags, utags));

  utags.CommaText := 'kan';
  Assert.AreEqual('kan-knda-in', TUILanguageManager.Find(ktags, utags));

  ktags.CommaText := 'en,en-au,de,fr,km,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  utags.CommaText := 'jar,km-KH';
  Assert.AreEqual('km', TUILanguageManager.Find(ktags, utags));

  ktags.Free;
  utags.Free;
end;

end.
