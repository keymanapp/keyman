unit Keyman.System.Test.InstallDefaultUILanguageTest;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TInstallDefaultUILanguageTest = class(TObject)
  public
    [Test]
    procedure TestFind;
  end;

implementation

uses
  Keyman.Configuration.System.InstallDefaultUILanguage;

{ TInstallDefaultUILanguageTest }

procedure TInstallDefaultUILanguageTest.TestFind;
var
  ktags, utags: TStringList;
begin
  ktags := TStringList.Create;
  utags := TStringList.Create;

  ktags.CommaText := 'en,de,fr,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  utags.CommaText := 'en-US';
  Assert.AreEqual('en', TInstallDefaultUILanguage.Find(ktags, utags));

  ktags.CommaText := 'en,en-au,de,fr,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  Assert.AreEqual('en', TInstallDefaultUILanguage.Find(ktags, utags));

  utags.CommaText := 'en-AU';
  Assert.AreEqual('en-au', TInstallDefaultUILanguage.Find(ktags, utags));

  utags.CommaText := 'kan';
  Assert.AreEqual('kan-knda-in', TInstallDefaultUILanguage.Find(ktags, utags));

  ktags.CommaText := 'en,en-au,de,fr,km,kan-knda-in,my,pt-br,qqq,ta,tr,vec';
  utags.CommaText := 'jar,km-KH';
  Assert.AreEqual('km', TInstallDefaultUILanguage.Find(ktags, utags));

  ktags.Free;
  utags.Free;
end;

end.
