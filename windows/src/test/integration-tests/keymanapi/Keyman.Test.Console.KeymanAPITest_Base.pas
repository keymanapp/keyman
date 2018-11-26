unit Keyman.Test.Console.KeymanAPITest_Base;

interface

uses
  DUnitX.TestFramework,
  keymanapi_tlb;

type
  TKeymanAPITest_Base = class
  strict private
    FTestPath: string;
    FK: IKeyman;
  protected
    procedure CheckKeyboardProperties(kbd: IKeymanKeyboard);
    procedure CheckPackageProperties(pkg: IKeymanPackage);
    property TestPath: string read FTestPath;
    property k: IKeyman read FK write FK;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Win.ComObj,
  System.SysUtils;

constructor TKeymanAPITest_Base.Create;
begin
  inherited Create;
  FTestPath := ExtractFilePath(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + 'test\';
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
end;

destructor TKeymanAPITest_Base.Destroy;
begin
  CoUninitialize;
  inherited Destroy;
end;

procedure TKeymanAPITest_Base.CheckKeyboardProperties(kbd: IKeymanKeyboard);
begin
  Assert.AreEqual('กขฃ', kbd.GetCharsUsed);
  Assert.AreEqual('(C) 2017 SIL International', kbd.Copyright);
  Assert.AreEqual('en', kbd.DefaultBCP47Languages);
  Assert.AreEqual($040C, kbd.DefaultPrimaryLanguage);
  Assert.AreEqual('x040C x040C', kbd.DefaultWindowsLanguages);
//  Assert.IsNotNull(f.DefaultHotkey);  TODO: Resolve DefaultHotkey implementation -- remove from reg, read from kmx in --installed, read from kmx in --file as well
//  Assert.AreEqual(False, f.DefaultHotkey.IsEmpty);
//  Assert.AreEqual(HK_ALT or HK_CTRL, f.DefaultHotkey.Modifiers);
//  Assert.AreEqual(Ord('T'), f.DefaultHotkey.VirtualKey);
//  Assert.AreEqual(0, f.DefaultHotkey.RawValue);
//  Assert.AreEqual(0, f.DefaultHotkey.Target);
  Assert.AreEqual(keUnicode, kbd.Encodings);
  Assert.AreEqual('test.kmx', ExtractFileName(kbd.Filename));
  Assert.AreEqual('test', kbd.ID);
  Assert.AreEqual(kltPositional, kbd.LayoutType);
  Assert.AreEqual('Test Message', kbd.Message);
  Assert.AreEqual('Test Keyboard', kbd.Name);
  Assert.AreEqual('1.2.3', kbd.Version);
  Assert.IsNotNull(kbd.Bitmap);
  //  Assert.AreEqual(f.SerializeXML(...), False);
end;

procedure TKeymanAPITest_Base.CheckPackageProperties(pkg: IKeymanPackage);
begin
  Assert.AreEqual('Keyman', pkg.Author);
  Assert.AreEqual('mailto:support@keyman.com', pkg.AuthorEmail);
  Assert.AreEqual('(C) 2017 SIL International', pkg.Copyright);

  Assert.IsNotNull(pkg.Files);
  // TODO: Check each file

  Assert.IsNotNull(pkg.Fonts);
  // TODO: Check each font

  Assert.IsNull(pkg.Graphic);
  Assert.IsNull(pkg.GraphicFile);
  Assert.AreEqual('test', pkg.ID);
  Assert.IsNull(pkg.KeyboardOptionsFile);

  Assert.IsNotNull(pkg.Keyboards);
  Assert.AreEqual(1, pkg.Keyboards.Count);

  Assert.AreEqual('Test Keyboard', pkg.Name);

  Assert.IsNotNull(pkg.ReadmeFile);
  Assert.AreEqual('readme.txt', ExtractFileName(pkg.ReadmeFile.Filename));

  Assert.IsNull(pkg.UsageFile);
  Assert.AreEqual('1.2.3', pkg.Version);

  Assert.IsNotNull(pkg.WelcomeFile);
  Assert.AreEqual('welcome.htm', ExtractFileName(pkg.WelcomeFile.Filename));

  Assert.AreEqual('https://keyman.com/', pkg.Website);
end;

end.
