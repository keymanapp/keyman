unit Keyman.Test.System.CompilePackageVersioningTest;

interface

uses
  DUnitX.TestFramework,
  Keyman.Developer.System.Project.ProjectLog;

type

  [TestFixture]
  TCompilePackageVersioningTest = class(TObject)
  private
    FRoot: string;
    procedure PackageMessage(Sender: TObject; msg: string;
      State: TProjectLogState);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('test-single-version-1-package', 'test1.kps,False')]
    [TestCase('test-single-version-2-package', 'test2.kps,False')]
    [TestCase('test-version-2-1-mismatch', 'test2-1.kps,True')]
    [TestCase('test-version-1-2-mismatch', 'test1-2.kps,True')]
    [TestCase('test-keyboard-1-package-2', 'test-keyboard-1-vs-package-2.kps,False')]
    [TestCase('test-package-1-keyboard-2', 'test-package-1-vs-keyboard-2.kps,False')]
    procedure TestPackageCompile(Path: string; ExpectError: Boolean);
  end;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX,

  compile,
  CompilePackage,
  Keyman.Developer.System.Project.kmnProjectFileAction,
  Keyman.Developer.System.Project.ProjectFile,
  kpsfile;

type
  TProjectConsole = class(TProject)
  private
    FSilent: Boolean;
    FFullySilent: Boolean;
  public
    procedure Log(AState: TProjectLogState; Filename: string; Msg: string; MsgCode, line: Integer); override;   // I4706
    function Save: Boolean; override;   // I4709

    property Silent: Boolean read FSilent write FSilent;
    property FullySilent: Boolean read FFullySilent write FFullySilent;
  end;

procedure TCompilePackageVersioningTest.Setup;
var
  p: TProjectConsole;
  i: Integer;
begin
  Assert.IgnoreCaseDefault := False;

  FRoot := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))));

  p := TProjectConsole.Create(ptUnknown, FRoot+'\test-1.0\test-1.0.kpj', False);
  try
    for i := 0 to p.Files.Count - 1 do
      if p.Files[i] is TkmnProjectFileAction then
        Assert.IsTrue((p.Files[i] as TkmnProjectFileAction).CompileKeyboard, 'Could not compile keyboard');
  finally
    p.Free;
  end;

  p := TProjectConsole.Create(ptUnknown, FRoot+'\test-2.0\test-2.0.kpj', False);
  try
    for i := 0 to p.Files.Count - 1 do
      if p.Files[i] is TkmnProjectFileAction then
        Assert.IsTrue((p.Files[i] as TkmnProjectFileAction).CompileKeyboard, 'Could not compile keyboard');
  finally
    p.Free;
  end;
end;

procedure TCompilePackageVersioningTest.TearDown;
begin
end;

procedure TCompilePackageVersioningTest.PackageMessage(Sender: TObject; msg: string; State: TProjectLogState);
const
  Map: array[TProjectLogState] of TLogLevel = (
    {plsInfo}    TLogLevel.Information,
    {plsHint}    TLogLevel.Information,
    {plsWarning} TLogLevel.Warning,
    {plsError}   TLogLevel.Error,
    {plsFatal}   TLogLevel.Error,
    {plsSuccess} TLogLevel.Information,
    {plsFailure} TLogLevel.Error
  );
begin
  Log(Map[State], msg);
end;

procedure TCompilePackageVersioningTest.TestPackageCompile(Path: string; ExpectError: Boolean);
var
  pack: TKPSFile;
  res: Boolean;
begin
  pack := TKPSFile.Create;
  try
    pack.FileName := FRoot+'\'+Path;
    pack.LoadXML;
    res := DoCompilePackage(pack, PackageMessage, False, False, FRoot+'\'+ChangeFileExt(Path,'.kmp'));
    Assert.AreEqual(not ExpectError, res);
  finally
    pack.Free;
  end;
end;

procedure TProjectConsole.Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer);   // I4706
begin
{$IFDEF DEBUG_PROJECT}
  case AState of
    plsInfo,
    plsSuccess,
    plsFailure:
      if not FSilent then
        writeln(ExtractFileName(Filename)+': '+Msg);
    plsWarning:
      if not FFullySilent then
        writeln(ExtractFileName(Filename)+': Warning: '+Msg);
    plsError:
      if not FFullySilent then
        writeln(ExtractFileName(Filename)+': Error: '+Msg);
    plsFatal:
      writeln(ExtractFileName(Filename)+': Fatal error: '+Msg);
  end;
{$ENDIF}
end;

function TProjectConsole.Save: Boolean;   // I4709
begin
  // We don't modify the project file in the console
  Result := True;
end;

initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  TDUnitX.RegisterTestFixture(TCompilePackageVersioningTest);
finalization
  CoUninitialize;
end.
