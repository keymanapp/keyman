unit DUnitX.Loggers.TeamCity;

interface

{I DUnitX.inc}

uses
  DUnitX.ConsoleWriter.Base,
  DUnitX.TestFramework;

type
  ///
  ///  Writes messages to the console window formatted for TeamCity
  ///
  TDUnitXTeamCityLogger = class(TInterfacedObject, ITestLogger)
  private
    FConsoleWriter : IDUnitXConsoleWriter;
    procedure WriteTeamCityTestFailure(error: ITestError);
    procedure WriteTeamCityLog(const message: string);
  protected
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);

    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnTestMemoryLeak(const threadId : TThreadID; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: TThreadID; const  AIgnored: ITestResult);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure OnLog(const logType : TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);

    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);

    procedure OnTestingEnds(const RunResults: IRunResults);
  public
    constructor Create;
  end;

function CreateDUnitXTeamCityOrConsoleLogger: ITestLogger;
procedure ReportToTeamCity;

implementation

uses
  DUnitX.ResStrs,
  System.SysUtils,
  DUnitX.AutoDetect.Console,
  DUnitX.Loggers.Console,
  DUnitX.IoC;

{ TDUnitXTeamCityLogger }

constructor TDUnitXTeamCityLogger.Create;
begin
  FConsoleWriter := TDUnitXIoC.DefaultContainer.Resolve<IDUnitXConsoleWriter>();
  if FConsoleWriter = nil then
    raise Exception.Create(SNoConsoleWriterClassRegistered);
end;

procedure TDUnitXTeamCityLogger.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnEndTeardownTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
end;

procedure TDUnitXTeamCityLogger.WriteTeamCityLog(const message: string);
begin
  FConsoleWriter.WriteLn('##teamcity['+message+']');
end;

procedure TDUnitXTeamCityLogger.OnEndTest(const threadId: TThreadID; const  Test: ITestResult);
begin
  WriteTeamCityLog(Format('testFinished name=''%s''',[Test.Test.FullName]));
end;

procedure TDUnitXTeamCityLogger.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin
  WriteTeamCityLog(Format('testSuiteFinished name=''%s''',[results.Name]));
end;

procedure TDUnitXTeamCityLogger.OnExecuteTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
end;

procedure TDUnitXTeamCityLogger.WriteTeamCityTestFailure(error: ITestError);
begin
  WriteTeamCityLog(Format('testFailed type=''%s'' name=''%s'' ' + 'message=''%s'' details=''%s''',
    [error.ExceptionClass.ClassName,
    error.Test.FullName,
    error.ExceptionMessage,
    error.ExceptionLocationInfo
    ]));
end;

procedure TDUnitXTeamCityLogger.OnTestError(const threadId: TThreadID; const  Error: ITestError);
begin
  WriteTeamCityTestFailure(Error);
end;

procedure TDUnitXTeamCityLogger.OnTestFailure(const threadId: TThreadID; const  Failure: ITestError);
begin
  WriteTeamCityTestFailure(Failure);
end;

procedure TDUnitXTeamCityLogger.OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
begin
  WriteTeamCityLog(Format('testIgnored name=''%s'' message=''%s''', [AIgnored.Test.FullName, AIgnored.Message]));
end;

procedure TDUnitXTeamCityLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
  FConsoleWriter.Indent(2);
  try
    {case logType  of
      TLogLevel.Information: SetConsoleDefaultColor();
      TLogLevel.Warning: SetConsoleWarningColor();
      TLogLevel.Error: SetConsoleErrorColor();
    end;}
    FConsoleWriter.WriteLn(msg);
  finally
    FConsoleWriter.Outdent(2);
  end;
end;

procedure TDUnitXTeamCityLogger.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnSetupTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnBeginTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
  WriteTeamCityLog(Format('testStarted name=''%s''',[Test.FullName]));
end;

procedure TDUnitXTeamCityLogger.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  WriteTeamCityLog(Format('testSuiteStarted name=''%s''',[fixture.FullName]));
end;

procedure TDUnitXTeamCityLogger.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin
end;

procedure TDUnitXTeamCityLogger.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
end;

procedure TDUnitXTeamCityLogger.OnTestingEnds(const RunResults: IRunResults);
var
  testResult: ITestResult;
begin
  FConsoleWriter.WriteLn(Format(STestsFound, [RunResults.TestCount]));
  FConsoleWriter.WriteLn(Format(STestsIgnored, [RunResults.IgnoredCount]));
  FConsoleWriter.WriteLn(Format(STestsPassed, [RunResults.PassCount]));
  FConsoleWriter.WriteLn(Format(STestsLeaked, [RunResults.MemoryLeakCount]));
  FConsoleWriter.WriteLn(Format(STestsFailed, [RunResults.FailureCount]));
  FConsoleWriter.WriteLn(Format(STestsErrored,[RunResults.ErrorCount]));

  if RunResults.FailureCount > 0  then
  begin
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(SFailingTests);
    FConsoleWriter.WriteLn;

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Failure then
      begin
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.ErrorCount > 0  then
  begin
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(STestsWithErrors);
    FConsoleWriter.WriteLn;

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Error then
      begin
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.MemoryLeakCount > 0  then
  begin
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(STestsWithLeak);
    FConsoleWriter.WriteLn;

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.MemoryLeak then
      begin
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;
end;

procedure TDUnitXTeamCityLogger.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount : Cardinal);
begin
end;

procedure TDUnitXTeamCityLogger.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
begin
  WriteTeamCityLog(Format('testFailed name=''%s'' message=''Memory leak'' details=''%s''',
    [Test.Test.FullName, Test.Message]));
end;


function CreateDUnitXTeamCityOrConsoleLogger: ITestLogger;
begin
//  if GetEnvironmentVariable('TEAMCITY_VERSION') <> ''
//    then Result := TDUnitXTeamCityLogger.Create
//    else
  Result := TDUnitXConsoleLogger.Create(true);
end;

procedure ReportToTeamCity;
var
  KeymanRoot: string;
  ReportPath: string;
begin
  if GetEnvironmentVariable('TEAMCITY_VERSION') <> '' then
  begin
    KeymanRoot := GetEnvironmentVariable('KEYMAN_ROOT');
    ReportPath := 'keyman\'+ExtractRelativePath(KeymanRoot, ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml');
    writeln('##teamcity[importData type=''nunit'' path='''+ReportPath+''']');
  end;
end;

initialization
finalization
  ReportToTeamCity;
end.
