unit Keyman.System.Test.JsonUtilTest;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TJsonUtilTest = class(TObject)
  private
    function DateTimeString(d: TDateTime): string;
  public
    [Test]
    procedure TestJsonDateToDateTime;

    [Test]
    procedure TestDateTimeToJsonDate;
  end;

implementation

uses
  System.DateUtils,
  JsonUtil,
  Soap.XsBuiltIns;

// We moved from using Soap.XsBuiltIns to DateToISO8601 in order to avoid having
// a massive dependency on Soap units. This validates that the changeover is
// identical.
function SoapBasedJSONDateToDateTime(const Value: string; var DateTime: TDateTime): Boolean;
begin
  try
    with TXSDateTime.Create do
    try
      XSToNative(Value);
      DateTime := AsDateTime;
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

function SoapBasedDateTimeToJSONDate(const ADateTime: TDateTime): string;
begin
  try
    with TXSDateTime.Create do
    try
      AsDateTime := ADateTime;
      Result := NativeToXS;
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

procedure TJsonUtilTest.TestDateTimeToJsonDate;
const
  tests: array[0..2] of string = (
    '2020-06-29',
    '2020-06-12T00:36:29',
    '2020-06-12T00:36:29.888Z'
  );
var
  test: string;
  dt: TDateTime;
  sSoap, sOurs: string;
begin
  for test in tests do
  begin
    Assert.IsTrue(SoapBasedJSONDateToDateTime(test, dt));
    sSoap := SoapBasedDateTimeToJSONDate(dt);
    sOurs := DateTimeToJSONDate(dt);
    Assert.AreEqual(sSoap, sOurs);
  end;
end;

procedure TJsonUtilTest.TestJsonDateToDateTime;
var
  dtOurs, dtSoap: TDateTime;
  test, sOurs, sSoap: string;
const
  tests: array[0..2] of string = (
    '2020-06-29',
    '2020-06-12T00:36:29',
    '2020-06-12T00:36:29.888Z'
  );
begin
  for test in tests do
  begin
    Assert.IsTrue(SoapBasedJSONDateToDateTime(test, dtSoap));
    Assert.IsTrue(JSONDateToDateTime(test, dtOurs));

    sOurs := DateTimeString(dtOurs);
    sSoap := DateTimeString(dtSoap);

    Assert.AreEqual(sSoap, sOurs);
  end;
end;

function TJsonUtilTest.DateTimeString(d: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd''T''hh:nn:ss.zzz', d);
end;

initialization
  TDUnitX.RegisterTestFixture(TJsonUtilTest);
end.
