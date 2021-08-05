unit Keyman.Test.RegExGroupHelperRSP19902Test;

interface

//
// Ref: https://quality.embarcadero.com/browse/RSP-19902
//
// This unit test tests the RSP-19902 workaround and ensures that if the code is
// in the Embarcadero libraries, that the unit test will fail, so we will be able
// to proactively resolve it without sending out an accidentally broken version.
//
// Fixed in VER330 (Delphi 10.3 Rio, 20.0)

uses
  DUnitX.TestFramework,
  Keyman.System.RegExGroupHelperRSP19902;

const
  Chakma1 = Char($D804)+Char($DD37);
  Chakma2 = Char($D804)+Char($DD38);
  Chakma3 = Char($D804)+Char($DD39);

type
  [TestFixture]
  TGroupHelperRSP19902Test = class(TObject)
  private
    procedure RunBasicRegEx(s,r,x: string; ShouldPassBuiltinTest: Boolean = False);
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure TestNoSMP;

    [Test]
    procedure TestDoublePrefixSMPAndSingleMatchedSMP;

    [Test]
    procedure TestDoublePrefixSMPCharacter;

    [Test]
    procedure TestSinglePrefixSMPAndSingleMatchedSMP;

    [Test]
    procedure TestSinglePrefixSMPCharacter;

    [Test]
    procedure TestSMPAtBeginningOfInput;

    [Test]
    procedure TestSMPAtEndOfInput;

    [Test]
    procedure TestSMPAtEndOfMatch;

    [Test]
    procedure TestSMPAtStartOfMatch;

    [Test]
    procedure TestSMPInMiddle;

    [Test]
    procedure TestSMPMatchWholeInput;

    [Test]
    procedure TestSMPOnlyMatch;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

procedure TGroupHelperRSP19902Test.RunBasicRegEx(s, r, x: string; ShouldPassBuiltinTest: Boolean);
var
  m: TMatch;
  g: TGroupHelperRSP19902;
begin
  m := TRegEx.Match(s, r);
  Assert.IsTrue(m.Success);
  writeln(Format('FAILED:(%s) at position %d (length %d): %s', ['x', m.Groups[1].Index, m.Groups[1].Length, m.Groups[1].Value]));
{$IFDEF VER320}
  if ShouldPassBuiltinTest
    then Assert.AreEqual(x, m.Groups[1].Value)
    else Assert.AreNotEqual(x, m.Groups[1].Value);
{$ELSE}
  // Fixed in VER330
  Assert.AreEqual(x, m.Groups[1].Value);
{$ENDIF}

  g := TGroupHelperRSP19902.Create(m.Groups[1], s);
  writeln(Format('PASSED:(%s) at position %d (length %d): %s [expected %s]', ['x', g.FixedIndex, g.FixedLength, g.FixedValue, x]));
  Assert.AreEqual(x, g.FixedValue);
end;

procedure TGroupHelperRSP19902Test.TestNoSMP;
begin
  RunBasicRegEx('abcdefg',           '(c.e)', 'cde', True);
end;

procedure TGroupHelperRSP19902Test.TestSinglePrefixSMPCharacter;
begin
  RunBasicRegEx(Chakma1+'bcdefg',           '(c.e)', 'cde');
end;

procedure TGroupHelperRSP19902Test.TestDoublePrefixSMPCharacter;
begin
  RunBasicRegEx(Chakma1+Chakma2+'bcdefg',   '(c.e)', 'cde');
end;

procedure TGroupHelperRSP19902Test.TestSinglePrefixSMPAndSingleMatchedSMP;
begin
  RunBasicRegEx(Chakma1+'bc'+Chakma2+'efg', '(c.e)', 'c'+Chakma2+'e');
end;

procedure TGroupHelperRSP19902Test.Setup;
begin
  Assert.IgnoreCaseDefault := False;
end;

procedure TGroupHelperRSP19902Test.TestDoublePrefixSMPAndSingleMatchedSMP;
begin
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+'efg',   '(c.e)', 'c'+Chakma2+'e');
end;

procedure TGroupHelperRSP19902Test.TestSMPInMiddle;
begin
  // smp match in middle
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3+'fg', '(c.'+Chakma3+')', 'c'+Chakma2+Chakma3);
end;

procedure TGroupHelperRSP19902Test.TestSMPAtStartOfMatch;
begin
  // smp at start of match
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3+'fg', '('+Chakma2+Chakma3+'f)', Chakma2+Chakma3+'f');
end;

procedure TGroupHelperRSP19902Test.TestSMPOnlyMatch;
begin
  // smp-only match
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3+'fg', '('+Chakma2+Chakma3+')', Chakma2+Chakma3);
end;

procedure TGroupHelperRSP19902Test.TestSMPAtEndOfMatch;
begin
  // smp at end of match
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3+'fg', '(c'+Chakma2+Chakma3+')', 'c'+Chakma2+Chakma3);
end;

procedure TGroupHelperRSP19902Test.TestSMPAtEndOfInput;
begin
  // match at end of string
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3, '('+Chakma2+Chakma3+')', Chakma2+Chakma3);
end;

procedure TGroupHelperRSP19902Test.TestSMPAtBeginningOfInput;
begin
  // match at beginning of string
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3+'fg', '('+Chakma1+Chakma2+')', Chakma1+Chakma2);
end;

procedure TGroupHelperRSP19902Test.TestSMPMatchWholeInput;
begin
  // match whole string
  RunBasicRegEx(Chakma1+Chakma2+'bc'+Chakma2+Chakma3, '('+Chakma1+Chakma2+'...'+Chakma3+')', Chakma1+Chakma2+'bc'+Chakma2+Chakma3);
end;

initialization
  TDUnitX.RegisterTestFixture(TGroupHelperRSP19902Test);
end.
