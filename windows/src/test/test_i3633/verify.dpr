program verify;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.DateUtils,
  System.SysUtils;

var
  FTestDate: Boolean;
  FVersion: string;

function ProcessLine(s: string): string;
var
  str: array[0..7] of string;
  I: Integer;
  n: Integer;
  FIsSetup: Boolean;
  FDate: TDateTime;
begin
  if Trim(s) = '' then Exit('');  // Ignore blank lines
  if Copy(s,1,4) = 'Path' then Exit(''); // Ignore sigcheck header line

  { split string into array }
  for I := 0 to 7 do
  begin
    if Copy(s,1,1) <> '"' then Exit('Unrecognised sigcheck output');
    Delete(s,1,1);
    n := Pos('"', s);
    if n = 0 then Exit('Unrecognised sigcheck output');
    str[I] := Copy(s, 1, n-1);
    Delete(s,1,n);
    if (I < 7) and (Copy(s,1,1) <> ',') then Exit('Unrecognised sigcheck output');
    Delete(s,1,1);
  end;

  { Check each element of string }
  if str[1] <> 'Signed' then
  begin
    FIsSetup := Pos('setup.exe', LowerCase(str[0])) > 0;
    if not FIsSetup then // setup.exe will not be signed because we sign after appending the zip data
    begin
      Exit('Unsigned executable');
    end;
  end;

  if FTestDate then
  begin
    n := Pos('M ', str[2])+1;
    if n < 2 then Exit('Unrecognised date format');
    FDate := StrToTime(Copy(str[2],1,n-1))+StrToDate(Copy(str[2],n+1,MAXINT));
    if DaysBetween(Now, FDate) > 1 then
    begin
      Exit('File was signed more than 1 day ago.');
    end;
  end;

  if str[3] <> 'SIL International' then
  begin
    Exit('File has wrong company name');
  end;

  if str[6] <> str[7] then
  begin
    Exit('File has mismatching versions');
  end;

  if str[6] <> FVersion then
  begin
    Exit('File does not have version '+FVersion);
  end;

  Result := '';
end;

var
  FVersionText, FError, ss: string;
  FVersionTextFile: TextFile;
  HaltCode: Integer;
begin
  try
    HaltCode := 0;

    if (ParamStr(1) = '-?') or (ParamCount < 1) then
    begin
      writeln('verify [-d] version.txt: Verify the output of sigcheck to ensure all executables are signed and have proper version.');
      writeln('  -d: Check the timestamp on the signature is less than 2 days old.');
      writeln('  version.txt: path to the version to verify against');
      Halt(2);
    end;

    FTestDate := ParamStr(1) = '-d';
    if FTestDate then FVersionText := ParamStr(2) else FVersionText := ParamStr(1);

    { Load the version.txt file }

    AssignFile(FVersionTextFile, FVersionText);
    Reset(FVersionTextFile);
    Readln(FVersionTextFile, FVersion);
    Close(FVersionTextFile);

    FVersion := Trim(FVersion);
    if Copy(FVersion, 1, Length('PRODUCTVERSION ')) <> 'PRODUCTVERSION ' then
    begin
      writeln('Invalid version.txt file format.  Expecting PRODUCTVERSION 1,2,3,4');
      Halt(3);
    end;
    Delete(FVersion, 1, Length('PRODUCTVERSION '));
    FVersion := StringReplace(FVersion, ',', '.', [rfReplaceAll]);

    { Process the sigcheck response }

    while not Eof do
    begin
      Readln(ss);
      FError := ProcessLine(ss);
      if FError <> '' then
      begin
        writeln(FError+': '+ss);
        HaltCode := 1;
      end;
    end;
    Halt(HaltCode);
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
