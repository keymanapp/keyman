unit Keyman.System.Analyze.Main;

interface

procedure Run;

implementation

uses
  kmxfile;

procedure Run;
var
  ki: TKeyboardInfo;
  pkfh: PKeyboardFileHeader;
begin
  if ParamCount < 2 then
  begin
    writeln('Usage: kmanalyze <infile.kmx>');
    Exit;
  end;

  GetKeyboardInfo(ParamStr(1), True, ki);

  //first version: use only first character from each store.
  pkfh := PKeyboardFileHeader(ki.MemoryDump.Memory);
  gp :=
end;



end.
