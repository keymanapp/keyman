unit inffilereader;

interface

uses
  IniFiles;

procedure Run;

implementation

procedure Run;
begin
  with TIniFile.Create('c:\temp\foo.ini') do
  try
    writeln('<'+ReadString('foo', 'bar', 'baz')+'>');
  finally
    Free;
  end;
end;

end.
