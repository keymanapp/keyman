unit Keyman.Developer.System.LexicalModelCompile;

interface

uses
  Keyman.Developer.System.Project.ProjectFile;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;

implementation

uses
  Keyman.Developer.System.KmcWrapper;

function CompileModelFile(ProjectFile: TProjectFile; infile, outfile: string; debug: Boolean): Boolean;
var
  w: TKmcWrapper;
begin
  w := TKmcWrapper.Create;
  try
    Result := w.Compile(ProjectFile, infile, outfile, debug);
  finally
    w.Free;
  end;
end;

end.
