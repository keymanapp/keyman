unit Keyman.Developer.System.KeymanDeveloperPaths;

interface

uses
  System.SysUtils,

  KeymanPaths;

type
  TKeymanDeveloperPaths = class sealed
  private
  public
    const S_LexicalModelCompiler = 'kmlmc.cmd';
    class function LexicalModelCompilerPath: string; static;
  end;

implementation

{ TKeymanDeveloperPaths }

class function TKeymanDeveloperPaths.LexicalModelCompilerPath: string;
var
  KeymanRoot: string;
begin
  if TKeymanPaths.RunningFromSource(KeymanRoot)
    then Result := KeymanRoot + 'windows\src\developer\tike\'
    else Result := ExtractFilePath(ParamStr(0));

  Result := Result + S_LexicalModelCompiler;
end;

end.
