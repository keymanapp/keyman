unit kccompilekvk;

interface

function CompileVisualKeyboardFromKMX(FInFile, FOutFile: string): Boolean;

implementation

uses
  System.Classes,
  System.SysUtils,

  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.ProjectLogConsole,
  KeyboardParser,
  kmxfile,
  kmxfileconsts,
  main,
  utilsystem,
  VisualKeyboard;

const
  CWARN_KVKFileIsInSourceFormat = $000020A2; // from kmn_compiler_errors.h

(**
  Compiles the visual keyboard from xml to binary, and/or copies to destination folder

  Parameters: FInFile     Source .kmn file name with path
              FOutFile    Destination .kmx file name with path

  Returns:    True on success

  The input FInFile references
  If the keyboard output is compiled to the same folder as the input, and a .kvk file
  is referenced in the .kmn file (instead of a .kvks file), then this function will
  take no action. This means that if the user has incorrectly saved a .kvk in XML,
  the output .kvk file will be in the wrong format for older versions of Keyman (there
  was a small window of time in May/June 2017 where .kvk files could be either XML or
  binary).
*)
function CompileVisualKeyboardFromKMX(FInFile, FOutFile: string): Boolean;
var
  FKVKInputFileName, FKVKOutputFileName: string;
begin
  try
    with TKeyboardParser.Create do   // I4720
    try
      LoadFromFile(FInFile);
      FKVKInputFileName := GetSystemStoreValue(ssVisualKeyboard);
      if FKVKInputFileName = '' then
      begin
        // Keyboard does not include a .kvk file
        Exit(True);
      end;

      FKVKInputFileName := ExpandFileNameClean(FInFile, FKVKInputFileName);
      FKVKOutputFileName := ExpandFileNameClean(FOutFile,
        ChangeFileExt(ExtractFileName(FKVKInputFileName), '.kvk'));

      with TVisualKeyboard.Create do
      try
        LoadFromFile(FKVKInputFileName);
        if not SameFilename(FKVKInputFileName, FKVKOutputFileName) then
        begin
          // Source and destination files are different
          SaveToFile(FKVKOutputFileName, kvksfBinary);
          TProjectLogConsole.Instance.Log(plsSuccess, FKVKInputFileName, 'Visual keyboard '+FKVKInputFileName+' successfully compiled to '+FKVKOutputFileName, 0, 0);
        end
        else
        begin
          if LoadedFileFormat = kvksfXML then
            TProjectLogConsole.Instance.Log(plsWarning, FKVKInputFileName, '.kvk file should be binary but is an XML file', CWARN_KVKFileIsInSourceFormat, 0)
          else
            TProjectLogConsole.Instance.Log(plsInfo, FKVKInputFileName, 'Visual keyboard '+FKVKInputFileName+' is valid', 0, 0);
        end;
      finally
        Free;
      end;
    finally
      Free;
    end;
    Result := True;
  except
    on E:EFCreateError do
    begin
      TProjectLogConsole.Instance.Log(plsFatal, FKVKInputFileName, E.Message, 0, 0);
      Exit(False);
    end;
    on E:EVisualKeyboardLoader do
    begin
      TProjectLogConsole.Instance.Log(plsFatal, FKVKInputFileName, E.Message, 0, 0);
      Exit(False);
    end;
  end;
end;

end.
