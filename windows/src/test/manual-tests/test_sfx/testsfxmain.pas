(*
  Name:             testsfxmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jul 2008

  Modified Date:    20 Jul 2008
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jul 2008 - mcdurdin - Initial version
*)
unit testsfxmain;

interface

procedure Run;

implementation

uses
  sfxgbls,
  SFXstrings,
  SFXmisc,
  SysUtils,
  tntWindows,
  tntsystem,
  windows;

procedure Run;
begin
  if ParamCount < 2 then
  begin
    writeln('Usage: testsfx <filename.zip|filename.exe> <outpath>');
    Exit;
  end;

  try
    (* Open the archive *)
    InFile := Tnt_CreateFileW(PWideChar(WideParamStr(1)),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         NIL,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0 );

    (* If error, notify and abort *)
    IF InFile = INVALID_HANDLE_VALUE THEN
    BEGIN
      writeln('Invalid file');
      EXIT;
    END;

    try
      ExtPath := IncludeTrailingPathDelimiter(ParamStr(2));
      ForceDirectories(ExtPath);

      New( CRC32Table );
      Make_CRC32Table;

      TRY
        FileLength := GetFileSize(InFile, nil);

        { Display the dialog }

        TotalBytes := 0;

        if not ProcessArchive(0, False) then
        begin
          writeln('Failed to extract files');
        end
        else
          writeln('Success extracting files');
      FINALLY
        { Close the archive }
        Dispose(CRC32Table);
      END;
    finally
      ExitCode := 1;
      CloseHandle(InFile);
    end;
  except
    on E:Exception do
    begin
      writeln(E.Message);
      raise;
    end;
  end;
end;

end.
