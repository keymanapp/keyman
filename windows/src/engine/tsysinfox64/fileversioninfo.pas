(*
  Name:             fileversioninfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Get file version information from a dll or exe
  Create Date:      13 May 2005

  Modified Date:    13 May 2005
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
*)
unit fileversioninfo;

interface

uses Windows, SysUtils;

type
  TFileVersionInfo = class
  private
    FFileName: string;
    pbuf: PChar;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function StringValue(const Name: string): string;
    property FileName: string read FFileName;
  end;

implementation

{ TFileVersionInfo }

constructor TFileVersionInfo.Create(const AFileName: string);
var
  dw, dwHandle: DWord;
begin
  FFileName := AFileName;
  dw := GetFileVersionInfoSize(PChar(AFileName), dwHandle);
  if dw = 0 then Exit;

  pbuf := AllocMem(dw);
  GetFileVersionInfo(PChar(AFileName), dwHandle, dw, pbuf);
end;

destructor TFileVersionInfo.Destroy;
begin
  FreeMem(pbuf);
  inherited Destroy;
end;

function TFileVersionInfo.StringValue(const Name: string): string;
var
  lpTranslate, lpBuffer: Pointer;
  cbTRanslate, dwBytes: DWord;
  pdw: PDWord;
  s: string;
begin
  Result := '';
  try
    if not Assigned(pbuf) then Result := ''
    else
    begin
      if VerQueryValue(pbuf,'\VarFileInfo\Translation', lpTranslate, cbTranslate) then
      begin
        pdw := lpTranslate;
        if cbTranslate >= 4 then
        begin
          s := Format('\StringFileInfo\%.04x%.04x\'+Name, [LOWORD(pdw^), HIWORD(pdw^)]);
          if VerQueryValue(pbuf, PChar(s), lpBuffer, dwBytes) then
            Result := PChar(lpBuffer);
        end;
      end;
    end;
  except
    on E:Exception do Result := E.Message;
  end;
end;

end.
 
