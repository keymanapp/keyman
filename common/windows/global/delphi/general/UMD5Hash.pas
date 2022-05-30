(*
  Name:             UMD5Hash
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 May 2015

  Modified Date:    11 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 May 2015 - mcdurdin - I4709 - V9.0 - Use static hashing for id for project files to avoid unnecessary changes
                    
*)
unit UMD5Hash;   // I4709

interface

function MD5Hash(Data: string): string;

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  dcpmd5;

function MD5Hash(Data: string): string;
  function BigEndian(n: DWord): DWord;
  begin
    Result := (n and $FF) shl 24 or
              (n and $FF00) shl 8 or
              (n and $FF0000) shr 8 or
              (n and $FF000000) shr 24;
  end;
var
  buf: ansistring;
  Digest: array[0..3] of DWord;
begin
  with TDCP_md5.Create(nil) do
  try
    Init;
    buf := AnsiString(Data);  // I3310
    Update(PAnsiChar(buf)^, Length(buf));  // I3310
    Final(Digest);
    Burn;
  finally
    Free;
  end;

  Result := LowerCase(Format('%0.8x%0.8x%0.8x%0.8x', [BigEndian(Digest[0]), BigEndian(Digest[1]), BigEndian(Digest[2]), BigEndian(Digest[3])]));
end;

end.
