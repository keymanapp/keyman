(*
  Name:             kmxfileusedchars
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    17 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit kmxfileusedchars;

interface

uses
  kmxfile;

function KMXFile_GetUsedChars(const FileName: WideString): WideString;

implementation

uses
  Classes,
  kmxfileconsts,
  kmxfileutils,
  SysUtils,
  Unicode;

function KMXFile_GetUsedChars(const FileName: WideString): WideString;
var
  ki: TKeyboardInfo;
  kfh: PKeyboardFileHeader;
  i: Integer;
  kfs: PKeyboardFileStore;
  kfg: PKeyboardFileGroup;
  j: Integer;
  kfk: PKeyboardFileKey;

  FChars: WideString;

      function KAddr(x: Cardinal): PByte;  // I3310
      begin
        Result := ki.MemoryDump.Memory;
        Inc(Result, x);
      end;

      procedure AddChars(x: Cardinal);
      var
        pwch, pwch2: PWideChar;
      begin
        if x = 0 then Exit;

        pwch := PWideChar(KAddr(x));
        while pwch^ <> #0 do
        begin
          pwch2 := incxstr(pwch);
          if pwch^ = WideChar(UC_SENTINEL) then
          begin
            while pwch < pwch2 do
            begin
              pwch^ := #32;
              Inc(pwch);
            end;
          end
          else
            pwch := pwch2;
        end;

        FChars := FChars + PWideChar(KAddr(x));
      end;

      procedure QuickSortChars(var Buf: array of Cardinal; L, R: Integer);
      var
        I, J, P: Integer;
        X: Cardinal;
      begin
        repeat
          I := L;
          J := R;
          P := (L + R) shr 1;
          repeat
            while Buf[I] < Buf[P] do Inc(I);
            while Buf[J] > Buf[P] do Dec(J);
            if I <= J then
            begin
              X := Buf[I]; Buf[I] := Buf[J]; Buf[J] := X;
              if P = I then
                P := J
              else if P = J then
                P := I;
              Inc(I);
              Dec(J);
            end;
          until I > J;
          if L < J then QuickSortChars(Buf, L, J);
          L := I;
        until I >= R;
      end;

      function SortChars(Chars: WideString): WideString;
      var
        I, J, P: Integer;
        LastBuf: Cardinal;
        Buf: array of Cardinal;
      begin
        I := 1; J := 0; P := Length(Chars);
        if P = 0 then
        begin
          Result := '';
          Exit;
        end;
        
        SetLength(Buf, P);
        while I <= Length(Chars) do
        begin
          if Uni_IsSurrogate1(Chars[I]) and (I < P) and Uni_IsSurrogate2(Chars[I+1]) then
          begin
            Buf[J] := Uni_SurrogateToUTF32(Chars[I], Chars[I+1]);
            Inc(I, 2);
          end
          else
          begin
            Buf[J] := Ord(Chars[I]);
            Inc(I);
          end;
          Inc(J);
        end;

        QuickSortChars(Buf, 0, J-1);

        SetLength(Result, P);
        LastBuf := 0; P := 1;
        for I := 0 to J - 1 do
        begin
          if LastBuf = Buf[I] then Continue;
          LastBuf := Buf[I];
          if LastBuf <= $7F then Continue;
          if Uni_IsIgnorable(LastBuf) then Continue;

          if Uni_IsSurrogate(Buf[I]) then
          begin
            Result[P] := Uni_UTF32ToSurrogate1(Buf[I]); Inc(P);
            Result[P] := Uni_UTF32ToSurrogate2(Buf[I]);
          end
          else
            Result[P] := WideChar(Buf[I]);
          Inc(P);
        end;
        SetLength(Result, P-1);
      end;
begin

  FChars := '';

  try
    GetKeyboardInfo( FileName, True, ki, False);
  except
    on E:EFileStreamError do
    begin
      Result := '';
      Exit;
    end;
  end;
  try
    { Go through all the rules and stores in the keyboard and put together a complete
      list of Unicode characters }
    kfh := PKeyboardFileHeader(KAddr(0));
    kfs := PKeyboardFileStore(KAddr(kfh.dpStoreArray));

    if kfh.cxStoreArray > 0 then
      for i := 0 to kfh.cxStoreArray - 1 do
      begin
        if kfs.dwSystemID = 0 then
          AddChars(kfs.dpString);
        Inc(kfs);
      end;

    kfg := PKeyboardFileGroup(KAddr(kfh.dpGroupArray));
    if kfh.cxGroupArray > 0 then
      for i := 0 to kfh.cxGroupArray - 1 do
      begin
        AddChars(kfg.dpMatch);
        AddChars(kfg.dpNoMatch);
        kfk := PKeyboardFileKey(KAddr(kfg.dpKeyArray));
        if kfg.cxKeyArray > 0 then
          for j := 0 to kfg.cxKeyArray - 1 do
          begin
            AddChars(kfk.dpOutput);
            AddChars(kfk.dpContext);
            Inc(kfk);
          end;
        Inc(kfg);
      end;
  finally
    ki.MemoryDump.Free;
  end;

  Result := SortChars(FChars);
end;

end.
