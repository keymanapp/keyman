(*
  Name:             utilxml
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    13 Jul 2007 - mcdurdin - I959 - Fix bug with xml encoding of entities
                    20 Jul 2008 - mcdurdin - Raise exception if gettempfilename fails
                    30 Jan 2009 - mcdurdin - I1839 - Improve XMLEncode performance
                    07 Sep 2009 - mcdurdin - I2092 - Regression on XMLEncode, causes invalid text for short XML strings due to buffer overflow
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit utilxml;  // I3306

interface

uses
  Classes;

function XMLEncode(const s: WideString): WideString;
function XMLFormat(const Args: array of const): WideString;
function XMLImageTempName(const ImagePath: string; var TempLockFile: string; References: TStrings): string;

implementation

uses
  Windows,
  StrUtils,
  SysUtils,
  Unicode;

{ I1839 }
function XMLEncode(const s: WideString): WideString;
var
  I, J: Integer;
  ls, lr: Integer;

      procedure DoAppendCh(const v: WideChar);
      begin
        if lr <= J+1 then
        begin
          lr := lr * 2;
          SetLength(Result, lr);
        end;
        Result[J] := v;
        Inc(J);
      end;

      procedure DoAppend(const v: WideString);
      var
        lv, K: Integer;
      begin
        lv := Length(v);
        while lr <= J+lv do
        begin
          lr := lr * 2;
          SetLength(Result, lr);
        end;
        for K := 1 to lv do
        begin
          Result[J] := v[K];
          Inc(J);
        end;
      end;
begin
  ls := Length(s);
  lr := ls * 2;
  SetLength(Result, lr);
  J := 1;
  for I := 1 to ls do
  begin
    case s[I] of
      '<':  DoAppend('&lt;');
      '>':  DoAppend('&gt;');
      '&':  DoAppend('&amp;');
      '''': DoAppend('&apos;');
      '"':  DoAppend('&quot;');
      else  DoAppendCh(s[I]);
    end;
  end;
  SetLength(Result, J-1);
end;

function XMLFormat(const Args: array of const): WideString;
var
  Value, FTag: WideString;
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Args) div 2 do
  begin
    with Args[I*2] do
      case VType of
        vtPWideChar:   FTag := VPWideChar;
        vtString:      FTag := String_AtoU(VString^);  // I3310
        vtAnsiString:  FTag := String_AtoU(AnsiString(VAnsiString));  // I3310
        vtWideString:  FTag := WideString(VWideString);
        vtPChar:       FTag := String_AtoU(ansistring(VPChar));  // I3310
        vtUnicodeString: FTag := UnicodeString(VUnicodeString);  // I3310
      else
        Assert(False, 'Invalid VType');
      end;

     with Args[I*2+1] do
        case VType of
          vtInteger:  Value := IntToStr(VInteger);
          vtBoolean:  begin if VBoolean then Result := Result + '<'+FTag+' />'; Continue; end;
          vtChar:     Value := String_AtoU(VChar);  // I3310
          vtExtended: Value := FloatToStr(VExtended^);
          vtString:   Value := String_AtoU(VString^);  // I3310
          vtPChar:    Value := String_AtoU(ansistring(VPChar));  // I3310
          vtAnsiString:  Value := String_AtoU(ansistring(VAnsiString));  // I3310
          vtCurrency:    Value := CurrToStr(VCurrency^);
          vtVariant:     Value := string(VVariant^);
          vtInt64:       Value := IntToStr(VInt64^);
          vtPWideChar:   Value := VPWideChar;
          vtWideString:  Value := WideString(VWideString);
          vtUnicodeString: Value := UnicodeString(VUnicodeString);  // I3310
        else
          Assert(False, 'Unsupported VType');
        end;

     if Value <> '' then
      if Copy(FTag,1,2) = 'x:'
        then Result := Result + '<'+Copy(FTag,3,Length(FTag))+'>'+Value+'</'+Copy(FTag,3,Length(FTag))+'>'
        else Result := Result + '<'+FTag+'>'+XMLEncode(Value)+'</'+FTag+'>';
  end;
end;

function XMLImageTempName(const ImagePath: string; var TempLockFile: string; References: TStrings): string;
var
  s: string;
  buf: array[0..260] of char;
begin
  s := ExcludeTrailingPathDelimiter(ImagePath);
  if GetTempFileName(PChar(s), 'kmn', 0, buf) = 0 then
    RaiseLastOSError;
  TempLockFile := buf;
  Result := ChangeFileExt(TempLockFile, '.bmp');
  References.Add(ExtractFileName(Result));
end;

end.
