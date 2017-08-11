(*
  Name:             exception_keyman
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      2 Feb 2012

  Modified Date:    4 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          02 Feb 2012 - mcdurdin - I3183 - Add debug info when tracing Keyman COM API errors
                    04 Nov 2012 - mcdurdin - I3530 - V9.0 - Merge of I3183 - Report clearer error when failure to register controller window
*)
unit exception_keyman;

interface

uses SysUtils, windows, activex, comobj, keymanerrorcodes;

type
  EKeyman = class(EOleException)
    constructor Create(const Source, WindowsMessage: string; ErrorCode: Cardinal);  // I3183   // I3530
    constructor CreateFmt(const Source, WindowsMessage: string; ErrorCode: Cardinal; AArgs: OleVariant);  // I3183   // I3530
  end;

function GetKeymanErrorMessage(const Source: string; ErrorCode: Cardinal): TKeymanErrorInfo;

implementation

uses
  utilvararray;

function GetKeymanErrorMessage(const Source: string; ErrorCode: Cardinal): TKeymanErrorInfo;
begin
  if ((Cardinal(ErrorCode) >= Cardinal(KMN_E_BASE)) and (Cardinal(ErrorCode) <= Cardinal(KMN_E_LAST))) or
     ((Cardinal(ErrorCode) >= Cardinal(KMN_W_BASE)) and (Cardinal(ErrorCode) <= Cardinal(KMN_W_LAST))) then
    Result := KeymanErrors[ErrorCode and $FFFF]
  else
  begin
    Result.Message := SysErrorMessage(ErrorCode) + ', '+IntToStr(ErrorCode);  // I3183   // I3530
    Result.Source := Source;
    Result.HelpContext := 0;
  end;
end;

{ EKeyman }

constructor EKeyman.Create(const Source, WindowsMessage: string; ErrorCode: Cardinal);  // I3183   // I3530
begin
  with GetKeymanErrorMessage(Source, ErrorCode) do
    inherited Create(ClassName+' '+Message, ErrorCode, Source, 'kmcomapi.hlp', HelpContext);
end;

constructor EKeyman.CreateFmt(const Source, WindowsMessage: string; ErrorCode: Cardinal; AArgs: OleVariant);  // I3183   // I3530
begin
  with GetKeymanErrorMessage(Source, ErrorCode) do
    inherited Create(FormatVariant(Message, AArgs), ErrorCode, Source, 'kmcomapi.hlp', HelpContext);
end;

end.
