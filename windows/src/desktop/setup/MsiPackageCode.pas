(*
  Name:             MsiPackageCode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2007

  Modified Date:    23 Aug 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2007 - mcdurdin - Initial version
*)
unit MsiPackageCode;

interface

function GetMSIPackageCode(const FileName: WideString): WideString;

implementation

uses
  Dialogs,
  jwawintype, jwawinbase, jwawinerror, jwamsi, jwamsiquery,
  SysUtils, RunTools;

function GetMSIPackageCode(const FileName: WideString): WideString;
const
  PID_REVNUMBER = 9;
var
  uiDataType: DWORD;
  iValue: Integer;
  ftValue: JwaWinType.FILETIME;
  sz: DWord;
  buf: array[0..64] of WideChar;
  hSummary: MSIHANDLE;
begin
  CheckMSIResult(MsiGetSummaryInformationW(0, PWideChar(FileName), 0, hSummary));
  sz := 64;
  CheckMSIResult(MsiSummaryInfoGetPropertyW(hSummary, PID_REVNUMBER, uiDataType, iValue, ftValue, buf, sz));
  ShowMessage(IntToStr(uiDataType) + #13#10 + buf);
  Result := buf;
  CheckMSIResult(MsiCloseHandle(hSummary));
end;

end.
