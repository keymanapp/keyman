{@exclude}
{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit provides declarations which are necessary for a successful
compilation in Delphi 5 and 6.

Author
Christian Wimmer

License
This unit is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied.
}
unit D5impl;
{$INCLUDE ..\includes\Jwscl.inc}
interface
uses Classes, JwaWindows, SysUtils;

{$IFNDEF DELPHI7_UP}


type
  {EOSError replaces EWin32Error
  @exclude
  }
  EOSError = EWin32Error;

{$IFDEF DELPHI5}
  {<b>PCardinal</b> is defined for Delphi 5 because it does not know this type.
  @exclude }
  PCardinal = ^Cardinal;
{$ENDIF DELPHI5}



{$IFDEF DELPHI5}
{@exclude}
function WideCompareText(const S1, S2: WideString): Integer;
{@exclude}
function WideSameText(const S1, S2: WideString): Boolean;

{Calls RaiseLastWin32Error internally.}
{@exclude}
procedure RaiseLastOSError;
{$ENDIF DELPHI5}

{@exclude}
procedure CheckThreadError(ErrCode: Integer); overload;
{@exclude}
procedure CheckThreadError(Err: Boolean); overload;
{$ENDIF DELPHI7_UP}

implementation

{$IFNDEF DELPHI7_UP}

resourcestring
  SThreadError = 'Thread Error: %s (%d)';


{$IFDEF DELPHI5}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function WideCompareText(const S1, S2: WideString): Integer;
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2));
  if Result = 0 then
    RaiseLastWin32Error;
  Dec(Result, 2);
end;

function WideSameText(const S1, S2: WideString): Boolean;
begin
  Result := WideCompareText(S1, S2) = 0;
end;
{$ENDIF DELPHI5}


procedure CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EThread.CreateResFmt(@SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure CheckThreadError(Err: Boolean);
begin
  if not Err then
    raise EThread.CreateResFmt(@SThreadError, [SysErrorMessage(GetLastError), GetLastError]);
end;

{$ENDIF DELPHI7_UP}

end.
