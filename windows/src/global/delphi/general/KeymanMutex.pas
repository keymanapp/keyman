(*
  Name:             Mutex
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      28 Feb 2011

  Modified Date:    28 Feb 2011
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman splash from showing multiple copies
*)
unit KeymanMutex;

interface

uses
  Windows;

type
  TKeymanMutex = class
  private
    hMutex: THandle;
  public
    constructor Create(FName: string);
    destructor Destroy; override;
    function TakeOwnership: Boolean;
    function ReleaseOwnership: Boolean;
  end;

implementation

uses
  SysUtils;

{ TKeymanMutex }

constructor TKeymanMutex.Create(FName: string);
begin
  inherited Create;
  hMutex := CreateMutex(nil, False, PChar('Keyman.'+FName));
  if hMutex = 0 then RaiseLastOSError;
end;

destructor TKeymanMutex.Destroy;
begin
  CloseHandle(hMutex);
  inherited Destroy;
end;

function TKeymanMutex.TakeOwnership: Boolean;
begin
  Result := WaitForSingleObject(hMutex, 0) = WAIT_OBJECT_0;
end;

function TKeymanMutex.ReleaseOwnership: Boolean;
begin
  Result := ReleaseMutex(hMutex);
end;

end.
