{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTSync.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.
            
Last Modified: 2002-09-29

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit MTSync;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, SyncObjs, Contnrs, MTConst;

type
  TMTSynchroObject = class (TSynchroObject)
  private
    FHandle: THandle;
    FLastError: Integer;
    FName: string;
  protected
    function CreateHandle: THandle; virtual; abstract;
  public
    constructor Create(Name: string = '');
    destructor Destroy; override;
    procedure Acquire; override;
    procedure Release; override;
    procedure Signal;
    procedure Wait;
    function WaitFor(Timeout: LongWord): Boolean; virtual;
    property Handle: THandle read FHandle;
    property LastError: Integer read FLastError;
    property Name: string read FName;
  end;

  TMTSimpleEvent = class(TMTSynchroObject)
  protected
    function CreateHandle: THandle; override;
  public
    procedure Release; override;

    procedure SetEvent;
    procedure ResetEvent;
  end;

  TMTSemaphore = class(TMTSynchroObject)
  private
    FInitialCount: Integer;
    FMaximumCount: Integer;
  protected
    function CreateHandle: THandle; override;
  public
    constructor Create(InitialCount, MaximumCount: Integer; Name: string = '');
    procedure Release; override;
  end;

  TMTMutex = class (TMTSemaphore)
  public
    constructor Create(Name: string = '');
    procedure Enter;
    procedure Leave;
  end;

  TMTCriticalSection = class (TMTMutex)
  private
    FOwnerThread: TObject;
    FSelfCount: Integer;
  public
    procedure Release; override;
    function WaitFor(Timeout: LongWord): Boolean; override;
  end;
  

implementation

uses
  MTThreading;

{ TMTSemaphore }

constructor TMTSynchroObject.Create(Name: string = '');
begin
  if Name = '' then
    FName := ClassName
  else
    FName := Name;
  FHandle := CreateHandle;
//  FHandle := CreateSemaphore(nil, InitialCount, MaximumCount, '');
end;

destructor TMTSynchroObject.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

procedure TMTSynchroObject.Acquire;
var
  OldName: string;
begin
  // first wait for 500 ms
  if not WaitFor(500) then
  begin
    // still not succeded: change the name of the thread and wait again
    if CurrentMTThread <> nil then
    begin
      OldName := CurrentMTThread.Name;
      CurrentMTThread.Name := OldName+'.'+FName+'.Wait';
    end;
    try
      WaitFor(INFINITE); // this time, wait forever (ETerminate can be raised though)
    finally
      if CurrentMTThread <> nil then
        CurrentMTThread.Name := OldName;
    end;
  end;
end;

procedure TMTSynchroObject.Release;
begin
//  ReleaseSemaphore(FHandle, 1, nil);
end;

procedure TMTSynchroObject.Signal;
begin
  Release;
end;

procedure TMTSynchroObject.Wait;
begin
  Acquire;
end;

function TMTSynchroObject.WaitFor(Timeout: LongWord): Boolean;
var
  HandleArray: array[0..1] of THandle;
  
  {
  WaitFor()
  
  Wait for the semaphore to become signalled or the for the timeout time to pass.
  If the Thread is terminated before or during the waiting, an EMTTerminate
  exception will be raised.
  The exception will only be raised if the semaphore was not signalled
  during the wait. This will ensure that the caller can take appropriate
  measures to return the semaphore to the appropriate state before terminating
  the thread.
  
  }
  
begin
  Result := False;
  
  if CurrentMTThread <> nil then
  begin {MT thread}
  
    // don't wait if we are allready terminated
    //   because we don't want to take the risk of getting the
    //   semaphore in that case.
    CurrentMTThread.CheckTerminate;
  
    // setup the handle array.
    //   the semphore has priority over the terminate signal
    //   because if we get the semaphore we must not raise an EMTTerminate
    HandleArray[0] := FHandle;
    HandleArray[1] := CurrentMTThread.TerminateSignal;
  
    // perform the wait
    case WaitForMultipleObjects(2, @HandleArray, False, TimeOut) of
      WAIT_FAILED:
      begin
        FLastError := GetLastError;
        raise EMTThread.Create('Semaphore failure ('+IntToStr(FLastError)+')');
      end;
      WAIT_TIMEOUT: Result := False;
      WAIT_OBJECT_0: Result := True;
      WAIT_OBJECT_0+1: CurrentMTThread.CheckTerminate; // do raise EMTTerminate
      WAIT_ABANDONED: raise EMTTerminate.Create('Semphore was abandoned');
      WAIT_ABANDONED+1: raise EMTTerminate.Create('Thread was abandoned');
    end;
  
  end
  else begin {main VCL thread}
    // perform the wait without checking the TerminateSignal since the
    // main VCL thread does not have such a signal
    case WaitForSingleObject(FHandle, Timeout) of
      WAIT_OBJECT_0: Result := True;
      WAIT_ABANDONED: raise EMTTerminate.Create('Semphore was abandoned');
      WAIT_TIMEOUT: Result := False;
      WAIT_FAILED:
      begin
        FLastError := GetLastError;
        raise EMTThread.Create('Semaphore failure');
      end;
    end;
  end;

end;

{ TMTSemaphore }

constructor TMTSemaphore.Create(InitialCount, MaximumCount: Integer;
  Name: string);
begin
  FInitialCount := InitialCount;
  FMaximumCount := MaximumCount;
  inherited Create(Name);
end;

function TMTSemaphore.CreateHandle: THandle;
begin
  Result := CreateSemaphore(nil, FInitialCount, FMaximumCount, '');
end;

procedure TMTSemaphore.Release;
begin
  ReleaseSemaphore(FHandle, 1, nil);
end;

{ TMTMutex }

constructor TMTMutex.Create(Name: string = '');
begin
  inherited Create(1, 1);
end;

procedure TMTMutex.Enter;
begin
  Acquire;
end;

procedure TMTMutex.Leave;
begin
  Release;
end;

{ TMTCriticalSection }

procedure TMTCriticalSection.Release;
begin
  Dec(FSelfCount);
  if FSelfCount = 0 then
  begin
    FOwnerThread := nil;
    inherited Release;
  end;
end;

function TMTCriticalSection.WaitFor(Timeout: LongWord): Boolean;
begin
  if CurrentMTThread <> FOwnerThread then
  begin
  Result := inherited WaitFor(Timeout);
    if Result then
    begin
      FOwnerThread := CurrentMTThread;
      FSelfCount := 1;
    end;
  end
  else begin
    Result := True;
    Inc(FSelfCount);
  end;
end;


{ TMTSimpleEvent }

function TMTSimpleEvent.CreateHandle: THandle;
begin
  Result := CreateEvent(nil, True, False, '');
end;

procedure TMTSimpleEvent.Release;
begin
  SetEvent;
end;

procedure TMTSimpleEvent.ResetEvent;
begin
  Windows.ResetEvent(FHandle);
end;

procedure TMTSimpleEvent.SetEvent;
begin
  Windows.SetEvent(FHandle);
end;

end.
