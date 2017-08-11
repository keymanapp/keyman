unit DirMonitor;
//**************************************************************************
//                           DirMonitor unit                               *
//                  by Peter Morris (Cubud)                                *
//                     For Delphi 5 to XE                                  * 
//                         Freeware Unit                                   *
//                                                                         *
//    Contributor:                                                         *
//    Eran Bodankin (bsalsa)  bsalsa@gmail.com - D2005 update              *
//    Robert Marquardt <robert_marquardt@gmx.de> Clean the code and Update *
//   I got Permition to add the DirMonitor unit and to change              *
//   the code by my needs from Peter Morris (Pete@StuckIndoors.com)        *
//   We thank him for that.                                                *
//   Regards,                                                              *
//   bsalsa                                                                *
//                                                                         *
//     Documentation and updated versions:                                 *
//               http://www.bsalsa.com                                     * 
//                                                                         *
//     Ps, to get your own FREE events page, go to                         *
//     www.stuckindoors.com                                                *
//**************************************************************************
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
{*******************************************************************************}
//$Id: DirMonitor.pas,v 1.2 2006/11/15 21:01:38 sergev Exp $

interface

{$I EWB.inc}

{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Windows, SysUtils, Classes, FileCtrl;

type
  TDirMonitor = class(TThread)
  private
    FCallback: TNotifyEvent;
    FDirectoryPath: string;
    FSubDirs: Boolean;
    procedure DoCallback;
  protected
    procedure Execute; override;
  public
    constructor Create(ADirectoryPath: string; ASubDirs: Boolean; ACallback: TNotifyEvent);
    property DirectoryPath: string read FDirectoryPath;
    property SubDirs: Boolean read FSubDirs;
  end;

implementation

constructor TDirMonitor.Create(ADirectoryPath: string;
  ASubDirs: Boolean; ACallback: TNotifyEvent);
begin
  inherited Create(True);
  if not {$IFDEF DELPHI6_UP}SysUtils.{$ENDIF}DirectoryExists(ADirectoryPath) then
    raise Exception.Create('Path does not exist');
  if not Assigned(ACallback) then
    raise Exception.Create('No callback has been set');
  FSubDirs := ASubDirs;
  FDirectoryPath := ADirectoryPath;
  FCallback := ACallback;
  FreeOnTerminate := True;
  {$IFDEF DELPHI2010_UP}
  Start;
  {$ELSE}
  Resume;
  {$ENDIF}
end;

procedure TDirMonitor.DoCallback;
begin
  FCallback(Self);
end;

procedure TDirMonitor.Execute;
var
  Event: THandle;
  WaitResult: DWORD;
begin
  Event := FindFirstChangeNotification(PChar(DirectoryPath), SubDirs,
    FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or
    FILE_NOTIFY_CHANGE_SIZE or FILE_NOTIFY_CHANGE_LAST_WRITE);
  if Event = INVALID_HANDLE_VALUE then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Terminate;
  end
  else
    repeat
      WaitResult := WaitForSingleObject(Event, 100);
      case WaitResult of
        WAIT_FAILED or WAIT_ABANDONED:
          begin
{$IFDEF USE_DebugString}
            OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
            Terminate;
          end;
        WAIT_OBJECT_0:
          if Assigned(FCallback) then
          try
            Synchronize(DoCallback);
          finally
            Terminate;
          end;
      end;
    until Terminated or (WaitResult = WAIT_OBJECT_0);
  if not FindCloseChangeNotification(Event) then
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
end;
(*
procedure TDirMonitor.Execute;
var
  Event: THandle;
  Finished: Boolean;
  WaitResult: DWORD;
begin
  Event := FindFirstChangeNotification(PChar(DirectoryPath), SubDirs,
    FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or
    FILE_NOTIFY_CHANGE_SIZE or FILE_NOTIFY_CHANGE_LAST_WRITE);
  if Event = INVALID_HANDLE_VALUE then Exit;
  repeat
    WaitResult := WaitForSingleObject(Event, 100);
    Finished := (WaitResult = WAIT_OBJECT_0);
    if Finished and Assigned(FCallback) then
      Synchronize(DoCallback);
  until Terminated or Finished;
  FindCloseChangeNotification(Event);
end;
*)
end.
