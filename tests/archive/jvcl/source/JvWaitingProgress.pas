{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWaitingProgress.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvWaitingProgress;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvSpecialProgress, JvImageDrawThread, JVCLVer;

type
  TJvWaitingProgress = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FActive: Boolean;
    FRefreshInterval: Cardinal;
    FLength: Cardinal;
    FOnEnded: TNotifyEvent;
    FWait: TJvImageDrawThread;
    FProgress: TJvSpecialProgress;
    function GetProgressColor: TColor;
    procedure SetActive(const Value: Boolean);
    procedure SetLength(const Value: Cardinal);
    procedure SetRefreshInterval(const Value: Cardinal);
    procedure SetProgressColor(const Value: TColor);
    procedure OnScroll(Sender: TObject);
    //function GetBColor: TColor;
    //procedure SetBColor(const Value: TColor);
  protected
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Length: Cardinal read FLength write SetLength default 30000;
    {(rb) When Active is read from the stream, RefreshInterval is 500 (as set
          in the constructor); Better set Active in Loaded }
    property Active: Boolean read FActive write SetActive default False;
    property RefreshInterval: Cardinal read FRefreshInterval write SetRefreshInterval default 500;
    property OnEnded: TNotifyEvent read FOnEnded write FOnEnded;
    property ProgressColor: TColor read GetProgressColor write SetProgressColor default clBlack;
    {(rb) no need to override Color property }
    //property Color: TColor read GetBColor write SetBColor;
    property Color;
    property ParentColor;
    property Height default 10;
    property Width default 100;
  end;

implementation

constructor TJvWaitingProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FRefreshInterval := 500;
  FLength := 30000;

  FWait := TJvImageDrawThread.Create(True);
  FWait.FreeOnTerminate := False;
  FWait.Delay := FRefreshInterval;
  FWait.OnDraw := OnScroll;

  FProgress := TJvSpecialProgress.Create(Self);
  FProgress.Parent := Self;
  FProgress.Maximum := FLength;
  FProgress.Position := 0;
  FProgress.StartColor := clBlack;
  FProgress.EndColor := clBlack;
  FProgress.Solid := True;

  FProgress.Left := 0;
  FProgress.Top := 0;
  FProgress.Width := Width;
  FProgress.Height := Height;

  //inherited Color := FProgress.Color;
  // (rom) always set default values also
  Height := 10;
  Width := 100;
end;

destructor TJvWaitingProgress.Destroy;
begin
  FWait.OnDraw := nil;
  FWait.Terminate;
  //  FWait.WaitFor;
  FreeAndNil(FWait);
  FProgress.Free;
  inherited Destroy;
end;

{function TJvWaitingProgress.GetBColor: TColor;
begin
  Result := FProgress.Color;
end;}

function TJvWaitingProgress.GetProgressColor: TColor;
begin
  Result := FProgress.StartColor;
end;

procedure TJvWaitingProgress.OnScroll(Sender: TObject);
begin
  //Step
  if Integer(FProgress.Position) + Integer(FRefreshInterval) > Integer(FLength) then
  begin
    FProgress.Position := FLength;
    SetActive(False);
    if Assigned(FOnEnded) then
      FOnEnded(Self);
  end
  else
    FProgress.Position := FProgress.Position + Integer(FRefreshInterval);
  Application.ProcessMessages;
end;

procedure TJvWaitingProgress.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      FProgress.Position := 0;
      FWait.Resume;
    end
    else
      FWait.Suspend;
  end;
end;

{procedure TJvWaitingProgress.SetBColor(const Value: TColor);
begin
  if FProgress.Color <> Value then
  begin
    FProgress.Color := Value;
    inherited Color := Value;
  end;
end;}

procedure TJvWaitingProgress.SetProgressColor(const Value: TColor);
begin
  FProgress.StartColor := Value;
  FProgress.EndColor := Value;
end;

procedure TJvWaitingProgress.SetLength(const Value: Cardinal);
begin
  FLength := Value;
  FProgress.Position := 0;
  FProgress.Maximum := FLength;
end;

procedure TJvWaitingProgress.SetRefreshInterval(const Value: Cardinal);
begin
  FRefreshInterval := Value;
  FWait.Delay := FRefreshInterval;
end;

procedure TJvWaitingProgress.WMSize(var Msg: TWMSize);
begin
  inherited;
  FProgress.Width := Self.Width;
  FProgress.Height := Self.Height;
end;

procedure TJvWaitingProgress.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  FProgress.Color := Color;
end;

end.

