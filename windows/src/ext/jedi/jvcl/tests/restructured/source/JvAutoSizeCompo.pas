{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAutoSizeCompo.PAS, released on 2001-02-28.

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

unit JvAutoSizeCompo;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JvComponent;

type
  TJvAutoSizeCompo = class(TJvComponent)
  private
    FForm: TForm;
    FActive: Boolean;
    FResize: TNotifyEvent;
    FOldWidth: Integer;
    FOldHeight: Integer;
    procedure Resize(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // (p3) default here should be false!!!
    property Active: Boolean read FActive write FActive; // default true;
  end;

implementation

{**************************************************}

constructor TJvAutoSizeCompo.Create(AOwner: TComponent);
begin
  inherited;
  // (p3) dangerous: can create problems without user being aware
//  FActive := True;
  FForm := GetParentForm(TControl(AOwner)) as TForm;
  if FForm <> nil then
  begin
    FOldWidth := FForm.Width;
    FOldHeight := FForm.Height;
    FResize := FForm.OnResize;
    FForm.OnResize := Resize;
  end;
end;
{**************************************************}

destructor TJvAutoSizeCompo.Destroy;
begin
  if FForm <> nil then
    FForm.OnResize := nil;
  FForm := nil;
  inherited;
end;
{**************************************************}

procedure TJvAutoSizeCompo.Resize(Sender: TObject);
var
  WidthRatio, HeightRatio: Double;
  CompIndex: Integer;
begin
  if FForm = nil then
    FForm := GetParentForm(Owner as TControl) as TForm;
  if FActive and (FForm <> nil) then
  begin
    // (p3) this code is slightly dangerous: no sanity checks -
    // values can become really large or really small
    if (FOldWidth <> 0) and (FOldHeight <> 0) then
    begin
      WidthRatio := FForm.Width / FOldWidth;
      HeightRatio := FForm.Height / FOldHeight;
      for CompIndex := 0 to (FForm.ComponentCount - 1) do
      begin
        if FForm.Components[CompIndex] is TControl then
        begin
          with FForm.Components[CompIndex] as TControl do
          begin
            if not (FForm.Components[CompIndex] is TButton) then
            begin
              Width := Round(Width * WidthRatio);
              Height := Round(Height * HeightRatio);
            end;
            Left := Round(Left * WidthRatio);
            Top := Round(Top * HeightRatio);
          end;
        end;
      end;
    end;
    FOldWidth := FForm.Width;
    FOldHeight := FForm.Height;
  end;
  if Assigned(FResize) then
    FResize(Sender);
end;

end.
