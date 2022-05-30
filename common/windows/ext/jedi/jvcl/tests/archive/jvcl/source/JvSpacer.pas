{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpacer;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  JvComponent;

type
  TJvSpacer = class(TJvCustomPanel)
  private
    FSpacing: Integer;
    procedure SetSpacing(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Align;
    property BevelInner default bvNone;
    property BevelOuter default bvNone;
    property BorderStyle default bsNone;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentColor;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

constructor TJvSpacer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];
  FSpacing := 4;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
end;

procedure TJvSpacer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Align in [alTop, alBottom] then
    AHeight := FSpacing
  else
  if Align in [alLeft, alRight] then
    AWidth := FSpacing;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvSpacer.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;

end.

