{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSIMDCpuInfo;

interface

{$I jedi.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclSysInfo;

type
  TFormCpuInfo = class(TForm)
    LabelName: TLabel;
    EditName: TEdit;
    LabelVendor: TLabel;
    EditVendor: TEdit;
    LabelFrequency: TLabel;
    EditFrequency: TEdit;
    CheckBoxMMX: TCheckBox;
    CheckBoxExMMX: TCheckBox;
    CheckBox3DNow: TCheckBox;
    CheckBoxEx3DNow: TCheckBox;
    CheckBox64Bits: TCheckBox;
    CheckBoxSSE1: TCheckBox;
    CheckBoxSSE2: TCheckBox;
    CheckBoxSSE3: TCheckBox;
    ButtonClose: TButton;
  private
  public
    procedure Execute(const CpuInfo: TCPUInfo);
  end;

implementation

{$R *.dfm}

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{ TFormCpuInfo }

procedure TFormCpuInfo.Execute(const CpuInfo: TCPUInfo);
begin
  EditName.Text := CpuInfo.CpuName;
  EditVendor.Text := CpuInfo.VendorIDString;
  EditFrequency.Text := IntToStr(CpuInfo.FrequencyInfo.NormFreq);
  CheckBoxMMX.Checked := CpuInfo.MMX;
  CheckBoxExMMX.Checked := CpuInfo.ExMMX;
  CheckBox3DNow.Checked := CpuInfo._3DNow;
  CheckBoxEx3DNow.Checked := CpuInfo.Ex3DNow;
  CheckBox64Bits.Checked := CpuInfo.Is64Bits;
  CheckBoxSSE1.Checked := CpuInfo.SSE >= 1;
  CheckBoxSSE2.Checked := CpuInfo.SSE >= 2;
  CheckBoxSSE3.Checked := CpuInfo.SSE >= 3;
  ShowModal;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
