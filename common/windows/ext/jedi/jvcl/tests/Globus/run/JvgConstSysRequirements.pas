{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgConstSysRequirements.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgConstSysRequirements;

interface

{ Constant messages for TJvgSysRequirements }

resourcestring
  {$IFDEF RUSSIAN}
  ERR_VideoVRefreshRate = '������� ���������� ������ ������ ���� %d ���� ��� ����. �������� ������� ���������� � ��������� ������.';
  ERR_GraphicResolution = '���������� ������ ������ ���� %s ����� ��� ����. �������� ���������� � ��������� ������.';
  ERR_ColorDepth = '���������� ������ ������ ������ ���� %s ������ ��� ����. �������� ����� ������ � ��������� ������.';
  ERR_SystemFont = '� ������� ������ ���� ���������� %s �����. �������� ��� ������ � ��������� ������.';
  ERR_OSPlatform = '��� ������ ��������� ���������� ������������ ������� %s.';
  {$ELSE}
  ERR_VideoVRefreshRate = 'The monitor refresh rate should be %d hertz or higher. Change monitor refresh rate in Monitor Control Panel.';
  ERR_GraphicResolution = 'The screen resolution should be equal %s pixels or higher. Change screen resolution in Monitor Control Panel.';
  ERR_ColorDepth = 'The number of colors of the screen should be equal to %s colors or higher. Change screen colors in Monitor Control Panel.';
  ERR_SystemFont = 'In system the small font should be established. Change to small fonts in Monitor Control Panel.';
  ERR_OSPlatform = 'The program requires %s or better.';
  {$ENDIF}

implementation

end.
