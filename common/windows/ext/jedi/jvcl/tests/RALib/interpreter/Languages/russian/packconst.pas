{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constants for Russian

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface
                                   
const
  SOK               = 'OK';
  SCancel           = '������';

 { Dataset Editor }
  SDEDatasetDesigner = '&�������� �����...';

  SDEAddItem          = '&�������� ����...';
  SDEDeleteItem       = '&�������';
  SDESelectAllItem    = '&�������� ���';
  SDENewItem          = '&����� ����...';

  SDEAddFieldsCaption = '���������� �����';
  SDEAvailableFields  = '��������� ����';

  SDENewFieldCaption    = '����� ����';
  SDEFieldProperties    = ' �������� ���� ';
  SDEFieldNameLabel     = '&���:';
  SDEFieldTypeLabel     = '&���:';
  SDEComponentNameLabel = '&���������:';
  SDEFieldSizeLabel     = '&������:';
  SDEFieldKind          = ' ��� ���� ';
  SDELookupGroup        = ' ��������� ���� ';
  SDEKeyFieldsLabel     = '&Key Fields:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = '&Result Field:';
  SDEFieldKindItems     = '&������'#13'&�����������'#13'&���������';
  SDEFieldTypeMustBeSpecified = '��� ���� ������ ���� ������';

  SDBGridColEditor    = '&�������� �������...';

 { Collection Editor }
  SCEEditCollection     = '����������� %s';
  SCEAdd                = '&��������';
  SCEDelete             = '&�������';
  SCEMoveUp             = '�&����';
  SCEMoveDown           = '�&���';
  SCESelectAllItem      = '�������� ��&�';

 { Picture Editor }
  SPELoad               = '&���������...';
  SPESave               = '&���������...';
  SPEClear              = '&��������';
  SPECopy               = '&����������';
  SPEPaste              = '&��������';

 { Menu Designer }
  SMDMenuDesigner       = '&�������� ����';
  SMDInsertItem         = '&��������';
  SMDDeleteItem         = '&�������';
  SMDCreateSubmenuItem  = '������� &�������';

implementation

end.
