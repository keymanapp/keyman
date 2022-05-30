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

description : Language specific constants for Russian

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = '������� %s ?';
  SDeleteNode2            = '������� %s (������ �� ���� ����������) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = '�������� ����� ����������� RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

   {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = '��� �������� ����� ������ ����� �����';
  sRegAutoEditorTreeHint       = '��������� ��������';
  sRegAutoEditorListHint       = '������ ����������� �������';
  sRegAutoEditorBtnAddPropHint = '��������/������� ��������';
  sRegAutoEditorSort           = '�����������';

 {JvEditor}
  RAEditorCompletionChars = #8+'_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';

{IParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z', '�'..'�', '�'..'�'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z', '�'..'�', '�'..'�'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm�������������������������������ި���������������������������������';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF BCB}

{$IFDEF COMPILER2}
  SScrollBarRange = '�������� Scrollbar ����� �� ���������� �������';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = '������';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';

  SCantGetShortCut      = 'Target FileName for ShortCut %s not available';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Property "%s" does not exists';
  SInvalidPropertyType  = 'Property "%s" has invalid type';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = '��������: ��������';
  SHLEdPropDlg_tsEditor = '��������';
  SHLEdPropDlg_tsColors = '�����';
  SHLEdPropDlg_lblEditorSpeedSettings = '����� ������� ������';
  SHLEdPropDlg_cbKeyboardLayotDefault = '����������� �����';
  SHLEdPropDlg_gbEditor = '��������� ���������:';
  SHLEdPropDlg_cbAutoIndent = '&����������';
  SHLEdPropDlg_cbSmartTab = '&����� ���';
  SHLEdPropDlg_cbBackspaceUnindents = '����� &�������� �����';
  SHLEdPropDlg_cbGroupUndo = '&��������� ������';
  SHLEdPropDlg_cbCursorBeyondEOF = '������ �� ����� &�����';
  SHLEdPropDlg_cbUndoAfterSave = '������ &����� ����������';
  SHLEdPropDlg_cbKeepTrailingBlanks = '��������� &����������� �������';
  SHLEdPropDlg_cbDoubleClickLine = '&������� ���� �������� ������';
  SHLEdPropDlg_cbSytaxHighlighting = '�������� &���������';
  SHLEdPropDlg_lblTabStops = '&�����������:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = '����� ���';
  SHLEdPropDlg_lblElement = '&�������:';
  SHLEdPropDlg_lblColor = '&����:';
  SHLEdPropDlg_gbTextAttributes = '�������� ������:';
  SHLEdPropDlg_gbUseDefaultsFor = '�����������:';
  SHLEdPropDlg_cbBold = '&������';
  SHLEdPropDlg_cbItalic = '&���������';
  SHLEdPropDlg_cbUnderline = '&������������';
  SHLEdPropDlg_cbDefForeground = '&�����';
  SHLEdPropDlg_cbDefBackground = '&���';
  SHLEdPropDlg_OptionCantBeChanged = '���� �������� ������ ������. ��������.';
  SHLEdPropDlg_RAHLEditorNotAssigned = '�������� JvHLEditor �� ���������';
  SHLEdPropDlg_RegAutoNotAssigned = '�������� RegAuto �� ���������';

implementation

end.

