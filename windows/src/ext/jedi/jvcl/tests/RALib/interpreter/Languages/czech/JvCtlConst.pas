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

Contributor(s): Jaromir Solar (jarda@foresta.cz)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Czech

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Nejprve mus�te zadat jm�no vlastnosti';
  sRegAutoEditorTreeHint       = 'Dostupn� vlastnosti';
  sRegAutoEditorListHint       = 'Ulo�en� vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'P�idat/odstranit vlastnost';
  sRegAutoEditorSort           = 'Set��dit';

 {JvEditor}
  RAEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

{IParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF BCB}

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z',  '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '_'];
{$ENDIF RAINTER}

{$IFDEF COMPILER2}
  SScrollBarRange = 'Hodnota �oup�tka je mimo hranice';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Storno';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh�� &menu';
  SMDInsertItem         = '&Vlo�it';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvo�it &podmenu';

  SCantGetShortCut      = 'Soubor u z�stupce %s nen� dostupn�';


 { RALib 1.23 }
  SPropertyNotExists    = 'Vlastnost "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnost "%s" je �patn�ho typu';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastnost�';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Barvy';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'P�ednastaven� mapov�n� kl�ves';
  SHLEdPropDlg_gbEditor = 'Vlastnosti editoru:';
  SHLEdPropDlg_cbAutoIndent = '&Auto odsazovac� m�d';
  SHLEdPropDlg_cbSmartTab = 'Ch&ytr� tabul�tor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Odsazen� p�i &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Skupinov� zp�t';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Kurzor i za &konec souboru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umo�nit z&p�t po ulo�en�';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Dr�et koncov� mezery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na ��dku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Pou�ij z&v�razn�n� syntaxe';
  SHLEdPropDlg_lblTabStops = 'Stop &tabul�toru na:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Barva SpeedSettings pro';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Barva:';
  SHLEdPropDlg_gbTextAttributes = 'Textov� atributy:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Pou�ij p�ednastaven� pro:';
  SHLEdPropDlg_cbBold = '&Tu�n�';
  SHLEdPropDlg_cbItalic = '&Kurz�va';
  SHLEdPropDlg_cbUnderline = '&Podtr�eno';
  SHLEdPropDlg_cbDefForeground = 'P&op�ed�';
  SHLEdPropDlg_cbDefBackground = 'Poza&d�';
  SHLEdPropDlg_OptionCantBeChanged = 'Tato mo�nost nem��e b�t zm�n�na.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnost JvHLEditor nen� p�i�azena';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnost RegAuto nen� p�i�azena';

implementation

end.
