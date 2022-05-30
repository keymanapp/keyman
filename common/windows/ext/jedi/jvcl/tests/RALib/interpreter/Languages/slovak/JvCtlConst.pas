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

Contributor(s): Tibor Bednar (tiborbmt@hotmail.com)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Slovak

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Tu zadajte n�zov vlastnosti';
  sRegAutoEditorTreeHint       = 'Dostupn� vlastnosti';
  sRegAutoEditorListHint       = 'Ulo�en� vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'Prida�/odstr�ni� vlastnos�';
  sRegAutoEditorSort           = 'Usporiada�';

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
  SScrollBarRange = 'Hodnota pos�va�a je mimo hran�c';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Cancel';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh &menu';
  SMDInsertItem         = '&Vlo�i�';
  SMDDeleteItem         = 'O&dstr�ni�';
  SMDCreateSubmenuItem  = 'Vytvori� &podmenu';

  SCantGetShortCut      = 'Cie�ov� s�bor odkazu %s je nedostupn�';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Vlastnos� "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnos� "%s" m� neplatn� typ';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastnost�';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Farby';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = '�tandardn� rozlo�enie kl�ves';
  SHLEdPropDlg_gbEditor = 'Nastavenia editoru:';
  SHLEdPropDlg_cbAutoIndent = '&Automatick� odsa�ovanie';
  SHLEdPropDlg_cbSmartTab = 'Inteli&gentn� tabul�tor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Backspace r&u�� odsadenie';
  SHLEdPropDlg_cbGroupUndo = '&Skupinov� sp�';
  SHLEdPropDlg_cbCursorBeyondEOF = '&Kurzor za koncom s�boru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umo�ni� s&p� po ulo�en�';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Zachova� koncov� &medzery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na riadku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Zv�raz�ovanie &syntaxe';
  SHLEdPropDlg_lblTabStops = 'Zar�ka &tabul�toru:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Farby SpeedSettings pre';
  SHLEdPropDlg_lblElement = '&Prvok:';
  SHLEdPropDlg_lblColor = '&Farba:';
  SHLEdPropDlg_gbTextAttributes = 'Textov� atrib�ty:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Pou�i� �tandard pre:';
  SHLEdPropDlg_cbBold = '&Tu�n�';
  SHLEdPropDlg_cbItalic = '&Kurz�va';
  SHLEdPropDlg_cbUnderline = '&Pod�iarknut�';
  SHLEdPropDlg_cbDefForeground = 'P&opredie';
  SHLEdPropDlg_cbDefBackground = 'Poza&die';
  SHLEdPropDlg_OptionCantBeChanged = 'T�to vo�ba nem��e by� zmenen�.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnos� JvHLEditor nie je priraden�';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnos� RegAuto nie je priraden�';

implementation

end.
