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

description : Language specific constant for Spanish(chilean)

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Puede tipear el nombre de la propiedad aqui';
  sRegAutoEditorTreeHint       = 'Propiedades disponibles';
  sRegAutoEditorListHint       = 'Propiedades almacenadas';
  sRegAutoEditorBtnAddPropHint = 'Agregar/Eliminar propiedad';
  sRegAutoEditorSort           = 'Orden';

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
  SScrollBarRange = 'Balor de la barra de scroll fuera de l�mite';
{$ENDIF}

 {JvDlg}
  SOk = 'Aceptar';
  SCancel = 'Cancelar';

 { Menu Designer }
  SMDMenuDesigner       = '&Dise�ador de Men�s';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Eliminar';
  SMDCreateSubmenuItem  = 'Crear &Sub Men�';

  SCantGetShortCut      = 'Nombre de archivo destino para ShortCut %s no disponible';


 { RALib 1.23 }
  SPropertyNotExists    = 'Propiedad "%s" no existe';
  SInvalidPropertyType  = 'Propiedad "%s" tiene un tipo inv�lido';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propiedades del editor';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colores';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Seteos r�pidos del Editor';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Mapeo de teclas por defecto';
  SHLEdPropDlg_gbEditor = 'Opciones del Editor:';
  SHLEdPropDlg_cbAutoIndent = 'Modo de &Autoindentaci�n';
  SHLEdPropDlg_cbSmartTab = 'Tabla I&nteligente';
  SHLEdPropDlg_cbBackspaceUnindents = 'Tecla Backspace &desindenta';
  SHLEdPropDlg_cbGroupUndo = 'Deshacer &Grupo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor m�s all� de &Fin de Archivo (EOF)';
  SHLEdPropDlg_cbUndoAfterSave = '&Deshacer despu�s de sal&var';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Mantener blancos de cola';
  SHLEdPropDlg_cbDoubleClickLine = 'Linea &doble click';
  SHLEdPropDlg_cbSytaxHighlighting = 'Usar &syntaxis destacada';
  SHLEdPropDlg_lblTabStops = 'paradas de &Tabs:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Seteo r�pido de color para';
  SHLEdPropDlg_lblElement = '&Elemento:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Atributos de texto:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Use valores por defecto para:';
  SHLEdPropDlg_cbBold = '&Resaltado';
  SHLEdPropDlg_cbItalic = '&Cursiva';
  SHLEdPropDlg_cbUnderline = '&Subrayado';
  SHLEdPropDlg_cbDefForeground = 'A&delante';
  SHLEdPropDlg_cbDefBackground = '&Atr�s';
  SHLEdPropDlg_OptionCantBeChanged = 'Esta opci�n no puede ser cambiada. Lo siento.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Propiedad de JvHLEditor no est� asignada';
  SHLEdPropDlg_RegAutoNotAssigned = 'Propiedad RegAuto no est� asignada';

implementation

end.
