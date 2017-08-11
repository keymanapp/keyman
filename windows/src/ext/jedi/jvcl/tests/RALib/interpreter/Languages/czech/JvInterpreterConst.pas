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

unit JvInterpreterConst;

interface

const

{JvInterpreterParser}
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

 {JvInterpreterFm}
  SNoReportProc = 'Procedura "JvInterpreterRunReportPreview" nenalezena';
  SNoReportProc2 = 'Procedura "JvInterpreterRunReportPreview2" nenalezena';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Nezn�m� chyba'),
      (ID:   2; Description: 'Intern� chyba interpretu: %s'),
      (ID:   3; Description: 'P�eru�eno u�ivatelem'),
      (ID:   4; Description: 'Znovu vyvol�n� vyj�mky je mo�no pouze v obsluze vyj�mky'),
      (ID:   5; Description: 'Chyba v unit� ''%s'' na ��dku %d : %s'),
      (ID:   6; Description: 'Extern� v unit� ''%s'' na ��dku %d : %s'),
      (ID:   7; Description: 'Odep�en p��stup do ''%s'''),
      (ID:  31; Description: 'Z�znam ''%s'' nebyl definov�n'),

      (ID:  52; Description: 'P�ete�en� z�sobn�ku'),
      (ID:  53; Description: 'Nevhodn� typ'),
      (ID:  55; Description: 'Nedefinovan� ''main'' funkce'),
      (ID:  56; Description: 'Unita ''%s'' nebyla nalezena'),
      (ID:  57; Description: 'Ud�lost ''%s'' nebyla registrov�na'),
      (ID:  58; Description: 'Dfm ''%s'' nebyla nalezena'),

      (ID: 101; Description: 'Chyba v pozn�mce'),
      (ID: 103; Description: 'O�ek�v�no %s, ale nalezeno %s'),
      (ID: 104; Description: 'Nedeklarovan� identifik�tor ''%s'''),
      (ID: 105; Description: 'V�raz mus� b�t typu boolean'),
      (ID: 106; Description: 'Je o�ek�v�n typ class'),
      (ID: 107; Description: ''';'' nen� podporovan� p�ed else'),
      (ID: 108; Description: 'V�raz mus� b�t typu integer'),
      (ID: 109; Description: 'O�ek�v�n typ z�znam, object nebo class'),
      (ID: 110; Description: 'Chyb�j�c� oper�tor nebo st�edn�k'),
      (ID: 111; Description: 'Op�tovn� deklarace identifik�toru: ''%s'''),

      (ID: 171; Description: 'Index pole je mimo jeho rozm�r'),
      (ID: 172; Description: 'P��li� mnoho rozm�r� pole'),
      (ID: 173; Description: 'Nedostatek rozm�r� pole'),
      (ID: 174; Description: '�patn� velikost pole'),
      (ID: 175; Description: '�patn� rozsah pole'),
      (ID: 176; Description: 'O�ek�v�n typ pole'),

      (ID: 181; Description: 'P��li� mnoho parametr�'),
      (ID: 182; Description: 'Nedostatek parametr�'),
      (ID: 183; Description: 'Nekompatibiln� typy: ''%s'' a ''%s'''),
      (ID: 184; Description: 'Chyba p�i na��t�n� knihovny ''%s'''),
      (ID: 185; Description: 'Chybn� typ argumentu p�i vol�n� funkce ''%s'''),
      (ID: 186; Description: 'Chybn� typ v�sledku p�i vol�n� funkce ''%s'''),
      (ID: 187; Description: 'Nem��u z�skat adresu funkce ''%s'''),
      (ID: 188; Description: 'Chybn� typ argumentu p�i vol�n� funkce ''%s'''),
      (ID: 189; Description: 'Chybn� typ v�sledku p�i vol�n� funkce ''%s'''),
      (ID: 190; Description: 'Chybn� konvence vol�n� funkce ''%s'''),

      (ID: 201; Description: 'Vol�n� ''%s'' se nepovedlo: ''%s'''),

      (ID: 301; Description: 'V�raz'),
      (ID: 302; Description: 'Identifik�tor'),
      (ID: 303; Description: 'Deklarace'),
      (ID: 304; Description: 'konec souboru'),
      (ID: 305; Description: 'deklarace t��dy'),

      (ID: 401; Description: 'Implementace unity nebyla nalezena')
    );
 
implementation

end.
