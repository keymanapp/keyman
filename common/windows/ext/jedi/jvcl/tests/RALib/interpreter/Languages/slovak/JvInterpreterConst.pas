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
  SNoReportProc = 'Proced�ra "JvInterpreterRunReportPreview" nebola n�jden�';
  SNoReportProc2 = 'Proced�ra "JvInterpreterRunReportPreview2" nebola n�jden�';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Nezn�ma chyba'),
      (ID:   2; Description: 'Intern� chyba interpretu: %s'),
      (ID:   3; Description: 'Preru�en� u��vate�om'),
      (ID:   4; Description: 'Znovu vyvolanie v�nimky je mo�n� iba v obsluhe v�nimky'),
      (ID:   5; Description: 'Chyba v jednotke ''%s'' na riadku %d : %s'),
      (ID:   6; Description: 'Extern� chyba v jednotke ''%s'' na riadku %d : %s'),
      (ID:   7; Description: 'Odopret� pr�stup do ''%s'''),
      (ID:  31; Description: 'Z�znam ''%s'' nebol definovan�'),

      (ID:  52; Description: 'Prete�enie z�sobn�ku'),
      (ID:  53; Description: 'Nevhodn� typ'),
      (ID:  55; Description: 'Nedefinovan� funkcia ''main'''),
      (ID:  56; Description: 'Jednotka ''%s'' nebola n�jden�'),
      (ID:  57; Description: 'Udalos� ''%s'' nebola registrovan�'),
      (ID:  58; Description: 'Dfm ''%s'' nebol n�jden�'),

      (ID: 101; Description: 'Chyba v pozn�mke'),
      (ID: 103; Description:'O�ak�va sa %s, ale na�lo sa %s'),
      (ID: 104; Description: 'Nedeklarovan� identifik�tor ''%s'''),
      (ID: 105; Description: 'V�raz mus� by� typu boolean'),
      (ID: 106; Description: 'Vy�aduje sa typ class'),
      (ID: 107; Description:' nie je podporovan� pred else'),
      (ID: 108; Description: 'V�raz mus� by� typu integer'),
      (ID: 109; Description: 'Vy�aduje sa typ z�znam, object alebo class'),
      (ID: 110; Description: 'Ch�ba oper�tor alebo bodko�iarka'),
      (ID: 111; Description: 'Opakovan� deklar�cia identifik�toru: ''%s'''),

      (ID: 171; Description: 'Index po�a je mimo jeho rozmer'),
      (ID: 172; Description: 'Pr�li� ve�a rozmerov po�a'),
      (ID: 173; Description: 'Nedostatok rozmerov po�a'),
      (ID: 174; Description: 'Nespr�vna ve�kos� po�a'),
      (ID: 175; Description: 'Nespr�vny rozsah po�a'),
      (ID: 176; Description: 'O�ak�va sa typ pole'),

      (ID: 181; Description: 'Pr�li� ve�a parametrov'),
      (ID: 182; Description: 'Nedostatok parametrov'),
      (ID: 183; Description: 'Nekompatibiln� typy: ''%s'' a ''%s'''),
      (ID: 184; Description: 'Chyba pri na��tavan� kni�nice ''%s'''),
      (ID: 185; Description: 'Neplatn� typ argumentu pri volan� funkcie ''%s'''),
      (ID: 186; Description: 'Neplatn� typ v�sledku pri volan� funkcie ''%s'''),
      (ID: 187; Description: 'Nie je mo�n� z�ska� adresu funkcie ''%s'''),
      (ID: 188; Description: 'Neplatn� typ argumentu pri volan� funkcie ''%s'''),
      (ID: 189; Description: 'Neplatn� typ v�sledku pri volan� funkcie ''%s'''),
      (ID: 190; Description: 'Neplatn� konvencia volania funkcie ''%s'''),

      (ID: 201; Description: 'Volanie ''%s'' sa nepodarilo: ''%s'''),

      (ID: 301; Description: 'V�raz'),
      (ID: 302; Description: 'Identifik�tor'),
      (ID: 303; Description: 'Deklar�cia'),
      (ID: 304; Description: 'koniec s�boru'),
      (ID: 305; Description: 'deklar�cia triedy'),

      (ID: 401; Description: 'Nebola n�jden� implement�cia jednotky')
    );

implementation

end.
