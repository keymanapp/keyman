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

Contributor(s): Jaromir Solar (jarda@foresta.cz).

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : JVCL implemetation of Delphi design-time packages

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'Ok';
  SCancel           = 'Storno';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor pol�...';

  SDEAddItem          = '&P�idat pole...';
  SDEDeleteItem       = '&Smazat';
  SDESelectAllItem    = 'Vybrat &v�e';
  SDENewItem          = '&Nov� pole...';

  SDEAddFieldsCaption = 'P�idat pole';
  SDEAvailableFields  = 'Dostupn� pole';

  SDENewFieldCaption    = 'Nov� pole';
  SDEFieldProperties    = 'Vlastnosti pole';
  SDEFieldNameLabel     = '&Jm�no:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = '&Komponenta:';
  SDEFieldSizeLabel     = '&Velikost:';
  SDEFieldKind          = 'Typ pole';
  SDELookupGroup        = 'Definice vyhled�v�n�';
  SDEKeyFieldsLabel     = 'K&l��ov� pole:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Kl��e pro v&yhled�v�n�:';
  SDEResultFieldLabel   = '&V�sledn� pole:';
  SDEFieldKindItems     = '&Data'#13'&Po��tan�'#13'&Vyhled�van�';
  SDEFieldTypeMustBeSpecified = 'Mus� b�t zad�n typ pole';

  SDBGridColEditor    = 'Editor &sloupc�...';

 { Collection Editor }
  SCEEditCollection     = 'Editace %s';
  SCEAdd                = '&P�idat';
  SCEDelete             = '&Smazat';
  SCEMoveUp             = 'P�esunout &nahoru';
  SCEMoveDown           = 'P�esunout &dol�';
  SCESelectAllItem      = 'Vy&brat v�e';

 { Picture Editor }
  SPELoad               = '&Na��st...';
  SPESave               = '&Ulo�it...';
  SPEClear              = '&Vy�istit';
  SPECopy               = '&Kop�rovat';
  SPEPaste              = 'V&lo�it';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh�� &menu';
  SMDInsertItem         = '&Vlo�it';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvo�it &podmenu';

implementation

end.
