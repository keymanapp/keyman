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

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Zru�i�';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor pol�...';

  SDEAddItem          = '&Prida� pole...';
  SDEDeleteItem       = '&Odstr�ni�';
  SDESelectAllItem    = 'Vy&bra� v�etko';
  SDENewItem          = '&Nov� pole...';

  SDEAddFieldsCaption = 'Prida� pole';
  SDEAvailableFields  = 'Dostupn� polia';

  SDENewFieldCaption    = 'Nov� pole';
  SDEFieldProperties    = 'Vlastnosti po�a';
  SDEFieldNameLabel     = '&N�zov:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = 'K&omponent:';
  SDEFieldSizeLabel     = '&Ve�kos�:';
  SDEFieldKind          = 'Typ po�a';
  SDELookupGroup        = 'Defin�cia vyh�ad�vania';
  SDEKeyFieldsLabel     = '&K���ov� polia:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'K���e pre v&yh�ad�vanie:';
  SDEResultFieldLabel   = '&V�sledn� pole:';
  SDEFieldKindItems     = '&D�ta'#13'&Po��tan�'#13'&Vyh�ad�van�';
  SDEFieldTypeMustBeSpecified = 'Typ po�a mus� by� zadan�';

  SDBGridColEditor    = 'Editor &st�pcov...';

 { Collection Editor }
  SCEEditCollection     = 'Edit�cia %s';
  SCEAdd                = '&Prida�';
  SCEDelete             = 'O&dstr�ni�';
  SCEMoveUp             = 'Presun�� &hore';
  SCEMoveDown           = 'Presun�� do&le';
  SCESelectAllItem      = 'Vy&bra� v�etko';

 { Picture Editor }
  SPELoad               = '&Na��ta�...';
  SPESave               = '&Ulo�i�...';
  SPEClear              = '&Vymaza�';
  SPECopy               = '&Kop�rova�';
  SPEPaste              = '&Prilepi�';

 { Menu Designer }
  SMDMenuDesigner       = 'N�vrh &menu';
  SMDInsertItem         = '&Vlo�i�';
  SMDDeleteItem         = 'O&dstr�ni�';
  SMDCreateSubmenuItem  = 'Vytvori� &podmenu';

implementation

end.
