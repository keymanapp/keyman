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

Contributor(s): KNIPPER John (knipjo@altavista.net)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for French


Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Effacer %s ?';
  SDeleteNode2            = 'Effacer %s (avec tous les fils) ?';
  SMasterFieldEmpty       = 'La propri�t� "MasterField" doit �tre renseign�';
  SDetailFieldEmpty       = 'La propri�t� "DetailField" doit �tre renseign�';
  SItemFieldEmpty         = 'La propri�t� "ItemField" doit �tre renseign�';
  SMasterDetailFieldError = '"MasterField" et "DetailField" doivent �tre du m�me type';
  SMasterFieldError       = '"MasterField" doit �tre un entier';
  SDetailFieldError       = '"DetailField" doit �tre un entier';
  SItemFieldError         = '"ItemField" doit �tre une cha�ne, une date ou un entier';
  SIconFieldError         = '"IconField" doit �tre un entier';
  SMoveToModeError        = 'Mode de d�placement invalide pour un RADBTreeNode';
  SDataSetNotActive       = 'Ensemble de donn�es inactif';

implementation

end.


