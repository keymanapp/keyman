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

description : Language specific constant for Czech

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Smazat %s ?';
  SDeleteNode2            = 'Smazat %s (se v�emi pod��zen�mi uzly) ?';
  SMasterFieldEmpty       = 'Vlastnost "MasterField" mus� b�t vypln�na';
  SDetailFieldEmpty       = 'Vlastnost "DetailField" mus� b�t vypln�na';
  SItemFieldEmpty         = 'Vlastnost "ItemField" mus� b�t vypln�na';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" mus� b�t stejn�ho typu';
  SMasterFieldError       = 'Vlastnost "MasterField" mus� b�t cel� ��slo';
  SDetailFieldError       = 'Vlastnost "DetailField" mus� b�t cel� ��slo';
  SItemFieldError         = 'Vlastnost "ItemField" mus� b�t �et�zec, datum nebo cel� ��slo';
  SIconFieldError         = 'Vlastnost "IconField" mus� b�t cel� ��slo';
  SMoveToModeError        = 'Chybn� p�esun uzlu v RADBTreeNode';
  SDataSetNotActive       = 'DataSet nen� aktivn�';

implementation

end.
