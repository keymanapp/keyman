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

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Odstr�ni� %s?';
  SDeleteNode2            = 'Odstr�ni� %s (so v�etk�mi podriaden�mi uzlami)?';
  SMasterFieldEmpty       = 'Vlastnos� "MasterField" mus� by� vyplnen�';
  SDetailFieldEmpty       = 'Vlastnos� "DetailField" mus� by� vyplnen�';
  SItemFieldEmpty         = 'Vlastnos� "ItemField" mus� by� vyplnen�';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" musia by� rovnak�ho typu';
  SMasterFieldError       = 'Vlastnos� "MasterField" mus� by� cel� ��slo';
  SDetailFieldError       = 'Vlastnos� "DetailField" mus� by� cel� ��slo';
  SItemFieldError         = 'Vlastnos� "ItemField" mus� by� re�azec, d�tum alebo cel� ��slo';
  SIconFieldError         = 'Vlastnos� "IconField" mus� by� cel� ��slo';
  SMoveToModeError        = 'Neplatn� sp�sob presunu pre RADBTreeNode';
  SDataSetNotActive       = 'DataSet nie je akt�vny';

implementation

end.
