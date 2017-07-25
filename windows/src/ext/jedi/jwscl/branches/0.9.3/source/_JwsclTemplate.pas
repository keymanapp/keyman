{
Description
Project JEDI Windows Security Code Library (JWSCL)

<Description here>

Author
<Author name>

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU Lesser General Public License (the  "LGPL License"), in which case the
provisions of the LGPL License are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the LGPL License and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL
License.  If you do not delete the provisions above, a recipient may use
your version of this file under either the MPL or the LGPL License.

For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

Note
The Original Code is Jwscl<UnitName>.pas.

The Initial Developer of the Original Code is <Author Name>


}
unit _JwsclTemplate;

{$INCLUDE ..\includes\Jwscl.inc}

interface
uses
  JwaWindows,      //JEDI API unit
  //more custom units here
  Classes,
  SysUtils,

  //more  JWSCL units here
  JwsclUtils,      //utility functions like thread, localization, memory, exception utils
  JwsclExceptions, //exception classes
  JwsclResource,   //resource strings
  JwsclConstants,  //all JWSCL constant
  JwsclTypes,      //JWSCL types
  JwsclStrings;    //TJwString, TJwPChar, JwCompareString, JwFormatString, LoadLocalizedString

implementation

{

}

end.
