{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_all.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvInterpreter_all;

interface

implementation

uses
  JvInterpreter_System, JvInterpreter_SysUtils, JvInterpreter_Classes, JvInterpreter_Graphics,
  JvInterpreter_Controls, JvInterpreter_Dialogs, JvInterpreter_JvRegAuto,
  {$IFDEF COMPLIB_VCL}
  JvInterpreter_Windows,
  JvInterpreter_StdCtrls, JvInterpreter_ComCtrls, JvInterpreter_ExtCtrls, JvInterpreter_Forms,
  JvInterpreter_Menus, JvInterpreter_Grids,
  {$IFNDEF DelphiPersonalEdition}
  JvInterpreter_Db, JvInterpreter_DBTables, JvInterpreter_DbCtrls, JvInterpreter_DbGrids,
  JvInterpreter_Quickrpt,
  {$ENDIF}
  JvInterpreter_JvEditor,
  JvInterpreterFm,
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  JvInterpreter_Types,
  {$ENDIF COMPLIB_CLX}
  JvInterpreter;

initialization
  JvInterpreter_System.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_SysUtils.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Classes.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_JvRegAuto.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  {$IFDEF COMPLIB_VCL}
  JvInterpreter_Windows.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Graphics.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Controls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreter_StdCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_ComCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_ExtCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Forms.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Dialogs.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Menus.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Grids.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  {$IFNDEF DelphiPersonalEdition}
  JvInterpreter_Db.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_DBTables.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_DbCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_DbGrids.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreter_Quickrpt.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  {$ENDIF}

  JvInterpreter_JvEditor.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreterFm.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  {$ENDIF COMPLIB_VCL}

end.

