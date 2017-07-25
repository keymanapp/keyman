{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit contains constants that are used by the units of JWSCL.

Author
Christian Wimmer

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

The Original Code is JwsclConstants.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.



}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclConstants;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  JwsclResource,
  jwaWindows,
  JwsclTypes,
  JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}

{Here we include resource strings and other things}


{WARNING:
Create new (empty) project and add this line in the main unit:

Open taskmanager and add the Virtual Memory Size column via the View -> Select Columns menu. Record the current value of VM Size column.

Compile and when the compilation has finished you can see 34,1 MB extra Virtual Memory is allocated.

Compile again, 34,1 MB more is allocated. This continues with every compilation.

Memory is not freed when closing the project, only restarting the IDE helps.

http://qc.codegear.com/wc/qcmain.aspx?d=57701
}
{$IFDEF JWSCL_INCLUDE_RES}
{.$R Jwscl.res}
{$ENDIF JWSCL_INCLUDE_RES}


const
  TAceTypeString : array[TJwAceType] of TJwString =
    (
    RsAceTypeStringAudit,//actAudit,
    RsAceAuditCallback,//actAuditCallback,
    RsAceAuditObject,//actAuditObject,
    RsAceAuditCallbackObject,//actAuditCallbackObject,

    RsAceMandatory,//actMandatory,

    RsAceTypeStringAllow,//actAllow,
    RsAceAllowCallback,//actAllowCallback,
    RsAceAllowObject,//actAllowObject,
    RsAceAllowCallbackObject,//actAllowCallbackObject,

    RsAceTypeStringDeny,//actDeny,
    RsAceDenyCallback,//actDenyCallback,
    RsAceDenyObject,//actDenyObject,
    RsAceDenyCallbackObject,//actDenyCallbackObject

    RsAceUnknown
    );



  IDAPPLY = 33;
  FILE_DELETE = Delete; //{0x00010000L}

  JwAllSecurityAccess = READ_CONTROL or WRITE_DAC or WRITE_OWNER or SYNCHRONIZE;

    {<B>SM_SERVERR2</B> is used as an additional constant for GetSystemMetrics
     to detect the second release of Win2003
    }
  SM_SERVERR2 = 98;


  { operating system (OS)constants }
  cOsUnknown = -1; //The system is unknown
  cOsWin95   = 0;  //The system is a Windows 95
  cOsWin98   = 1;  //The system is a Windows 98
  cOsWin98SE = 2;  //The system is a Windows 98 Second Edition
  cOsWinME   = 3;  //The system is a ME
  cOsWinNT   = 4;  //The system is a NT
  cOsWin2000 = 5;  //The system is a Windows 2000
  cOsXP      = 6;  //The system is a XP
  cOS2003    = 7;  //The system is a 2003 Server
  cOS2003R2  = 8;  //The system is a 2003 Server Release 2
  cOSXP64    = 9;  //The system is a XP 64 bit version
  cOsVista   = 10; //The system is a Vista
  cOsWin2008 = 11; //The system is a 2008 Server (tested with RC)
  cOsWin7    = 12; //The system is a Win7
  //cOsWinXXX  = cOsWin7; //use this to set proper name for this OS!

  {<B>sOSVerString</B> contains the windows version as text}
  sOSVerString: array[-1..15] of TJwString =
    ('Unknown',
    'Windows 95',
    'Windows 98',
    'Windows 98 Second Edition',
    'Windows ME',
    'Windows NT',
    'Windows 2000',
    'Windows XP',
    'Windows 2003',
    'Windows 2003 Release 2',
    'Windows XP 64 Edition',
    'Windows Vista',
    'Windows 2008',
    'Windows 7',
    '',
    '',
    ''
    );




  {<B>ALL_SECURITY_INFORMATION</B> can be used in TJwSecurityDescriptor.getStringSid as the parameter value to
   get all SID information at once.
   This value is defined as zero and also as a or combination of the given values.
  }
  ALL_SECURITY_INFORMATION = {= 0 =}
    OWNER_SECURITY_INFORMATION or
    GROUP_SECURITY_INFORMATION or
    DACL_SECURITY_INFORMATION or
    SACL_SECURITY_INFORMATION or
    LABEL_SECURITY_INFORMATION;


  //<B>LUID_INVALID</B> defines a LUID structure that is invalid
  LUID_INVALID: TLuid = (LowPart: 0; HighPart: -1);
  //<B>LUID_NULL</B> defines a LUID structure that is undefined
  LUID_NULL: TLuid = (LowPart: 0; HighPart: 0);

  //<B>LUID_A_INVALID</B> defines a Luid_and_attributes structure that is invalid
  LUID_A_INVALID: TLuidAndAttributes =
    (Luid: (LowPart: 0; HighPart: -1); Attributes: 0);
  //<B>LUID_A_NULL</B> defines a Luid_and_attributes structure that is undefined
  LUID_A_NULL: TLuidAndAttributes =
    (Luid: (LowPart: 0; HighPart: 0); Attributes: 0);


 
const
  JwSecurityInformationAllFlags =
    [siOwnerSecurityInformation,
    siGroupSecurityInformation,
    siDaclSecurityInformation,
    siSaclSecurityInformation];
  JwSecurityInformationAllFlagsOGDS = JwSecurityInformationAllFlags;

  JwSecurityInformationACLFlagsDS =
    [siDaclSecurityInformation,
    siSaclSecurityInformation];

  JwSecurityInformationACLFlagsODS =
    [siOwnerSecurityInformation,
    siDaclSecurityInformation,
    siSaclSecurityInformation];

  JwSecurityInformationACLFlagsOD =
    [siOwnerSecurityInformation,
    siDaclSecurityInformation];


  JwSecurityInformationACLFlagsOG =
    [siOwnerSecurityInformation,
    siGroupSecurityInformation];


  JwSecurityInformationProtectedACLFlags =
    [siProtectedDaclSecurityInformation,
    siProtectedSaclSecurityInformation];

  JwSecurityInformationUnprotectedACLFlags =
    [siUnprotectedDaclSecurityInformation,
    siUnprotectedSaclSecurityInformation];


var
  FileMapping: array[1..22] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_REQUIRED;
    Name: 'STANDARD_RIGHTS_REQUIRED'; Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE;
    Name: 'STANDARD_RIGHTS_EXECUTE'; Flags: 0),

    (Right: $1FF; Name: '$1FF';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_DATA; Name: 'FILE_READ_DATA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES; Name: 'FILE_READ_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_EA; Name: 'FILE_READ_EA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SYNCHRONIZE; Name: 'SYNCHRONIZE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_DATA; Name: 'FILE_WRITE_DATA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_ATTRIBUTES; Name: 'FILE_WRITE_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_EA; Name: 'FILE_WRITE_EA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_APPEND_DATA; Name: 'FILE_APPEND_DATA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES; Name: 'FILE_READ_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_EXECUTE; Name: 'FILE_EXECUTE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_APPEND_DATA; Name: 'FILE_APPEND_DATA';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: ACCESS_SYSTEM_SECURITY; Name: 'ACCESS_SYSTEM_SECURITY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SYNCHRONIZE; Name: 'SYNCHRONIZE';
    Flags: SI_ACCESS_SPECIFIC)
    );           


  FileGenericMapping: TGenericMapping =
    (GenericRead: FILE_GENERIC_READ;
    //STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
    GenericWrite: FILE_GENERIC_WRITE;
    //STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE;
    GenericExecute: FILE_GENERIC_EXECUTE;
    //STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;
    GenericAll: FILE_ALL_ACCESS;
    //STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF;
    );


  FolderMapping: array[1..19] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_REQUIRED;
    Name: 'STANDARD_RIGHTS_REQUIRED'; Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE;
    Name: 'STANDARD_RIGHTS_EXECUTE'; Flags: 0),

    (Right: FILE_LIST_DIRECTORY; Name: 'FILE_LIST_DIRECTORY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_FILE; Name: 'FILE_ADD_FILE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_SUBDIRECTORY; Name: 'FILE_ADD_SUBDIRECTORY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_EA; Name: 'FILE_READ_EA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_EA; Name: 'FILE_WRITE_EA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_TRAVERSE; Name: 'FILE_TRAVERSE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_DELETE_CHILD; Name: 'FILE_DELETE_CHILD';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES; Name: 'FILE_READ_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_ATTRIBUTES; Name: 'FILE_WRITE_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: ACCESS_SYSTEM_SECURITY; Name: 'ACCESS_SYSTEM_SECURITY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SYNCHRONIZE; Name: 'SYNCHRONIZE';
    Flags: SI_ACCESS_SPECIFIC)
    );


  FolderGenericMapping: TGenericMapping =
    (GenericRead: STANDARD_RIGHTS_READ or READ_CONTROL or
    SYNCHRONIZE or FILE_READ_EA or
    FILE_READ_DATA or FILE_READ_EA;
    GenericWrite: STANDARD_RIGHTS_WRITE or WRITE_DAC or
    WRITE_OWNER or Delete or FILE_ADD_FILE or
    FILE_ADD_SUBDIRECTORY or FILE_WRITE_EA or
    FILE_DELETE_CHILD or
    FILE_WRITE_ATTRIBUTES or SYNCHRONIZE;
    GenericExecute: STANDARD_RIGHTS_EXECUTE or
    SYNCHRONIZE or FILE_LIST_DIRECTORY or
    FILE_TRAVERSE;
    GenericAll: FILE_ALL_ACCESS;
    );


  FileFolderMapping: array[1..19] of TJwRightsMapping =
    (

    (Right: FILE_ALL_ACCESS; Name: 'Full control';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_WRITE or FILE_GENERIC_EXECUTE or Delete;
    Name: 'Modify';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
    Name: 'Read and execute';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
    Name: 'List folder contents';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ;
    Name: 'Read';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_WRITE and not READ_CONTROL;
    Name: 'Write'; Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_TRAVERSE or FILE_EXECUTE;
    Name: 'Traverse Folder / Execute File';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_LIST_DIRECTORY or FILE_READ_DATA;
    Name: 'List Folder / Read Data'; Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES;
    Name: 'Read Attributes';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_EA;
    Name: 'Read Extended Attributes';     //10
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_FILE or FILE_WRITE_DATA;
    Name: 'Create Files / Write Data';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_SUBDIRECTORY or FILE_APPEND_DATA;
    Name: 'Create Folders / Append Data'; Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_ATTRIBUTES;
    Name: 'Write Attributes';      //13
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_EA;
    Name: 'Write Extended Attributes';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_DELETE_CHILD;
    Name: 'Delete Subfolders and Files';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_DELETE;
    Name: 'Delete';                //16
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC;
    Name: 'Change Permissions';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER;
    Name: 'Take Ownership';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL;
    Name: 'Read Permissions';   //19
    Flags: SI_ACCESS_SPECIFIC)
    );

  FileFolderMappingEx: array[1..19] of TJwRightsMapping =
    (

    (Right: FILE_ALL_ACCESS; Name: 'FILE_ALL_ACCESS';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_WRITE or FILE_GENERIC_EXECUTE or Delete;
    Name: 'FILE_GENERIC_READ or FILE_GENERIC_WRITE or FILE_GENERIC_EXECUTE or Delet';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
    Name: 'SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ or FILE_GENERIC_EXECUTE;
    Name: 'FILE_GENERIC_READ or FILE_GENERIC_EXECUTE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_READ;
    Name: 'FILE_GENERIC_READ';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_GENERIC_WRITE and not READ_CONTROL;
    Name: 'FILE_GENERIC_WRITE and not READ_CONTROL'; Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: FILE_TRAVERSE or FILE_EXECUTE;
    Name: 'FILE_TRAVERSE or FILE_EXECUTE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_LIST_DIRECTORY or FILE_READ_DATA;
    Name: 'FILE_LIST_DIRECTORY or FILE_READ_DATA'; Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES;
    Name: 'FILE_READ_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_EA;
    Name: 'FILE_READ_EA';     //10
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_FILE or FILE_WRITE_DATA;
    Name: 'CheckListBox1.ItemIndex';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_ADD_SUBDIRECTORY or FILE_APPEND_DATA;
    Name: 'FILE_ADD_SUBDIRECTORY or FILE_APPEND_DATA'; Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_ATTRIBUTES;
    Name: 'FILE_WRITE_ATTRIBUTES';      //13
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_EA;
    Name: 'FILE_WRITE_EA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_DELETE_CHILD;
    Name: 'FILE_DELETE_CHILD';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_DELETE;
    Name: 'FILE_DELETE';                //16
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC;
    Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER;
    Name: 'SI_ACCESS_SPECIFIC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL;
    Name: 'READ_CONTROL';   //19
    Flags: SI_ACCESS_SPECIFIC)
    );


  FileFolderGenericMapping: TGenericMapping =
    (GenericRead: STANDARD_RIGHTS_READ or READ_CONTROL or
    SYNCHRONIZE or FILE_READ_EA or
    FILE_READ_DATA or FILE_READ_EA;
    GenericWrite: STANDARD_RIGHTS_WRITE or WRITE_DAC or
    WRITE_OWNER or Delete or FILE_ADD_FILE or
    FILE_ADD_SUBDIRECTORY or FILE_WRITE_EA or
    FILE_DELETE_CHILD or FILE_WRITE_ATTRIBUTES or
    SYNCHRONIZE;
    GenericExecute: STANDARD_RIGHTS_EXECUTE or
    SYNCHRONIZE or FILE_LIST_DIRECTORY or
    FILE_TRAVERSE;
    GenericAll: FILE_ALL_ACCESS;
    );



  WinStationMapping: array[1..17] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_REQUIRED;
    Name: 'STANDARD_RIGHTS_REQUIRED'; Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE;
    Name: 'STANDARD_RIGHTS_EXECUTE'; Flags: 0),

    (Right: WINSTA_ACCESSCLIPBOARD; Name: 'WINSTA_ACCESSCLIPBOARD';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_ACCESSGLOBALATOMS;
    Name: 'WINSTA_ACCESSGLOBALATOMS'; Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_CREATEDESKTOP; Name: 'WINSTA_CREATEDESKTOP';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_ENUMDESKTOPS; Name: 'WINSTA_ENUMDESKTOPS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_ENUMERATE; Name: 'WINSTA_ENUMERATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_EXITWINDOWS; Name: 'WINSTA_EXITWINDOWS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_READATTRIBUTES; Name: 'WINSTA_READATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_READSCREEN; Name: 'WINSTA_READSCREEN';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WINSTA_WRITEATTRIBUTES; Name: 'WINSTA_WRITEATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC)
     {    (Right: ACCESS_SYSTEM_SECURITY;      Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;                 Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)     }
    );

  WinStationGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or      }
    WINSTA_ENUMDESKTOPS or
    WINSTA_ENUMERATE or WINSTA_READATTRIBUTES or
    WINSTA_READSCREEN;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or {ACCESS_SYSTEM_SECURITY or }
    {STANDARD_RIGHTS_WRITE or}
    WINSTA_ACCESSCLIPBOARD or WINSTA_CREATEDESKTOP or
    WINSTA_WRITEATTRIBUTES;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or  }
    WINSTA_ACCESSGLOBALATOMS or WINSTA_EXITWINDOWS;
    GenericAll: Cardinal(
    -1) //set to Sum of all Generic rights in TJwSecurityUserMapping.GetMapping
    );

  DesktopMapping: array[1..17] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: DESKTOP_ENUMERATE; Name: 'DESKTOP_ENUMERATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_READOBJECTS; Name: 'DESKTOP_READOBJECTS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_CREATEMENU; Name: 'DESKTOP_CREATEMENU';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_CREATEWINDOW; Name: 'DESKTOP_CREATEWINDOW';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_HOOKCONTROL; Name: 'DESKTOP_HOOKCONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_JOURNALPLAYBACK; Name: 'DESKTOP_JOURNALPLAYBACK';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_JOURNALRECORD; Name: 'DESKTOP_JOURNALRECORD';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_WRITEOBJECTS; Name: 'DESKTOP_WRITEOBJECTS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: DESKTOP_SWITCHDESKTOP; Name: 'DESKTOP_SWITCHDESKTOP';
    Flags: SI_ACCESS_SPECIFIC),


    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC)
     {    (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)   }
    );

  DesktopGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or }READ_CONTROL or
    {SYNCHRONIZE or  }

    DESKTOP_ENUMERATE or DESKTOP_READOBJECTS;
    GenericWrite: {STANDARD_RIGHTS_WRITE or }WRITE_DAC or
    WRITE_OWNER or Delete or {ACCESS_SYSTEM_SECURITY or }
    {STANDARD_RIGHTS_WRITE or }
    DESKTOP_CREATEMENU or
    DESKTOP_CREATEWINDOW or DESKTOP_HOOKCONTROL or DESKTOP_JOURNALPLAYBACK or
    DESKTOP_JOURNALRECORD or DESKTOP_WRITEOBJECTS;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or}
    DESKTOP_SWITCHDESKTOP;
    GenericAll: Cardinal(-1)
    );



  RegistryMapping: array[1..16 - 2] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL 1';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ 2';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE;
    Name: 'STANDARD_RIGHTS_EXECUTE'; Flags: 0),

    (Right: KEY_QUERY_VALUE; Name: 'KEY_QUERY_VALUE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: KEY_ENUMERATE_SUB_KEYS; Name: 'KEY_ENUMERATE_SUB_KEYS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: KEY_NOTIFY; Name: 'KEY_NOTIFY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: KEY_CREATE_SUB_KEY; Name: 'KEY_CREATE_SUB_KEY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: KEY_CREATE_LINK; Name: 'KEY_CREATE_LINK';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: KEY_SET_VALUE; Name: 'KEY_SET_VALUE';
    Flags: SI_ACCESS_SPECIFIC),


    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC)
       {  (Right: ACCESS_SYSTEM_SECURITY;      Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;                 Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)       }
    );

  RegistryGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    SYNCHRONIZE or KEY_READ;
    //STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY  and (not SYNCHRONIZE);
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or KEY_WRITE;
    //STANDARD_RIGHTS_WRITE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY and (not SYNCHRONIZE)
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or}
    KEY_EXECUTE;    //KEY_READ and  (not SYNCHRONIZE);
    GenericAll: KEY_ALL_ACCESS;
    //STANDARD_RIGHTS_ALL or KEY_QUERY_VALUE or KEY_SET_VALUE or  KEY_CREATE_SUB_KEY or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or   KEY_CREATE_LINK and (not SYNCHRONIZE);
    );



  ServiceMapping: array[1..20 - 2] of TJwRightsMapping =
    ((Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: SERVICE_ALL_ACCESS;
    Name: 'SERVICE_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_CHANGE_CONFIG;
    Name: 'SERVICE_CHANGE_CONFIG';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_ENUMERATE_DEPENDENTS;
    Name: 'SERVICE_ENUMERATE_DEPENDENTS'; Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_INTERROGATE;
    Name: 'SERVICE_INTERROGATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_PAUSE_CONTINUE;
    Name: 'SERVICE_PAUSE_CONTINUE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_QUERY_CONFIG;
    Name: 'SERVICE_QUERY_CONFIG';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_QUERY_STATUS;
    Name: 'SERVICE_QUERY_STATUS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_START; Name: 'SERVICE_START';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_STOP; Name: 'SERVICE_STOP';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SERVICE_USER_DEFINED_CONTROL;
    Name: 'SERVICE_USER_DEFINED_CONTROL'; Flags: SI_ACCESS_SPECIFIC),


    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC)
      {   (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)    }
    );

  ServiceGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or }READ_CONTROL or
    {SYNCHRONIZE or  }
    SERVICE_QUERY_CONFIG or
    SERVICE_QUERY_STATUS or
    SERVICE_INTERROGATE or
    SERVICE_ENUMERATE_DEPENDENTS;
    GenericWrite: {STANDARD_RIGHTS_WRITE or }WRITE_DAC or
    WRITE_OWNER or Delete or SERVICE_CHANGE_CONFIG;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or  }
    SERVICE_START or
    SERVICE_STOP or SERVICE_PAUSE_CONTINUE or
    SERVICE_USER_DEFINED_CONTROL;
    GenericAll: Cardinal(-1);
    );



  SCManagerMapping: array[1..17 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: SC_MANAGER_ALL_ACCESS;
    Name: 'SC_MANAGER_ALL_ACCESS';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_CREATE_SERVICE;
    Name: 'SC_MANAGER_CREATE_SERVICE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_CONNECT; Name: 'SC_MANAGER_CONNECT';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_ENUMERATE_SERVICE;
    Name: 'SC_MANAGER_ENUMERATE_SERVICE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_LOCK; Name: 'SC_MANAGER_LOCK';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_MODIFY_BOOT_CONFIG;
    Name: 'SC_MANAGER_MODIFY_BOOT_CONFIG';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SC_MANAGER_QUERY_LOCK_STATUS;
    Name: 'SC_MANAGER_QUERY_LOCK_STATUS';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)  }
    );

  SCManagerGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or }READ_CONTROL or
    {SYNCHRONIZE or   }
    SC_MANAGER_ENUMERATE_SERVICE or
    SC_MANAGER_QUERY_LOCK_STATUS;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or
    SC_MANAGER_CREATE_SERVICE or
    SC_MANAGER_MODIFY_BOOT_CONFIG;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or} SYNCHRONIZE or
    SC_MANAGER_CONNECT or
    SC_MANAGER_LOCK;
    GenericAll: SC_MANAGER_ALL_ACCESS;
    );


const
  SERVER_ACCESS_ADMINISTER = 00000001;
  SERVER_ACCESS_ENUMERATE  = 00000002;

  PRINTER_ACCESS_ADMINISTER = 00000004;
  PRINTER_ACCESS_USE        = 00000008;

  JOB_ACCESS_ADMINISTER = 00000010;
  JOB_ACCESS_READ       = 00000020;




  SERVER_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or
    SERVER_ACCESS_ADMINISTER or
    SERVER_ACCESS_ENUMERATE;

  SERVER_READ = STANDARD_RIGHTS_READ or
    SERVER_ACCESS_ENUMERATE;

  SERVER_WRITE = STANDARD_RIGHTS_WRITE or
    SERVER_ACCESS_ADMINISTER or
    SERVER_ACCESS_ENUMERATE;

  SERVER_EXECUTE = STANDARD_RIGHTS_EXECUTE or
    SERVER_ACCESS_ENUMERATE;



  PRINTER_ALL_ACCESS =
    STANDARD_RIGHTS_REQUIRED or
    PRINTER_ACCESS_ADMINISTER or
    PRINTER_ACCESS_USE;

  PRINTER_READ =
    STANDARD_RIGHTS_READ or PRINTER_ACCESS_USE;

  PRINTER_WRITE =
    STANDARD_RIGHTS_WRITE or PRINTER_ACCESS_USE;

  PRINTER_EXECUTE = STANDARD_RIGHTS_EXECUTE;

var
  PrinterMapping: array[1..16 - 2] of TJwRightsMapping =
    (
    (Right: PRINTER_ALL_ACCESS; Name: 'PRINTER_ALL_ACCESS';
    Flags: SI_ACCESS_GENERAL),
    (Right: PRINTER_READ; Name: 'PRINTER_READ';
    Flags: SI_ACCESS_GENERAL),
    (Right: PRINTER_WRITE; Name: 'PRINTER_WRITE';
    Flags: SI_ACCESS_GENERAL),
    (Right: PRINTER_EXECUTE; Name: 'PRINTER_EXECUTE';
    Flags: SI_ACCESS_GENERAL),

    (Right: PRINTER_ALL_ACCESS; Name: 'PRINTER_ALL_ACCESS';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SERVER_ACCESS_ADMINISTER;
    Name: 'SERVER_ACCESS_ADMINISTER';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: SERVER_ACCESS_ENUMERATE;
    Name: 'SERVER_ACCESS_ENUMERATE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: PRINTER_ACCESS_ADMINISTER;
    Name: 'PRINTER_ACCESS_ADMINISTER';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: PRINTER_ACCESS_USE; Name: 'PRINTER_ACCESS_USE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: JOB_ACCESS_ADMINISTER; Name: 'JOB_ACCESS_ADMINISTER';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)  }
    );

  PrinterGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or }READ_CONTROL
    {or SYNCHRONIZE} or PRINTER_READ;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PRINTER_WRITE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or} SYNCHRONIZE or
    PRINTER_EXECUTE;
    GenericAll: PRINTER_ALL_ACCESS;
    );


  ShareMapping: array[1..13 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: PERM_FILE_READ; Name: 'PERM_FILE_READ';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: PERM_FILE_WRITE; Name: 'PERM_FILE_WRITE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),
    (Right: PERM_FILE_CREATE; Name: 'PERM_FILE_CREATE';
    Flags: SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  ShareGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PERM_FILE_READ;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PERM_FILE_CREATE or PERM_FILE_WRITE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: Cardinal(-1);
    );




  ProcessMapping: array[1..22 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: PROCESS_ALL_ACCESS; Name: 'PROCESS_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_TERMINATE; Name: 'PROCESS_TERMINATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_CREATE_THREAD; Name: 'PROCESS_CREATE_THREAD';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_SET_SESSIONID; Name: 'PROCESS_SET_SESSIONID';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_VM_OPERATION; Name: 'PROCESS_VM_OPERATION';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_VM_READ; Name: 'PROCESS_VM_READ';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: PROCESS_VM_WRITE; Name: 'PROCESS_VM_WRITE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_DUP_HANDLE; Name: 'PROCESS_DUP_HANDLE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_CREATE_PROCESS; Name: 'PROCESS_CREATE_PROCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_SET_QUOTA; Name: 'PROCESS_SET_QUOTA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_SET_INFORMATION; Name: 'PROCESS_SET_INFORMATION';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: PROCESS_QUERY_INFORMATION;
    Name: 'PROCESS_QUERY_INFORMATION'; Flags: SI_ACCESS_SPECIFIC),


    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  ProcessGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL
    {or SYNCHRONIZE} or PROCESS_QUERY_INFORMATION or PROCESS_VM_READ;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or PROCESS_CREATE_THREAD or
    PROCESS_CREATE_THREAD or
    PROCESS_SET_SESSIONID or PROCESS_VM_OPERATION or
    PROCESS_SET_INFORMATION or PROCESS_DUP_HANDLE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: PROCESS_ALL_ACCESS;
    );



  ThreadMapping: array[1..20 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: THREAD_ALL_ACCESS; Name: 'THREAD_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_TERMINATE; Name: 'THREAD_TERMINATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_SUSPEND_RESUME; Name: 'THREAD_SUSPEND_RESUME';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_GET_CONTEXT; Name: 'THREAD_GET_CONTEXT';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_SET_CONTEXT; Name: 'THREAD_SET_CONTEXT';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_SET_INFORMATION; Name: 'THREAD_SET_INFORMATION';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: THREAD_QUERY_INFORMATION;
    Name: 'THREAD_QUERY_INFORMATION'; Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_SET_THREAD_TOKEN;
    Name: 'THREAD_SET_THREAD_TOKEN'; Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_IMPERSONATE; Name: 'THREAD_IMPERSONATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: THREAD_DIRECT_IMPERSONATION;
    Name: 'THREAD_DIRECT_IMPERSONATION'; Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)    }
    );

  ThreadGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PROCESS_QUERY_INFORMATION or
    THREAD_QUERY_INFORMATION or THREAD_GET_CONTEXT;
    GenericWrite:{ STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    THREAD_TERMINATE or
    THREAD_SUSPEND_RESUME or THREAD_SET_CONTEXT or THREAD_SET_INFORMATION or
    THREAD_SET_THREAD_TOKEN;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or  }
    THREAD_SUSPEND_RESUME or
    THREAD_IMPERSONATE or THREAD_DIRECT_IMPERSONATION;
    GenericAll: THREAD_ALL_ACCESS;
    );


  JobMapping: array[1..16 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: JOB_OBJECT_ALL_ACCESS;
    Name: 'JOB_OBJECT_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: JOB_OBJECT_ASSIGN_PROCESS;
    Name: 'JOB_OBJECT_ASSIGN_PROCESS';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: JOB_OBJECT_SET_ATTRIBUTES;
    Name: 'JOB_OBJECT_SET_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: JOB_OBJECT_QUERY;
    Name: 'JOB_OBJECT_QUERY';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: JOB_OBJECT_TERMINATE;
    Name: 'JOB_OBJECT_TERMINATE';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: JOB_OBJECT_SET_SECURITY_ATTRIBUTES;
    Name: 'JOB_OBJECT_SET_SECURITY_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  JobGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL
    {or SYNCHRONIZE} or PROCESS_QUERY_INFORMATION or
    JOB_OBJECT_QUERY;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    JOB_OBJECT_ASSIGN_PROCESS or
    JOB_OBJECT_SET_ATTRIBUTES or JOB_OBJECT_TERMINATE or
    JOB_OBJECT_SET_SECURITY_ATTRIBUTES;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: JOB_OBJECT_ALL_ACCESS;
    );



  SemaphoreMapping: array[1..12 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: SEMAPHORE_ALL_ACCESS; Name: 'SEMAPHORE_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SEMAPHORE_MODIFY_STATE; Name: 'SEMAPHORE_MODIFY_STATE';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  SemaphoreGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PROCESS_QUERY_INFORMATION;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    SEMAPHORE_MODIFY_STATE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: SEMAPHORE_ALL_ACCESS;
    );


  EventMapping: array[1..12 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: EVENT_ALL_ACCESS; Name: 'EVENT_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: EVENT_MODIFY_STATE; Name: 'EVENT_MODIFY_STATE';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  EventGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or }READ_CONTROL
    {or SYNCHRONIZE} or PROCESS_QUERY_INFORMATION;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    EVENT_MODIFY_STATE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: EVENT_ALL_ACCESS;
    );


  MutexMapping: array[1..12 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: MUTEX_ALL_ACCESS; Name: 'MUTEX_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: MUTEX_MODIFY_STATE; Name: 'MUTEX_MODIFY_STATE';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  MutexGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or }PROCESS_QUERY_INFORMATION;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    MUTEX_MODIFY_STATE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: MUTEX_ALL_ACCESS;
    );


  FileMapMapping: array[1..15 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: FILE_MAP_ALL_ACCESS; Name: 'FILE_MAP_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_MAP_COPY; Name: 'FILE_MAP_COPY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_MAP_WRITE; Name: 'FILE_MAP_WRITE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_MAP_READ; Name: 'FILE_MAP_READ';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: SECTION_EXTEND_SIZE; Name: 'SECTION_EXTEND_SIZE';
    Flags: SI_ACCESS_SPECIFIC),


    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC) }
    );

  FileMapGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PROCESS_QUERY_INFORMATION or
    FILE_MAP_READ;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    FILE_MAP_READ or FILE_MAP_COPY or
    SECTION_EXTEND_SIZE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: FILE_MAP_ALL_ACCESS;
    );

  TimerMapping: array[1..13 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: TIMER_ALL_ACCESS; Name: 'TIMER_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TIMER_QUERY_STATE; Name: 'TIMER_QUERY_STATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TIMER_MODIFY_STATE; Name: 'TIMER_MODIFY_STATE';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)   }
    );

  TimerGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PROCESS_QUERY_INFORMATION or
    TIMER_QUERY_STATE;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    TIMER_MODIFY_STATE;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: TIMER_ALL_ACCESS;
    );


  TokenMapping: array[1..20 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: TOKEN_ALL_ACCESS; Name: 'TOKEN_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_ASSIGN_PRIMARY; Name: 'TOKEN_ASSIGN_PRIMARY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_DUPLICATE; Name: 'TOKEN_DUPLICATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_IMPERSONATE; Name: 'TOKEN_IMPERSONATE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_QUERY; Name: 'TOKEN_QUERY';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_QUERY_SOURCE; Name: 'TOKEN_QUERY_SOURCE';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_ADJUST_PRIVILEGES; Name: 'TOKEN_ADJUST_PRIVILEGES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_ADJUST_GROUPS; Name: 'TOKEN_ADJUST_GROUPS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_ADJUST_DEFAULT; Name: 'TOKEN_ADJUST_DEFAULT';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: TOKEN_ADJUST_SESSIONID; Name: 'TOKEN_ADJUST_SESSIONID';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)  }
    );

  TokenGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL
    or{SYNCHRONIZE or} PROCESS_QUERY_INFORMATION or
    TOKEN_QUERY_SOURCE or TOKEN_QUERY;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    TOKEN_ASSIGN_PRIMARY or
    TOKEN_DUPLICATE or TOKEN_ADJUST_PRIVILEGES or
    TOKEN_ADJUST_GROUPS or
    TOKEN_ADJUST_DEFAULT or TOKEN_ADJUST_SESSIONID;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE or }
    TOKEN_IMPERSONATE;
    GenericAll: TOKEN_ALL_ACCESS;
    );

var
  PipeMapping: array[1..16 - 2] of TJwRightsMapping =
    (
    (Right: STANDARD_RIGHTS_ALL; Name: 'STANDARD_RIGHTS_ALL';
    Flags: 0; StringId : -50008),
    (Right: STANDARD_RIGHTS_READ; Name: 'STANDARD_RIGHTS_READ';
    Flags: 0),
    (Right: STANDARD_RIGHTS_WRITE; Name: 'STANDARD_RIGHTS_WRITE';
    Flags: 0),
    (Right: STANDARD_RIGHTS_EXECUTE; Name: 'STANDARD_RIGHTS_EXECUTE';
    Flags: 0),

    (Right: FILE_ALL_ACCESS; Name: 'FILE_ALL_ACCESS';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_DATA; Name: 'FILE_READ_DATA';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: FILE_WRITE_DATA; Name: 'FILE_WRITE_DATA';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_CREATE_PIPE_INSTANCE;
    Name: 'FILE_CREATE_PIPE_INSTANCE'; Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_READ_ATTRIBUTES; Name: 'FILE_READ_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: FILE_WRITE_ATTRIBUTES; Name: 'FILE_WRITE_ATTRIBUTES';
    Flags: SI_ACCESS_SPECIFIC),

    (Right: WRITE_DAC; Name: 'WRITE_DAC';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: WRITE_OWNER; Name: 'WRITE_OWNER';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: READ_CONTROL; Name: 'READ_CONTROL';
    Flags: SI_ACCESS_SPECIFIC),
    (Right: Delete; Name: 'DELETE';
    Flags: SI_ACCESS_SPECIFIC){,
         (Right: ACCESS_SYSTEM_SECURITY;    Name : 'ACCESS_SYSTEM_SECURITY';      Flags : SI_ACCESS_SPECIFIC),
         (Right: SYNCHRONIZE;               Name : 'SYNCHRONIZE';                 Flags : SI_ACCESS_SPECIFIC)     }
    );

  PipeGenericMapping: TGenericMapping =
    (GenericRead: {STANDARD_RIGHTS_READ or} READ_CONTROL or
    {SYNCHRONIZE or} PROCESS_QUERY_INFORMATION or
    FILE_READ_DATA or FILE_READ_ATTRIBUTES;
    GenericWrite: {STANDARD_RIGHTS_WRITE or} WRITE_DAC or
    WRITE_OWNER or Delete or PROCESS_TERMINATE or
    FILE_WRITE_DATA or FILE_CREATE_PIPE_INSTANCE or
    FILE_WRITE_ATTRIBUTES;
    GenericExecute: {STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE}0;
    GenericAll: FILE_ALL_ACCESS;
    );

  JwNullSecurityAttributes: TSECURITYATTRIBUTES =
   (nLength: 0;
   lpSecurityDescriptor: nil;
   bInheritHandle: False
  );

  {<B>NullGenericMapping</B> defines a mapping record that
  maps all generic rights to zero.
  It can be used to reset a dynamic mapping record (may be faster
  than call ZeroMemory).
  }
  NullGenericMapping : TGenericMapping =
    (GenericRead: 0;
    GenericWrite: 0;
    GenericExecute: 0;
    GenericAll: 0;
    );
{TODO: recheck the values of these flags (only guessed)
and move them to jwawincrypt
its vista!
}
const CRYPTPROTECTMEMORY_SAME_PROCESS = 0;
      CRYPTPROTECTMEMORY_CROSS_PROCESS = 1;
      CRYPTPROTECTMEMORY_SAME_LOGON = 2;

const
  {<B>JwSidAttributeStrings</B> converts all
  enumeration values of TJwSidAttribute into their defined names.
  }
  JwSidAttributeStrings: TJwSidAttributesStringArray =
    (
    'sidaUnknown',
    'sidaGroupMandatory',
    'sidaGroupEnabledByDefault',
    'sidaGroupEnabled',
    'sidaGroupOwner',
    'sidaGroupUseForDenyOnly',
    'sidaGroupLogonId',
    'sidaGroupResource',
    'sidaGroupIntegrity',
    'sidaGroupIntegrityEnabled',
    'sidaPad0',
    'sidaPad1',
    'sidaPad2',
    'sidaPad3',
    'sidaPad4',
    'sidaPad5'
    );

const
  {<B>JwSidAttributeHumanStrings</B> converts all
  enumeration values of TJwSidAttribute into human readable strings.
  This array uses resource strings that can be translated.
  }
  JwSidAttributeHumanStrings: TJwSidAttributesStringArray =
    (
    RsAttributeHumanString0,//'unknown',
    RsAttributeHumanString1,//'mandatory',
    RsAttributeHumanString2,//'default',
    RsAttributeHumanString3,//'enabled',
    RsAttributeHumanString4,//'owner',
    RsAttributeHumanString5,//'deny',
    RsAttributeHumanString6,//'logon ID',
    RsAttributeHumanString7,//'domain-local',
    RsAttributeHumanString8,//'integrity',
    RsAttributeHumanString9,//'integrity enabled',
    RsAttributeHumanString10,//'pad0',
    RsAttributeHumanString11,//'pad1',
    RsAttributeHumanString12,//'pad2',
    RsAttributeHumanString13,//'pad3',
    RsAttributeHumanString14,//'pad4',
    RsAttributeHumanString15//'pad5'
    );    




const
  Powers2: array[0..35] of int64 =
    (0, 1, 2, 4, 8, 16, 32, 64,
    128, 256, 512, 1024, 2048, 4096, 8192, 16384,
    32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304,
    8388608, 16777216, 33554432, 67108864, 134217728,
    268435456, 536870912, 1073741824,
    2147483648, 4294967296, 8589934592, 17179869184);




const
  //Maximum size of a resource string
  MaxResourceStringLength = 300;

  {This value is used in LocalizeMapping for parameter UseDefaultOnError.
  Set to true to ignore invalid jwscl resource strings and use default ones.
  Otherwise an exception is raised.
  }
  IgnoreEJwsclResourceNotFound = true;

  {These values defines the first resource string for the given
  mapping resource strings. 
  }

  //
  FileMappingStartIndex = 50000;
  FolderMappingStartIndex = 50030;
  FileFolderMappingStartIndex = 50060;
  WinStationMappingStartIndex = 50090;
  DesktopMappingStartIndex = 50120;
  RegistryMappingStartIndex = 50150;
  ServiceMappingStartIndex = 50180;
  SCManagerMappingStartIndex = 50210;
  PrinterMappingStartIndex = 50240;
  ShareMappingStartIndex = 50270;
  ProcessMappingStartIndex = 50290;
  ThreadMappingStartIndex = 50320;
  JobMappingStartIndex = 50350;
  SemaphoreMappingStartIndex = 50380;
  EventMappingStartIndex = 50400;
  MutexMappingStartIndex = 50420;
  FileMapMappingStartIndex = 50440;
  TimerMappingStartIndex = 50470;
  TokenMappingStartIndex = 50490;
  PipeMappingStartIndex = 50520;


const
  //LogonUser undefined constant values (may be moved to JWA)
  ERROR_ILL_FORMED_PASSWORD = 1327;

const
  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;

  NET_FW_IP_VERSION_ANY = 2;
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_IP_PROTOCOL_UDP = 17;

  FW_MGR_CLASS_NAME = 'HNetCfg.FwMgr';
  FW_AUTHORIZEDAPPLICATION_CLASS_NAME = 'HNetCfg.FwAuthorizedApplication';
  FW_OPENPORT_CLASS = 'HNetCfg.FWOpenPort';

{<B>JwInitLocalizedMappings</B> translate all the rights mapping arrays using the resource.

 user language or neutral if not found
 JwInitLocalizedMappings(PRIMARYLANGID(GetUserDefaultUILanguage),
   SUBLANGID(GetUserDefaultUILanguage));
   JwInitLocalizedMappings(LANG_SYSTEM_DEFAULT, SUBLANG_SYS_DEFAULT);

 english version
  JwInitLocalizedMappings(LANG_ENGLISH, SUBLANG_ENGLISH_UK);

 german version
  JwInitLocalizedMappings(LANG_GERMAN,SUBLANG_GERMAN);

 neutral version (using constant names)
  JwInitLocalizedMappings(LANG_NEUTRAL, SUBLANG_SYS_DEFAULT);
  JwInitLocalizedMappings(0,0);

@param PrimaryLanguage defines the primary language id 
@param SubLanguage defines the primary language id 
@param Inst defines the resource source 

}
procedure JwInitLocalizedMappings(PrimaryLanguage,
  SubLanguage : Word; Inst : HINST = 0);


const
  {
  <B>ERROR_CANCELLED</B>
  The user has canceled the UAC prompt.
  }
  E_USER_CANCELED_OPERATION = HRESULT($800704C7);//

  {
  <B>ERROR_CANCELLED</B>
  The user has canceled the UAC prompt.
  Integer version. Contains only code part of HRESULT.
  }
  E_USER_CANCELED_OPERATIONint = 1223;//

  {
  <B>E_CLASS_IS_NOT_SETUP</B>
  The requested COM class has not been setup to be used for elevation.
  }
  E_CLASS_IS_NOT_SETUP = HRESULT($80080017);

  {
  <B>E_CLASS_IS_NOT_SETUPint</B>
  The requested COM class has not been setup to be used for elevation.
  Integer version. Contains only code part of HRESULT.
  }
  E_CLASS_IS_NOT_SETUPint = 23;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses SysUtils, JwsclExceptions, JwsclUtils, JwsclMapping;

procedure JwInitLocalizedMappings(PrimaryLanguage,
  SubLanguage : Word; Inst : HINST = 0);


{var Count : Cardinal;
    PsiArray : PSI_ACCESS;
    PsiArray2 : TJwSiAccessArray;}
begin
  LocalizeMapping(FileMapping, //var MappingRecord : array of TJwRightsMapping;
    FileMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

 { DEBUG
  PsiArray := TJwSecurityFileMapping.GetAccessNames(Count);
  PsiArray2 := TJwSiAccessArray(PsiArray);
  if PsiArray2 = nil then;
  TJwSecurityFileMapping.FreeAccessNames(PsiArray,Count);   }



  LocalizeMapping(FolderMapping, //var MappingRecord : array of TJwRightsMapping;
    FolderMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(FileFolderMapping, //var MappingRecord : array of TJwRightsMapping;
    FileFolderMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(WinStationMapping, //var MappingRecord : array of TJwRightsMapping;
    WinStationMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(DesktopMapping, //var MappingRecord : array of TJwRightsMapping;
    DesktopMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(RegistryMapping, //var MappingRecord : array of TJwRightsMapping;
    RegistryMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );


  LocalizeMapping(ServiceMapping, //var MappingRecord : array of TJwRightsMapping;
    ServiceMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(SCManagerMapping, //var MappingRecord : array of TJwRightsMapping;
    SCManagerMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(PrinterMapping, //var MappingRecord : array of TJwRightsMapping;
    PrinterMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(ShareMapping, //var MappingRecord : array of TJwRightsMapping;
    ShareMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(ProcessMapping, //var MappingRecord : array of TJwRightsMapping;
    ProcessMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(ThreadMapping, //var MappingRecord : array of TJwRightsMapping;
    ThreadMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(JobMapping, //var MappingRecord : array of TJwRightsMapping;
    JobMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(SemaphoreMapping, //var MappingRecord : array of TJwRightsMapping;
    SemaphoreMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(EventMapping, //var MappingRecord : array of TJwRightsMapping;
    EventMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(MutexMapping, //var MappingRecord : array of TJwRightsMapping;
    MutexMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(FileMapMapping, //var MappingRecord : array of TJwRightsMapping;
    FileMapMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(TimerMapping, //var MappingRecord : array of TJwRightsMapping;
    TimerMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(TokenMapping, //var MappingRecord : array of TJwRightsMapping;
    TokenMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );

  LocalizeMapping(PipeMapping, //var MappingRecord : array of TJwRightsMapping;
    PipeMappingStartIndex,//const StartStringId : Cardinal;
    PrimaryLanguage, SubLanguage,//SubLanguageId : Word;
    IgnoreEJwsclResourceNotFound,//const UseDefaultOnError : Boolean = true;
    Inst//ModuleInstance : HINST = 0
  );






end;

initialization

// user language or neutral if not found
// JwInitLocalizedMappings(PRIMARYLANGID(GetUserDefaultUILanguage),
//   SUBLANGID(GetUserDefaultUILanguage));



{$IFDEF JWSCL_INCLUDE_RES}
   JwInitLocalizedMappings(LANG_SYSTEM_DEFAULT, SUBLANG_SYS_DEFAULT);
{$ENDIF JWSCL_INCLUDE_RES}

// english version
//  JwInitLocalizedMappings(LANG_ENGLISH, SUBLANG_ENGLISH_UK);

// german version
//  JwInitLocalizedMappings(LANG_GERMAN,SUBLANG_GERMAN);

// neutral version (using constant names)
//  JwInitLocalizedMappings(LANG_NEUTRAL, SUBLANG_SYS_DEFAULT);
//  JwInitLocalizedMappings(0,0);



{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}
{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
