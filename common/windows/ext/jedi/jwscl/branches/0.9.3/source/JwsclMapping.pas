{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains the Access Mask Mapping class

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

The Original Code is JwsclMapping.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclMapping;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses Classes,
  SysUtils,
  jwaWindows,
  JwsclResource,
  JwsclUtils,
  JwsclConstants,
  JwsclExceptions, JwsclTypes, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwOnGetAccessText = procedure(AccessMaskBit: Cardinal;
    var Text: WideString) of object;


     {<B>TJwSecurityGenericMapping</B> defines a general access mapping.
      To create your own access map, derive a class from <B>TJwSecurityGenericMapping</B>
      and override the class method GetMapping.
     }

  TJwSecurityGenericMapping = class(TPersistent)
  protected
       {<B>CheckAndStrip</B> checks for a given bit and removes it from the mask.
        @param Access Mask to be used.
        @param BitState}
    class function CheckAndStrip(var Access: TJwAccessMask;
      const BitState: Cardinal): boolean; virtual;

    class function GetOnGetAccessText(const AccessMaskBit: Cardinal;
      const DefaultText: WideString): WideString; virtual;
    class procedure CreateSI(var si: SI_ACCESS;
      const Access, Flags: Cardinal; Name: WideString); virtual;

    class function GetAccessNamesEx(out iCount: Cardinal;
      Maps: array of TJwRightsMapping): PSI_ACCESS; virtual;
    class function MapAccessMaskToStringEx(Access: TJwAccessMask;
      Maps: array of TJwRightsMapping): TJwString; virtual;

  public
    {<B>GetBitMappingString</B> returns an description of a specific bit value given in Idx.
     This function only supports one bit per call.
     @param Idx Defines a bit position between 0 and 31 
     @return Returns the name and description of the given bit or an empty string
       if the bit is invalid. 
     }
    class function GetBitMappingString(Idx: Cardinal): TJwString; virtual;

       {<B>MapAccessMaskToString</B> creates a string represenative for the given access mask.
        The look of the string depends on the result of GetMapping}
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      virtual;

       {<B>Map</B> maps one of the generic access mask (GENERIC_READ, GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL)
        to the specific access mask.
        The specific access mask depends on GetMapping. GetMapping can be overriden in derived classes
        to change the behaviour.
        In TJwSecurityGenericMapping the result is always 0.
        @param genericAccessMask receives one of the generic access constants (GENERIC_READ, GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL).
        @return Returns the mapped access rights.
        raises
 EJwsclInvalidGenericAccessMask:  is raised, if genericAccessMask does not contain one of the GENERIC access masks.
       }
    class function Map(genericAccessMask: TJwAccessMask): TJwAccessMask;

    {<B>GenericMap</B> maps an access mask using the winapi MapGenericMask.}
    class function GenericMap(
      const AccessMask: TJwAccessMask): TJwAccessMask;

       {<B>GetMapping</B> creates a generic mapping.
       @return In TJwSecurityGenericMapping this method returns the standard rights.}
    class function GetMapping: TGenericMapping; virtual;

    {<B>GetAccessNames</B> returns a pointer to a SI_ACCESS structure.
     The caller must free the returned value by using FreeAccessNames.
    }
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      virtual;
    class procedure FreeAccessNames(var pPSI_ACCESS: PSI_ACCESS;
      iCount: Cardinal);


  end;

  TJwSecurityUserMapping = class(TJwSecurityGenericMapping)
  public
    class function GenericMap(Mapping: TGenericMapping;
      const AccessMask: TJwAccessMask): TJwAccessMask; reintroduce; virtual;

    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping(Mapping: TGenericMapping): TGenericMapping;
      reintroduce; virtual;
    class function MapAccessMaskToString(Access: TJwAccessMask;
      Maps: array of TJwRightsMapping): TJwString; reintroduce; virtual;
    class function GetAccessNames(Maps: array of TJwRightsMapping;
      out iCount: Cardinal): PSI_ACCESS; reintroduce; virtual;
  end;

  {<B>TJwSecurityFileMapping</B> defines a generic mapping for file access rights}
  TJwSecurityFileMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityRegistryMapping</B> defines a generic mapping for file access rights}
  TJwSecurityRegistryMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for registry objects.}
    class function GetMapping: jwaWindows.TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  { TJwSecurityWinStationMapping provides methods that map generic access rights to
    specific access rights for a WindowStation, converts single access rights to a
    string or create a SI_ACCESS structure for usage in a security descriptor editor
    (TJwSecurityDescriptorDialog in JwsclSecurityDialogs.pas)                        }
  TJwSecurityWinStationMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for registry objects.}
    class function GetMapping: jwaWindows.TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  TJwSecurityDesktopMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for registry objects.}
    class function GetMapping: jwaWindows.TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {-------}
  {<B>TJwSecurityFileFolderMapping</B> defines a generic mapping for file access rights}
  TJwSecurityFileFolderMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;


  {<B>TJwSecurityServiceMapping</B> defines a generic mapping for file access rights}
  TJwSecurityServiceMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityServiceManagerMapping</B> defines a generic mapping for file access rights}
  TJwSecurityServiceManagerMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityPrinterMapping</B> defines a generic mapping for file access rights}
  TJwSecurityPrinterMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityShareMapping</B> defines a generic mapping for file access rights}
  TJwSecurityShareMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityProcessMapping</B> defines a generic mapping for file access rights}
  TJwSecurityProcessMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;


  {<B>TJwSecurityThreadMapping</B> defines a generic mapping for file access rights}
  TJwSecurityThreadMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityJobMapping</B> defines a generic mapping for file access rights}
  TJwSecurityJobMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;


  {<B>TJwSecuritySemaphoreMapping</B> defines a generic mapping for file access rights}
  TJwSecuritySemaphoreMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;


  {<B>TJwSecurityEventMapping</B> defines a generic mapping for file access rights}
  TJwSecurityEventMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;


  {<B>TJwSecurityMutexMapping</B> defines a generic mapping for file access rights}
  TJwSecurityMutexMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;



  {<B>TJwSecurityFileMapMapping</B> defines a generic mapping for file access rights}
  TJwSecurityFileMapMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityTimerMapping</B> defines a generic mapping for file access rights}
  TJwSecurityTimerMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityTokenMapping</B> defines a generic mapping for file access rights}
  TJwSecurityTokenMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;

  {<B>TJwSecurityPipeMapping</B> defines a generic mapping for file access rights}
  TJwSecurityPipeMapping = class(TJwSecurityGenericMapping)
  public
    {<B>GetMapping</B> returns the generic mapping for file objects.}
    class function GetMapping: TGenericMapping; override;
    class function MapAccessMaskToString(Access: TJwAccessMask): TJwString;
      override;
    class function GetAccessNames(out iCount: Cardinal): PSI_ACCESS;
      override;
  end;




  TJwSecurityGenericMappingClass = class of TJwSecurityGenericMapping;



var
  JwOnGetAccessText: TJwOnGetAccessText = nil;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math;

{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

const
  Powers2: array[0..35] of int64 =
    (0, 1, 2, 4, 8, 16, 32, 64,
    128, 256, 512, 1024, 2048, 4096, 8192, 16384,
    32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304,
    8388608, 16777216, 33554432, 67108864, 134217728,
    268435456, 536870912, 1073741824,
    2147483648, 4294967296, 8589934592, 17179869184);



function IsSet(const Bits: TJwAccessMask; i: Cardinal): boolean;
begin
  Result := (Bits and Powers2[i]) = Powers2[i];
end;




{ TSecurityMapping }

class function TJwSecurityGenericMapping.CheckAndStrip(
  var Access: TJwAccessMask; const BitState: Cardinal): boolean;
begin
  Result := (Access and BitState) = BitState;
  if Result then
    Access := Access and not BitState;
end;

class procedure TJwSecurityGenericMapping.CreateSI(var si: SI_ACCESS;
  const Access, Flags: Cardinal; Name: WideString);
begin
  FillChar(si, sizeof(si), 0);

  Name := GetOnGetAccessText(Access, Name);

  GetMem(si.pszName, Length(Name) * 2 + 2);
  FillChar(si.pszName^, Length(Name) * 2 + 2, 0);
  CopyMemory(si.pszName, @Name[1], Length(Name) * 2);

  si.mask := Access;
  si.dwFlags := Flags;
end;

class function TJwSecurityGenericMapping.GenericMap(
  const AccessMask: TJwAccessMask): TJwAccessMask;
var
  p: jwaWindows.TGenericMapping;
begin
  Result := AccessMask;
  p := GetMapping();
  MapGenericMask(Result, p);
end;

class procedure TJwSecurityGenericMapping.FreeAccessNames(
  var pPSI_ACCESS: PSI_ACCESS; iCount: Cardinal);
var
  i: integer;
  siArr: PSI_ACCESS;
begin
  if pPSI_ACCESS = nil then
    exit;

  siArr := pPSI_ACCESS;

  for i := 0 to iCount - 1 do
  begin
    if (siArr.pszName <> nil) then
      FreeMem(siArr.pszName);
    Inc(siArr);
  end;

  FreeMem(pPSI_ACCESS);
  pPSI_ACCESS := nil;
end;

class function TJwSecurityGenericMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
var
  siPos: PSI_ACCESS;
  i: Cardinal;
  w: WideString;
begin
  iCount := sizeof(Cardinal) * 8;//32;
  GetMem(Result, SizeOf(SI_ACCESS) * iCount);
  FillChar(Result^, SizeOf(SI_ACCESS) * iCount, 0);

  siPos := Result;
  for i := 0 to iCount - 1 do
  begin
    w := WideString(GetBitMappingString(i));
    CreateSI(siPos^, Powers2[i], SI_ACCESS_SPECIFIC or SI_ACCESS_GENERAL, w);
    Inc(siPos); //c Style
  end;
end;

class function TJwSecurityGenericMapping.MapAccessMaskToStringEx(
  Access: TJwAccessMask; Maps: array of TJwRightsMapping): TJwString;
var
  i: Cardinal;

begin
  Result := '';

  for i := low(Maps) to high(Maps) do
  begin
    if CheckAndStrip(Access, Maps[i].Right) then
      Result := Result + Maps[i].Name + ' '+RsStringOr+' ';
  end;

  if Access > 0 then
    Result := Result + ' ' + IntToStr(Access) + ' [$' + IntToHex(Access, 2) + ']'
  else
    System.Delete(Result, Length(Result) - 3, 4);
end;

class function TJwSecurityGenericMapping.GetAccessNamesEx(
  out iCount: Cardinal; Maps: array of TJwRightsMapping): PSI_ACCESS;
var
  siArr: PSI_ACCESS;

  i,
  //iPos,
  iStart: Cardinal;
  w: WideString;
//  tempResult : PSI_ACCESS;
begin
  iCount := Length(Maps);
  for i := low(Maps) to high(Maps) do
  begin
    if (Maps[i].Flags = 0) then
      Dec(iCount);
  end;


  if (Maps[0].Flags = 0) then
    Inc(iCount, 4);

  GetMem(Result, Sizeof(SI_ACCESS) * iCount);
  {result = siArr}
  siArr := Result;

  //iPos := 0;
  iStart := 0;
  //automatically add generic rights to array if wanted
  if (Maps[0].Flags = 0) then
  begin
    CreateSI(siArr^, GetMapping.GenericAll, SI_ACCESS_GENERAL or
      SI_ACCESS_SPECIFIC, RsMappingGeneralFullControll);
    Inc(siArr);
    CreateSI(siArr^, GetMapping.GenericRead, SI_ACCESS_GENERAL, RsMappingGeneralRead);
    Inc(siArr);
    CreateSI(siArr^, GetMapping.GenericWrite, SI_ACCESS_GENERAL, RsMappingGeneralWrite);
    Inc(siArr);
    CreateSI(siArr^, GetMapping.GenericExecute,
      SI_ACCESS_GENERAL, RsMappingGeneralExecute);
    Inc(siArr);

    //iPos := 4;
    //iStart := iPos;
  end;         


  for i := iStart to high(Maps) do
  begin
    w := Maps[i].Name;
    if (Maps[i].Flags <> 0) then
    begin
      CreateSI(siArr^, Maps[i].Right, Maps[i].Flags, w);
      //Inc(iPos);
      Inc(siArr);
    end;
  end;

end;

class function TJwSecurityGenericMapping.GetBitMappingString(
  Idx: Cardinal): TJwString;
begin
  Result := '';
  if (Idx <= 15) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeSpecific);
  end
  else
  if (Idx >= 16) and (Idx <= 23) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeStandard);
  end
  else
  if (Idx = 24) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeSacl);
  end
  else
  if (Idx = 25) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeMaximumAllowed);
  end
  else
  if (Idx = 26) or (Idx = 27) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeReserved);
  end
  else
  if (Idx >= 28) and (Idx <= 31) then
  begin
    Result := Result + RsMappingBitString + IntToStr(Idx) + ' ' +
      GetOnGetAccessText(Powers2[Idx], RsMappingTypeGeneric);
  end;
end;

class function TJwSecurityGenericMapping.GetMapping: jwaWindows.TGenericMapping;
begin
  Result.GenericRead :=
    READ_CONTROL or STANDARD_RIGHTS_READ;
  Result.GenericWrite := WRITE_DAC or WRITE_OWNER or Delete or
    ACCESS_SYSTEM_SECURITY or STANDARD_RIGHTS_WRITE;
  Result.GenericExecute := STANDARD_RIGHTS_EXECUTE or SYNCHRONIZE;
  Result.GenericAll := Result.GenericRead or Result.GenericWrite or
    Result.GenericExecute;
end;

class function TJwSecurityGenericMapping.GetOnGetAccessText(
  const AccessMaskBit: Cardinal; const DefaultText: WideString): WideString;
begin
  Result := DefaultText;
  if Assigned(JwOnGetAccessText) then
    JwOnGetAccessText(AccessMaskBit, Result);
end;

class function TJwSecurityGenericMapping.Map(
  genericAccessMask: TJwAccessMask): TJwAccessMask;
begin
  case genericAccessMask of
    GENERIC_READ: Result := GetMapping.GenericRead;
    GENERIC_WRITE: Result := GetMapping.GenericWrite;
    GENERIC_EXECUTE: Result := GetMapping.GenericExecute;
    GENERIC_ALL: Result := GetMapping.GenericAll;
    else
      raise EJwsclInvalidGenericAccessMask.CreateFmtEx(
        RsMappingNotAGenericMask,
        'Map', ClassName, 'JwsclMapping.pas', 0, False, []);
  end;
end;

class function TJwSecurityGenericMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
var
  i: integer;
  bSet: boolean;
begin
  Result := '';

  //usually from 31 downto 0 (32bit)
  for i := (sizeof(Access) * 8)-1 downto 0 do
  begin
    bSet := IsSet(Access, i);
    if bSet then
      Result := Result + GetBitMappingString(i) + ' '+RsStringOr+' ';
  end;

  //remove last RsStringOr-String and its 2 space
  System.Delete(Result, Length(Result) - Length(RsStringOr)-1, Length(RsStringOr)+2);
end;



{ TJwSecurityUserMapping }


class function TJwSecurityUserMapping.GetAccessNames(
  Maps: array of TJwRightsMapping; out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, Maps);
end;


class function TJwSecurityUserMapping.GenericMap(Mapping: TGenericMapping;
  const AccessMask: TJwAccessMask): TJwAccessMask;
var
  p: jwaWindows.TGenericMapping;
begin
  Result := AccessMask;
  p := GetMapping(Mapping);
  MapGenericMask(Result, p);
end;



class function TJwSecurityUserMapping.GetMapping(
  Mapping: TGenericMapping): TGenericMapping;
begin
  Result := Mapping;
  if (Result.GenericAll = Cardinal(-1)) then
    Result.GenericAll := Result.GenericRead or Result.GenericWrite or
      Result.GenericExecute;
end;

class function TJwSecurityUserMapping.MapAccessMaskToString(
  Access: TJwAccessMask; Maps: array of TJwRightsMapping): TJwString;
begin
  Result := MapAccessMaskToStringEx(Access, Maps);
end;


{ TJwSecurityDesktopMapping }

class function TJwSecurityDesktopMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, DesktopMapping);
end;

class function TJwSecurityDesktopMapping.GetMapping: jwaWindows.TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(DesktopGenericMapping);
end;

class function TJwSecurityDesktopMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, DesktopMapping);
end;

{ TJwSecurityFileMapping }

class function TJwSecurityFileMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, FileMapping);
end;

class function TJwSecurityFileMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(FileGenericMapping);
end;

class function TJwSecurityFileMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, FileMapping);
end;

{ TJwSecurityRegistryMapping }

class function TJwSecurityRegistryMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, RegistryMapping);
end;

class function TJwSecurityRegistryMapping.GetMapping:
jwaWindows.TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(RegistryGenericMapping);
end;

class function TJwSecurityRegistryMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(
    Access, RegistryMapping);
end;

{ TJwSecurityWinStationMapping }

class function TJwSecurityWinStationMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, WinStationMapping);
end;

class function TJwSecurityWinStationMapping.GetMapping:
jwaWindows.TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(WinStationGenericMapping);
end;

class function TJwSecurityWinStationMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(
    Access, WinStationMapping);
end;


{ TJwSecurityFileFolderMapping }

class function TJwSecurityFileFolderMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, FileFolderMapping);
//  The Ex version contains constant names instead of human readable name constants
//  Result := GetAccessNamesEx(iCount, FileFolderMappingEx);
end;

class function TJwSecurityFileFolderMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(FileFolderGenericMapping);
end;

class function TJwSecurityFileFolderMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(
    Access, FileFolderMapping);
end;

{ TJwSecurityServiceMapping }

class function TJwSecurityServiceMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, ServiceMapping);
end;

class function TJwSecurityServiceMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(ServiceGenericMapping);
end;

class function TJwSecurityServiceMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, ServiceMapping);
end;


{ TJwSecurityServiceManagerMapping }

class function TJwSecurityServiceManagerMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, SCManagerMapping);
end;

class function TJwSecurityServiceManagerMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(SCManagerGenericMapping);
end;

class function TJwSecurityServiceManagerMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(
    Access, SCManagerMapping);
end;

{ TJwSecurityPrinterMapping }

class function TJwSecurityPrinterMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, PrinterMapping);
end;

class function TJwSecurityPrinterMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(PrinterGenericMapping);
end;

class function TJwSecurityPrinterMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, PrinterMapping);
end;

{ TJwSecurityShareMapping }

class function TJwSecurityShareMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, ShareMapping);
end;

class function TJwSecurityShareMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(ShareGenericMapping);
end;

class function TJwSecurityShareMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, ShareMapping);
end;

{ TJwSecurityProcessMapping }

class function TJwSecurityProcessMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, ProcessMapping);
end;

class function TJwSecurityProcessMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(ProcessGenericMapping);
end;

class function TJwSecurityProcessMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, ProcessMapping);
end;

{ TJwSecurityThreadMapping }

class function TJwSecurityThreadMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, ThreadMapping);
end;

class function TJwSecurityThreadMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(ThreadGenericMapping);
end;

class function TJwSecurityThreadMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, ThreadMapping);
end;

{ TJwSecurityJobMapping }

class function TJwSecurityJobMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, JobMapping);
end;

class function TJwSecurityJobMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(JobGenericMapping);
end;

class function TJwSecurityJobMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, JobMapping);
end;

{ TJwSecuritySemaphoreMapping }

class function TJwSecuritySemaphoreMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, ServiceMapping);
end;

class function TJwSecuritySemaphoreMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(SemaphoreGenericMapping);
end;

class function TJwSecuritySemaphoreMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, ServiceMapping);
end;

{ TJwSecurityEventMapping }

class function TJwSecurityEventMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, EventMapping);
end;

class function TJwSecurityEventMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(EventGenericMapping);
end;

class function TJwSecurityEventMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, EventMapping);
end;

{ TJwSecurityFileMapMapping }

class function TJwSecurityFileMapMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, FileMapping);
end;

class function TJwSecurityFileMapMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(FileMapGenericMapping);
end;

class function TJwSecurityFileMapMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, FileMapping);
end;

{ TJwSecurityMutexMapping }

class function TJwSecurityMutexMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, MutexMapping);
end;

class function TJwSecurityMutexMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(MutexGenericMapping);
end;

class function TJwSecurityMutexMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, MutexMapping);
end;

{ TJwSecurityTimerMapping }

class function TJwSecurityTimerMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, TimerMapping);
end;

class function TJwSecurityTimerMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(TimerGenericMapping);
end;

class function TJwSecurityTimerMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, TimerMapping);
end;

{ TSecurityTokenapping }

class function TJwSecurityTokenMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, TokenMapping);
end;

class function TJwSecurityTokenMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(TokenGenericMapping);
end;

class function TJwSecurityTokenMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, TokenMapping);
end;

{ TJwSecurityPipeMapping }

class function TJwSecurityPipeMapping.GetAccessNames(
  out iCount: Cardinal): PSI_ACCESS;
begin
  Result := GetAccessNamesEx(iCount, PipeMapping);
end;

class function TJwSecurityPipeMapping.GetMapping: TGenericMapping;
begin
  Result := TJwSecurityUserMapping.GetMapping(PipeGenericMapping);
end;

class function TJwSecurityPipeMapping.MapAccessMaskToString(
  Access: TJwAccessMask): TJwString;
begin
  Result := TJwSecurityUserMapping.MapAccessMaskToString(Access, PipeMapping);
end;



{$ENDIF SL_INTERFACE_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
