// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2018 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFResourceBundle;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResourceBundleRef = class(TCefBaseRefCountedRef, ICefResourceBundle)
    protected
      function GetLocalizedString(stringId: Integer): ustring;
      function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
      function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;
    public
      class function UnWrap(data: Pointer): ICefResourceBundle;
      class function Global: ICefResourceBundle;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefResourceBundleRef.GetDataResource(    resourceId   : Integer;
                                               var data         : Pointer;
                                               var dataSize     : NativeUInt): Boolean;
begin
  Result := PCefResourceBundle(FData)^.get_data_resource(PCefResourceBundle(FData),
                                                         resourceId,
                                                         data,
                                                         dataSize) <> 0;
end;

function TCefResourceBundleRef.GetDataResourceForScale(    resourceId  : Integer;
                                                           scaleFactor : TCefScaleFactor;
                                                       var data        : Pointer;
                                                       var dataSize    : NativeUInt): Boolean;
begin
  Result := PCefResourceBundle(FData)^.get_data_resource_for_scale(PCefResourceBundle(FData),
                                                                   resourceId,
                                                                   scaleFactor,
                                                                   data,
                                                                   dataSize) <> 0;
end;

function TCefResourceBundleRef.GetLocalizedString(stringId: Integer): ustring;
begin
  Result := CefStringFreeAndGet(PCefResourceBundle(FData)^.get_localized_string(PCefResourceBundle(FData), stringId));
end;

class function TCefResourceBundleRef.Global: ICefResourceBundle;
begin
  Result := UnWrap(cef_resource_bundle_get_global());
end;

class function TCefResourceBundleRef.UnWrap(data: Pointer): ICefResourceBundle;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResourceBundle
   else
    Result := nil;
end;

end.
