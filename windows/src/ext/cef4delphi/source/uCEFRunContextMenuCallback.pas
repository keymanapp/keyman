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

unit uCEFRunContextMenuCallback;

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
  TCefRunContextMenuCallbackRef = class(TCefBaseRefCountedRef, ICefRunContextMenuCallback)
  protected
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefRunContextMenuCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefRunContextMenuCallbackRef.Cancel;
begin
  PCefRunContextMenuCallback(FData)^.cancel(PCefRunContextMenuCallback(FData));
end;

procedure TCefRunContextMenuCallbackRef.Cont(commandId: Integer; eventFlags: TCefEventFlags);
begin
  PCefRunContextMenuCallback(FData)^.cont(PCefRunContextMenuCallback(FData), commandId, eventFlags);
end;

class function TCefRunContextMenuCallbackRef.UnWrap(data: Pointer): ICefRunContextMenuCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRunContextMenuCallback
   else
    Result := nil;
end;

end.
