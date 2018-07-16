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

unit uCEFAuthCallback;

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
  TCefAuthCallbackRef = class(TCefBaseRefCountedRef, ICefAuthCallback)
  protected
    procedure Cont(const username, password: ustring);
    procedure Cancel;

  public
    class function UnWrap(data: Pointer): ICefAuthCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefAuthCallbackRef.Cancel;
begin
  PCefAuthCallback(FData)^.cancel(PCefAuthCallback(FData));
end;

procedure TCefAuthCallbackRef.Cont(const username, password: ustring);
var
  TempUsername, TempPassword : TCefString;
begin
  TempUsername := CefString(username);
  TempPassword := CefString(password);
  PCefAuthCallback(FData)^.cont(PCefAuthCallback(FData), @TempUsername, @TempPassword);
end;

class function TCefAuthCallbackRef.UnWrap(data: Pointer): ICefAuthCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefAuthCallback
   else
    Result := nil;
end;

end.
