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

unit uCEFPDFPrintOptions;

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
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes;

type
  TPDFPrintOptions = class(TPersistent)
    protected
      Fpage_width            : integer;
      Fpage_height           : Integer;
      Fscale_factor          : integer;
      Fmargin_top            : double;
      Fmargin_right          : double;
      Fmargin_bottom         : double;
      Fmargin_left           : double;
      Fmargin_type           : TCefPdfPrintMarginType;
      Fheader_footer_enabled : integer;
      Fselection_only        : integer;
      Flandscape             : integer;
      Fbackgrounds_enabled   : integer;

    public
      constructor Create; virtual;

    published
      property page_width            : integer                 read Fpage_width               write Fpage_width            default 0;
      property page_height           : integer                 read Fpage_height              write Fpage_height           default 0;
      property scale_factor          : integer                 read Fscale_factor             write Fscale_factor          default 0;
      property margin_top            : double                  read Fmargin_top               write Fmargin_top;
      property margin_right          : double                  read Fmargin_right             write Fmargin_right;
      property margin_bottom         : double                  read Fmargin_bottom            write Fmargin_bottom;
      property margin_left           : double                  read Fmargin_left              write Fmargin_left;
      property margin_type           : TCefPdfPrintMarginType  read Fmargin_type              write Fmargin_type           default PDF_PRINT_MARGIN_DEFAULT;
      property header_footer_enabled : integer                 read Fheader_footer_enabled    write Fheader_footer_enabled default 0;
      property selection_only        : integer                 read Fselection_only           write Fselection_only        default 0;
      property landscape             : integer                 read Flandscape                write Flandscape             default 0;
      property backgrounds_enabled   : integer                 read Fbackgrounds_enabled      write Fbackgrounds_enabled   default 0;
  end;

implementation

constructor TPDFPrintOptions.Create;
begin
  Fpage_width            := 0;
  Fpage_height           := 0;
  Fscale_factor          := 100;
  Fmargin_top            := 0;
  Fmargin_right          := 0;
  Fmargin_bottom         := 0;
  Fmargin_left           := 0;
  Fmargin_type           := PDF_PRINT_MARGIN_DEFAULT;
  Fheader_footer_enabled := 0;
  Fselection_only        := 0;
  Flandscape             := 0;
  Fbackgrounds_enabled   := 0;
end;

end.
