{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStrToHtml;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvComponent;

type
  TJvStrToHtml = class(TJvComponent)
  private
    FHtml: string;
    FValue: string;
    procedure SetHtml(const Value: string);
    procedure SetValue(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text: string read FValue write SetValue;
    property Html: string read FHtml write SetHtml;
    function CharToHtml(Ch: Char): string;
    function TextToHtml(Text: string): string;
    function HtmlToText(Text: string): string;
  end;

function StringToHtml(Value: string): string;
function HtmlToString(Value: string): string;

implementation

type
  TJvHtmlCodeRec = record
    Ch: Char;
    Html: string;
  end;

const
  Conversions: array[1..79] of TJvHtmlCodeRec = (
    (Ch: '"'; Html: '&quot;'),
    (Ch: '�'; Html: '&agrave;'),
    (Ch: '�'; Html: '&ccedil;'),
    (Ch: '�'; Html: '&eacute;'),
    (Ch: '�'; Html: '&egrave;'),
    (Ch: '�'; Html: '&ecirc;'),
    (Ch: '�'; Html: '&ugrave;'),
    (Ch: '�'; Html: '&euml;'),
    (Ch: '<'; Html: '&lt;'),
    (Ch: '>'; Html: '&gt;'),
    (Ch: '^'; Html: '&#136;'),
    (Ch: '~'; Html: '&#152;'),
    (Ch: '�'; Html: '&#163;'),
    (Ch: '�'; Html: '&#167;'),
    (Ch: '�'; Html: '&#176;'),
    (Ch: '�'; Html: '&#178;'),
    (Ch: '�'; Html: '&#179;'),
    (Ch: '�'; Html: '&#181;'),
    (Ch: '�'; Html: '&#183;'),
    (Ch: '�'; Html: '&#188;'),
    (Ch: '�'; Html: '&#189;'),
    (Ch: '�'; Html: '&#191;'),
    (Ch: '�'; Html: '&#192;'),
    (Ch: '�'; Html: '&#193;'),
    (Ch: '�'; Html: '&#194;'),
    (Ch: '�'; Html: '&#195;'),
    (Ch: '�'; Html: '&#196;'),
    (Ch: '�'; Html: '&#197;'),
    (Ch: '�'; Html: '&#198;'),
    (Ch: '�'; Html: '&#199;'),
    (Ch: '�'; Html: '&#200;'),
    (Ch: '�'; Html: '&#201;'),
    (Ch: '�'; Html: '&#202;'),
    (Ch: '�'; Html: '&#203;'),
    (Ch: '�'; Html: '&#204;'),
    (Ch: '�'; Html: '&#205;'),
    (Ch: '�'; Html: '&#206;'),
    (Ch: '�'; Html: '&#207;'),
    (Ch: '�'; Html: '&#209;'),
    (Ch: '�'; Html: '&#210;'),
    (Ch: '�'; Html: '&#211;'),
    (Ch: '�'; Html: '&#212;'),
    (Ch: '�'; Html: '&#213;'),
    (Ch: '�'; Html: '&#214;'),
    (Ch: '�'; Html: '&#217;'),
    (Ch: '�'; Html: '&#218;'),
    (Ch: '�'; Html: '&#219;'),
    (Ch: '�'; Html: '&#220;'),
    (Ch: '�'; Html: '&#221;'),
    (Ch: '�'; Html: '&#223;'),
    (Ch: '�'; Html: '&#224;'),
    (Ch: '�'; Html: '&#225;'),
    (Ch: '�'; Html: '&#226;'),
    (Ch: '�'; Html: '&#227;'),
    (Ch: '�'; Html: '&#228;'),
    (Ch: '�'; Html: '&#229;'),
    (Ch: '�'; Html: '&#230;'),
    (Ch: '�'; Html: '&#231;'),
    (Ch: '�'; Html: '&#232;'),
    (Ch: '�'; Html: '&#233;'),
    (Ch: '�'; Html: '&#234;'),
    (Ch: '�'; Html: '&#235;'),
    (Ch: '�'; Html: '&#236;'),
    (Ch: '�'; Html: '&#237;'),
    (Ch: '�'; Html: '&#238;'),
    (Ch: '�'; Html: '&#239;'),
    (Ch: '�'; Html: '&#241;'),
    (Ch: '�'; Html: '&#242;'),
    (Ch: '�'; Html: '&#243;'),
    (Ch: '�'; Html: '&#244;'),
    (Ch: '�'; Html: '&#245;'),
    (Ch: '�'; Html: '&#246;'),
    (Ch: '�'; Html: '&#247;'),
    (Ch: '�'; Html: '&#249;'),
    (Ch: '�'; Html: '&#250;'),
    (Ch: '�'; Html: '&#251;'),
    (Ch: '�'; Html: '&#252;'),
    (Ch: '�'; Html: '&#253;'),
    (Ch: '�'; Html: '&#255;')
    );

  {**************************************************}

function TJvStrToHtml.CharToHtml(Ch: Char): string;
var
  I: Integer;
begin
  for I := Low(Conversions) to High(Conversions) do
    if Conversions[I].Ch = Ch then
    begin
      Result := Conversions[I].Html;
      Exit;
    end;
  Result := Ch;
end;

{**************************************************}

constructor TJvStrToHtml.Create(AOwner: TComponent);
begin
  inherited;
  FValue := '';
  FHtml := '';
end;

{**************************************************}

function TJvStrToHtml.HtmlToText(Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
    Result := Result + CharToHtml(Text[I]);
end;

{**************************************************}

procedure TJvStrToHtml.SetHtml(const Value: string);
begin
  FValue := HtmlToText(Value);
end;

{**************************************************}

procedure TJvStrToHtml.SetValue(const Value: string);
begin
  FHtml := TextToHtml(Value);
end;

{**************************************************}

function TJvStrToHtml.TextToHtml(Text: string): string;
var
  i: Integer;
begin
  Result := Text;
  for i := Low(Conversions) to High(Conversions) do
    Result := StringReplace(Result, Conversions[i].Html, Conversions[i].Ch,
      [rfReplaceAll, rfIgnoreCase]);
end;

{**************************************************}

// (rom) this is silly. Better base the component methods on the functions.

function StringToHtml(Value: string): string;
begin
  with TJvStrToHtml.Create(nil) do
  begin
    Result := TextToHtml(Value);
    Free;
  end;
end;

{**************************************************}

function HtmlToString(Value: string): string;
begin
  with TJvStrToHtml.Create(nil) do
  begin
    Result := HtmlToText(Value);
    Free;
  end;
end;

end.
