{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNetReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvNetReg;

{$I jvcl.inc}

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

interface
  {$IFDEF COMPILER5}

  TJvLocationCategory = class(TLayoutCategory)
  public
    class function Name: string; override;
  end;


  {$ENDIF COMPILER5}


procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  JvRichEditToHTML, JvMail, JvMailEditor,
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  JvUrlListGrabber, JvUrlGrabbers, JvUrlListGrabberEditors, JvProgramVersionCheck,
  {$ENDIF USEWINDOWS}
  JvHtmlParser, JvHtmlParserEditor,
  JvTypes, JvDsgnConsts,
  JvStringListToHtml, JvFormToHtml, JvRgbToHtml, JvStrToHtml;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvNetReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvNetReg.dcr}
{$ENDIF UNIX}

resourcestring
  RsLocation = 'Location';

//=== { TJvTFGridLayoutCategory } ============================================

{$IFDEF COMPILER5}
class function TJvLocationCategory.Name: string;
begin
  Result := RsLocation;
end;
{$ENDIF COMPILER5}


procedure Register;
begin
  RegisterComponents(RsPaletteInterNetWork, [TJvHTMLParser,
    {$IFDEF MSWINDOWS}
    TJvFTPURLGrabber, TJvHTTPURLGrabber,
    TJvLocalFileURLGrabber, TJvUrlListGrabber,
    TJvProgramVersionCheck, TJvProgramVersionNetworkLocation,
    TJvProgramVersionHTTPLocation, TJvProgramVersionFTPLocation,
    TJvProgramVersionDatabaseLocation,
    {$ENDIF MSWINDOWS}
    {$IFDEF VCL}
    TJvMail, TJvRichEditToHTML,
    {$ENDIF VCL}
    TJvStrToHTML, TJvStringListToHTML, TJvFormToHTML, TJvRGBToHTML]);
  {$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(TStrings),
    TJvHTMLParser, 'Parser', TJvHTMLParserEditor);
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberIndex),
    TJvUrlListGrabber, '', TJvUrlGrabberIndexProperty);
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberDefaultPropertiesList),
    TJvUrlListGrabber, '', TJvUrlGrabberDefaultPropertiesListEditor);
  RegisterPropertyEditor(TypeInfo(TJvCustomUrlGrabberDefaultProperties),
    TJvUrlGrabberDefPropEdTrick, '', TJvUrlGrabberDefaultPropertiesEditor);
  {$IFDEF VCL}
  RegisterComponentEditor(TJvMail, TJvMailEditor);
  {$ENDIF VCL}
  {$ENDIF MSWINDOWS}

   {$IFDEF COMPILER5}
  RegisterPropertiesInCategory(TJvLocationCategory, ['Location*',]);
  {$ENDIF COMPILER5}

end;

end.
