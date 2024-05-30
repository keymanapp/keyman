(*
  Name:             VisualKeyboardExportXML
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 Dec 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Dec 2006 - mcdurdin - Export to version 7, UTF-8, fix text encoding for key caps
                    22 Jan 2007 - mcdurdin - Export XML files to filename_xml_files subfolder
                    19 Mar 2007 - mcdurdin - I699 - Fix crash when exporting OSK to HTML/XML
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    16 Jan 2009 - mcdurdin - WideString fields
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit VisualKeyboardExportXML;  // I3306

interface

uses VisualKeyboard;

type
  TVisualKeyboardExportXML = class(TVisualKeyboardExport)
  private
    FSubDir: WideString;
    function XMLWideString(FileName: WideString): WideString;
  public
    procedure ExportToFile(FileName: WideString); override;
  end;

implementation

uses
  System.UITypes,
  Controls,
  Classes,
  Dialogs,
  KeymanVersion,
  SysUtils,
  Unicode,
  utildir,
  utilxml,
  VKeys;

{ TVisualKeyboardExportXML }

function TVisualKeyboardExportXML.XMLWideString(FileName: WideString): WideString;
var
  s: WideString;
  t: WideString;
  FUnicode, i: Integer;
  FShiftIndex: Integer;
  stream: TFileStream;
const
  nl: WideString = #13#10;
begin
  s := '<?xml version="1.0" encoding="UTF-8" ?>' + nl;
  s := s + '<visualkeyboard>' + nl;
  s := s + '  <header>' + nl;
  s := s + '    <version>'+SKeymanVersion70+'</version>' + nl;
  s := s + WideFormat('    <kbdname>%s</kbdname>', [FKbd.Header.AssociatedKeyboard]) + nl;

  t := '';
  if kvkh102 in FKbd.Header.Flags then t := t + '<key102/>';
  if kvkhDisplayUnderlying in FKbd.Header.Flags then t := t + '<displayunderlying/>';
  if kvkhUseUnderlying in FKbd.Header.Flags then t := t + '<useunderlying/>';
  if kvkhAltGr in FKbd.Header.Flags then t := t + '<usealtgr/>';

  s := s + WideFormat('    <flags>%s</flags>', [t]) + nl;
  s := s + WideFormat('    <ansifont><font name="%s" size="%d"/></ansifont>',
    [FKbd.Header.ANSIFont.Name, FKbd.Header.ANSIFont.Size]) + nl;
  s := s + WideFormat('    <unicodefont><font name="%s" size="%d"/></unicodefont>',
    [FKbd.Header.UnicodeFont.Name, FKbd.Header.UnicodeFont.Size]) + nl;
  s := s + WideFormat('    <layout>%s</layout>', [FKbd.Header.UnderlyingLayout]) + nl;
  s := s + WideFormat('    <filename>%s</filename>', [ExtractFileName(ChangeFileExt(FileName, ''))]);
  s := s + '  </header>' + nl;
  s := s + '  <keys>' + nl;

//  GetVKLegalShiftStateIndex(0);   ///

  for i := 0 to FKbd.Keys.Count - 1 do
  begin
    if kvkkUnicode in FKbd.Keys[i].Flags then FUnicode := 1 else FUnicode := 0;
    FShiftIndex := GetVKLegalShiftStateIndex(FKbd.Keys[i].Shift);
    if FShiftIndex >= 0 then
    begin
      s := s + WideFormat('    <key vkey="%s" shift="%s" unicode="%d" ', [VKeyNames[FKbd.Keys[i].VKey],
        VKLegalShiftStates[FShiftIndex].Name, FUnicode]);
      if kvkkBitmap in FKbd.Keys[i].Flags then
        if Assigned(FKbd.Keys[i].Bitmap) then
        begin
          stream := TFileStream.Create(WideFormat('%s\glyph%d.bmp', [FSubdir, i]), fmCreate);
          try
            FKbd.Keys[i].Bitmap.SaveToStream(stream);
          finally
            stream.Free;
          end;
          s := s + WideFormat('bitmap="%s/glyph%d.bmp"/>', [ExtractFileName(FSubdir), i]) + nl;
        end
        else
          s := s + '/>' + nl
      else
        s := s + 'text="'+XMLEncode(FKbd.Keys[i].Text)+'"/>' + nl;
    end;
  end;

  s := s + '  </keys>' + nl;
  s := s + '</visualkeyboard>' + nl;

  Result := s;
end;

procedure TVisualKeyboardExportXML.ExportToFile(FileName: WideString);
var
  ss: TStringStream;
begin
  FSubdir := ChangeFileExt(FileName, '')+'_xml_files';

  if DirectoryExists(FSubdir) and not DirectoryEmpty(FSubdir) then
  begin
    if MessageDlg('The subdirectory "'+FSubdir+'" already exists.  Images for the HTML file will be placed in this '+
      'subdirectory.  If you continue, any files currently in the directory will be deleted.'#13#10#13#10+
      'Continue exporting and delete all existing files in the subdirectory?', mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
    if not EmptyDirectory(FSubdir) then
      if MessageDlg('The subdirectory "'+FSubdir+'" was not able to be emptied.  Continue exporting anyway?',
        mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
  end;

  CreateDir(FSubdir);

  ss := TStringStream.Create(XMLWideString(FileName), TEncoding.UTF8);
  with TFileStream.Create(FileName, fmCreate) do
  try
    CopyFrom(ss, 0);
  finally
    Free;
    ss.Free;
  end;
end;

end.
