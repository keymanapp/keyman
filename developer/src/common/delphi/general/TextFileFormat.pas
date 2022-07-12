(*
  Name:             TextFileFormat
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    13 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    13 Dec 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    13 Dec 2012 - mcdurdin - I3637 - V9.0 - I3502 Fail - Reload as Format button is disabled in some contexts
*)
unit TextFileFormat;

interface

uses
  SysUtils;

type
  TTextFileFormat = (tffANSI, tffUTF8, tffUTF16);
  TEditorFormat = (efKMN, efXML, efText, efHTML, efJSON, efJS, efCSS, efWordlistTsv);

function TextFileFormatToEncoding(AFormat: TTextFileFormat): TEncoding;   // I3502   // I3637

implementation

function TextFileFormatToEncoding(AFormat: TTextFileFormat): TEncoding;   // I3502   // I3637
begin
    case AFormat of
      tffANSI: Result := TEncoding.Default;
      tffUTF8: Result := TEncoding.UTF8;
      tffUTF16: Result := TEncoding.Unicode;
      else Result := TEncoding.UTF8;
    end;
end;

end.
