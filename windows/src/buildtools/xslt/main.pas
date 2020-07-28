(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    25 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    05 Nov 2007 - mcdurdin - Use poResolveExternals to avoid problems with external files
                    05 Nov 2007 - mcdurdin - CoInitialize...
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    17 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    25 May 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
*)
unit main;

interface

uses
  System.Classes,
  Xml.xmlintf,
  Xml.xmldoc,
  Winapi.activex;

procedure Run;

implementation

uses
  Xml.Win.msxmldom,
  System.SysUtils,

  Unicode;

procedure Parse;
var
  doc, xsl: IXMLDocument;
  xml: WideString;
begin
  doc := TXMLDocument.Create(nil);
  doc.ParseOptions := [poResolveExternals, poPreserveWhiteSpace];  // I902 - resolve externals when loading XML files
  doc.LoadFromFile(ParamStr(1));

  xsl := TXMLDocument.Create(nil);
  xsl.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files
  xsl.LoadFromFile(ParamStr(2));

  doc.Node.TransformNode(xsl.Node, xml);
  xsl := nil;
  doc := nil;
  xml := FormatXMLData(xml);
  if ParamCount < 3 then
  begin
    writeln(UTF8Encode(xml));   // I3337  // Yes.  We want to write it to the console as UTF-8
  end
  else
    with TStringList.Create do  // I3306
    try
      Text := xml;
      SaveToFile(ParamStr(3), TEncoding.UTF8);    // I3337 // Yes, we are using UTF-8 // I3306
    finally
      Free;
    end;
end;

procedure Run;
begin
  if ParamCount < 2 then
  begin
    writeln('xslt doc.xml stylesheet.xsl [output.xml]');
    writeln('Outputs the document transformed by stylesheet.xsl in UTF-8');
    writeln('If output.xml is not specified, then writes to stdout');
    ExitCode := 3;
    Exit;
  end;

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
    Parse;
    ExitCode := 0;
  except
    on E:Exception do
    begin
      writeln(E.ClassName+': '+E.Message);
      ExitCode := 1;
    end;
  end;

  CoUninitialize;
end;

end.
