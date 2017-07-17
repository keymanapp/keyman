(**
===============================================================================================
Name    : LibXmlComps
===============================================================================================
Project : All Projects processing XML documents
===============================================================================================
Subject : XML parser for Delphi's VCL toolbar
===============================================================================================
Dipl.-Ing. (FH) Stefan Heymann, Softwaresysteme, Tübingen, Germany
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
$Id: LibXmlComps.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $
2000-03-31  HeySt  1.0.0  Start
2000-07-27  HeySt  1.0.1  Added "TAttr" declaration
                          Moved GetNormalize/SetNormalize to PROTECTED section
2001-02-03  HeySt         Changed prototype for the TExternalEvent callback function type
                          so that C++Builder users should get it compiled better.

2001-02-28  HeySt  1.0.2  Introduced the "StopParser" property. When you set this property to
                          TRUE in one of the Parser Events, parsing is stopped and the Execute
                          method returns.
                          Introduced Version numbers
2001-07-10  HeySt  1.0.3  Fixed a bug in TScannerXmlParser.DtdElementFound so that the
                          OnAttList event is correctly fired
2001-07-11  HeySt  1.1.0  Derived from the new TCustomXmlScanner class from LibXmlParser
*)

unit LibXmlComps;

interface

uses
  Classes,
  LibXmlParser;

type
  TXmlScanner = class(TCustomXmlScanner)
  public
    property XmlParser;
    property StopParser;
  published
    property Filename;
    property Normalize;
    property OnXmlProlog;
    property OnComment;
    property OnPI;
    property OnDtdRead;
    property OnStartTag;
    property OnEmptyTag;
    property OnEndTag;
    property OnContent;
    property OnCData;
    property OnElement;
    property OnAttList;
    property OnEntity;
    property OnNotation;
    property OnDtdError;
    property OnLoadExternal;
    property OnTranslateEncoding;
  end;

  // The "Easy" XML Scanner leaves out events and properties which you are unlikely to use
  // for "normal" XML files.
  // CDATA sections trigger "OnContent" events
  TEasyXmlScanner = class(TCustomXmlScanner)
  protected
    procedure WhenCData(Content: string); override;
  public
    property XmlParser;
    property StopParser;
  published
    property Filename;
    property Normalize;
    property OnComment;
    property OnPI;
    property OnStartTag;
    property OnEmptyTag;
    property OnEndTag;
    property OnContent;
    property OnLoadExternal;
    property OnTranslateEncoding;
  end;

(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

implementation

(*
===============================================================================================
TEasyXmlScanner
===============================================================================================
*)

procedure TEasyXmlScanner.WhenCData(Content: string);
begin
  inherited WhenContent(Content);
end;

(*
===============================================================================================
INITIALIZATION
===============================================================================================
*)

end.
