(*
  Name:             MSXMLDomCreate
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Nov 2011

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Nov 2011 - mcdurdin - Fix MSXML 6.0 default properties changed and preventing XSLT from working
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Use old MSXML settings for back compat
*)
unit MSXMLDomCreate;  // I3309

interface

implementation

uses
  Xml.Win.msxmldom, Xml.XmlDoc;

initialization
  MSXMLDOMDocumentFactory.AddDOMProperty('AllowDocumentFunction', True);
  MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  UseXSDBooleanStrings := False;
end.
