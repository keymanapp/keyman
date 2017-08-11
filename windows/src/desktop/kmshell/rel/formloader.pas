(*
  Name:             formloader
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Dec 2006

  Modified Date:    5 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Dec 2006 - mcdurdin - ?
*)
unit formloader;

interface

uses Windows, Classes, Controls, Forms;

function Show_frmHTML(AParent: TComponent; const ACaption, AText, AFileName: string; AHelpContext: Integer): Integer;
procedure Main;

implementation


end.
