(*
  Name:             httpuploader_messageprocessor_forms
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jun 2007

  Modified Date:    4 Jun 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jun 2007 - mcdurdin - Initial version
*)
unit httpuploader_messageprocessor_forms;

interface

implementation

uses
  Forms,
  httpuploader,
  SysUtils;

type
  THTTPUploader_MessageProcessor_Forms = class(THTTPUploader_MessageProcessor)
  public
    procedure ProcessMessages; override;
  end;

{ THTTPUploader_MessageProcessor_Forms }

procedure THTTPUploader_MessageProcessor_Forms.ProcessMessages;
begin
  Application.ProcessMessages;
end;

initialization
  FHTTPUploader_MessageProcessor := THTTPUploader_MessageProcessor_Forms.Create;
finalization
  FreeAndNil(FHTTPUploader_MessageProcessor);
end.
