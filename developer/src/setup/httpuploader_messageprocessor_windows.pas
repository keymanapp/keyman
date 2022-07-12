(*
  Name:             httpuploader_messageprocessor_windows
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jun 2007

  Modified Date:    23 Aug 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jun 2007 - mcdurdin - Initial version
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
*)
unit httpuploader_messageprocessor_windows;

interface

implementation

uses
  httpuploader,
  SysUtils;

type
  THTTPUploader_MessageProcessor_Windows = class(THTTPUploader_MessageProcessor)
  public
    procedure ProcessMessages; override;
  end;

{ THTTPUploader_MessageProcessor_Forms }

procedure THTTPUploader_MessageProcessor_Windows.ProcessMessages;
begin
//  Application.ProcessMessages;
end;

initialization
  FHTTPUploader_MessageProcessor := THTTPUploader_MessageProcessor_Windows.Create;
finalization
  FHTTPUploader_MessageProcessor.Free;
  FHTTPUploader_MessageProcessor := nil;
end.
