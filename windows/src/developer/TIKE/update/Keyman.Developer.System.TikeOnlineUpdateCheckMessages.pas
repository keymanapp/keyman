(*
  Name:             OnlineUpdateCheckMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      15 Jan 2007

  Modified Date:    15 Jan 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          15 Jan 2007 - mcdurdin - Initial version
*)
unit Keyman.Developer.System.TikeOnlineUpdateCheckMessages;

interface

function S_OnlineUpdate_UnableToContact: WideString;
function S_OnlineUpdate_UnableToContact_Error: WideString;

implementation

function S_OnlineUpdate_UnableToContact: WideString;
begin
  Result := 'Unable to contact Keyman website - please make sure you have an active Internet connection and try again.';
end;

function S_OnlineUpdate_UnableToContact_Error: WideString;
begin
  Result := 'Unable to contact Keyman website - please make sure you have an active Internet connection and try again.  The error received was: %0:s';
end;

end.

