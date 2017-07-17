(*
  Name:             WebSoundControl
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      29 Mar 2010

  Modified Date:    29 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          29 Mar 2010 - mcdurdin - I2268 - Disable click sound in web browser
*)
unit WebSoundControl;

interface

implementation

uses Windows;

procedure DisableInternetSounds;
type
  TCoInternetSetFeatureEnabled = function(FeatureEntry: DWORD; Flags: DWORD; fEnable: BOOL): HRESULT; stdcall;
const
  FEATURE_DISABLE_NAVIGATION_SOUNDS = 21;
  SET_FEATURE_ON_THREAD = $00000001;
  SET_FEATURE_ON_PROCESS = $00000002;
  SET_FEATURE_IN_REGISTRY = $00000004;
  SET_FEATURE_ON_THREAD_LOCALMACHINE = $00000008;
  SET_FEATURE_ON_THREAD_INTRANET = $00000010;
  SET_FEATURE_ON_THREAD_TRUSTED = $00000020;
  SET_FEATURE_ON_THREAD_INTERNET = $00000040;
  SET_FEATURE_ON_THREAD_RESTRICTED = $00000080;
var
  FCoInternetSetFeatureEnabled: TCoInternetSetFeatureEnabled;
  FUrlMon: THandle;
begin
  FUrlMon := LoadLibrary('urlmon.dll');
  if FUrlMon <> 0 then
  begin
    FCoInternetSetFeatureEnabled := GetProcAddress(FUrlMon, 'CoInternetSetFeatureEnabled');
    if Assigned(FCoInternetSetFeatureEnabled) then
      FCoInternetSetFeatureEnabled(FEATURE_DISABLE_NAVIGATION_SOUNDS, SET_FEATURE_ON_PROCESS, TRUE);
  end;
end;

initialization
  DisableInternetSounds;
end.
