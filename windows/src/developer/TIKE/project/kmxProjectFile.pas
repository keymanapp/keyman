(*
  Name:             kmxProjectFile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2015

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
*)
unit kmxProjectFile;   // I4687

interface

uses
  ProjectFile,
  ProjectFiles,
  ProjectFileType;

type
  TkmxProjectFile = class(TOpenableProjectFile)
  protected
    function GetRelativeOrder: Integer; override;
  end;

implementation

{-------------------------------------------------------------------------------
 - TkmxProjectFile                                                             -
 -------------------------------------------------------------------------------}

function TkmxProjectFile.GetRelativeOrder: Integer;
begin
  Result := 12;
end;

initialization
  RegisterProjectFileType('.kmx', TkmxProjectFile);
end.

