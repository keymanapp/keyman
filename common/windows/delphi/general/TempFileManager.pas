(*
  Name:             TempFileManager
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      24 Apr 2014

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Apr 2014 - mcdurdin - I4195 - V9.0 - Use TTempFileManager for all temporary files
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit TempFileManager;   // I4195   // I4181

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TTempFileManager = class;

  TTempFile = class
  private
    FOwner: TTempFileManager;
    FBaseName: string;
    FName: string;
  public
    constructor Create(AOwner: TTempFileManager; const AExt: string);
    destructor Destroy; override;
    property BaseName: string read FBaseName;
    property Name: string read FName;
  end;

  TTempFiles = class(TObjectList<TTempFile>)
  end;

  TTempFileManager = class
  private
    class var FInstance: TTempFileManager;
    FDeleteOnDestroyFiles: TStringList;
    FTempFiles: TTempFiles;
  protected
    procedure Delete(const Item: TTempFile);
    class function Instance: TTempFileManager;
    constructor Create;
  public
    destructor Destroy; override;
    class function Get(const AExtension: string = ''): TTempFile;
  end;

implementation

uses
  Winapi.Windows,
  klog,
  System.SysUtils,

  utildir;

{ TTempFileManager }

constructor TTempFileManager.Create;
begin
  inherited Create;
  FTempFiles := TTempFiles.Create(False);
  FDeleteOnDestroyFiles := TStringList.Create;
end;

procedure TTempFileManager.Delete(const Item: TTempFile);
var
  n: Integer;
begin
  FTempFiles.Remove(Item);

  n := FDeleteOnDestroyFiles.IndexOf(Item.Name);
  if n >= 0 then
  begin
    if not FileExists(Item.Name) or DeleteFileCleanAttr(Item.Name) then   // I4574
      FDeleteOnDestroyFiles.Delete(n);
  end;

  // If BaseName = Name, then n = -1, so safe
  n := FDeleteOnDestroyFiles.IndexOf(Item.BaseName);
  if n >= 0 then
  begin
    if not FileExists(Item.BaseName) or DeleteFileCleanAttr(Item.BaseName) then   // I4574
      FDeleteOnDestroyFiles.Delete(n);
  end;
end;

destructor TTempFileManager.Destroy;
var
  i: Integer;
begin
  //
  // Schedule for deletion any files that weren't removed from the tempfilemanager
  // Technically a fault somewhere else in the code
  //

  for i := FTempFiles.Count - 1 downto 0 do
  begin
    KL.Log('Temp file remaining at destruction - '+FTempFiles[i].Name);
    FTempFiles[i].Free;
  end;

  FreeAndNil(FTempFiles);

  // Try and delete any files that were in use during the session
  // Silently ignore any errors -- "it's just a temp file..."

  for i := 0 to FDeleteOnDestroyFiles.Count - 1 do
    if FileExists(FDeleteOnDestroyFiles[i]) then
      DeleteFileCleanAttr(FDeleteOnDestroyFiles[i]);   // I4574

  FreeAndNil(FDeleteOnDestroyFiles);
  inherited Destroy;
end;

class function TTempFileManager.Get(const AExtension: string): TTempFile;
begin
  Result := TTempFile.Create(Instance, AExtension);
  Instance.FDeleteOnDestroyFiles.Add(Result.Name);
  if Result.Name <> Result.BaseName then
    Instance.FDeleteOnDestroyFiles.Add(Result.BaseName);
  Instance.FTempFiles.Add(Result);
end;

class function TTempFileManager.Instance: TTempFileManager;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TTempFileManager.Create;
  end;
  Result := FInstance;
end;

{ TTempFile }

constructor TTempFile.Create(AOwner: TTempFileManager; const AExt: string);
var
  buf: array[0..260] of char;
  FPath: string;
begin
  inherited Create;
  FOwner := AOwner;

  FPath := ExcludeTrailingPathDelimiter(KGetTempPath);

  if GetTempFileName(PChar(FPath), 'kmn', 0, buf) = 0 then
    RaiseLastOSError;

  FBaseName := buf;

  if (AExt <> '')
    then FName := ChangeFileExt(FBaseName, AExt)
    else FName := FBaseName;
end;

destructor TTempFile.Destroy;
begin
  FOwner.Delete(Self);
  inherited Destroy;
end;

initialization
finalization
  FreeAndNil(TTempFileManager.FInstance);
end.
