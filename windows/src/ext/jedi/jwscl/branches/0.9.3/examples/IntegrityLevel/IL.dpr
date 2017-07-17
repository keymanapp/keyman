{This example shows how to get the integrity label of a file in Windows Vista.}
program IL;

{$APPTYPE CONSOLE}

uses
  Dialogs,
  JwaVista,
  jwaWindows,
  JwsclSecureObjects,
  JwsclDescriptor,
  JwsclMapping,
  JwsclAcl,
  JwsclTypes,
  SysUtils;

var Path : String;
    IsDir : Boolean;
    SD : TJwSecurityDescriptor;
    H : HANDLE;
    Mand : TJwSystemMandatoryAccessControlEntry;
    SO : TJwSecureFileObject;
begin
  Path := ParamStr(1);
  if not FileExists(Path) and not DirectoryExists(Path) then
    exit;
    


  IsDir := not FileExists(Path) and DirectoryExists(Path);

  H := CreateFile(
    PChar(Path),//LPCTSTR lpFileName,
    STANDARD_RIGHTS_READ,//__in          DWORD dwDesiredAccess,
    0,//__in          DWORD dwShareMode,
    nil ,//__in          LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    OPEN_EXISTING,//__in          DWORD dwCreationDisposition,
    FILE_FLAG_BACKUP_SEMANTICS,//__in          DWORD dwFlagsAndAttributes,
    0//__in          HANDLE hTemplateFile
  );

  if H = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  try
    {We could also directly use GetNamedSecurityInfo}
    SD := TJwSecureGeneralObject.GetSecurityInfo(H,SE_FILE_OBJECT,
       [siDaclSecurityInformation,siLabelSecurityInformation]);

    if Assigned(SD) then
    begin
      if IsDir then
        Writeln(SD.DACL.GetTextMap(TJwSecurityFileFolderMapping))
      else
        Writeln(SD.DACL.GetTextMap(TJwSecurityFileMapping));
    end;

    if SD.AuditACL.HasMandatoryLabel then
      Writeln(SD.AuditACL.MandatoryLabel.SID.GetText);

    SD.Free;
  except
    On E : Exception do
      Writeln(E.Message);
  end;
  CloseHandle(H);

  writeln;
  writeln('Another typ of retrieving mandatory label:');
  SO := TJwSecureFileObject.Create(Path);
  try
    Mand := SO.GetMandatoryLabel;
    if Assigned(Mand) then
      Write(Mand.SID.GetText)
    else
      Write('No mandatory label found.');
    write(' ');
    if mpNoWriteUp in Mand.GetMandatoryPolicy then
      write('[NW]');
    if mpNoReadUp in Mand.GetMandatoryPolicy then
      write('[NR]');
    if mpNoExecuteUp in Mand.GetMandatoryPolicy then
      write('[NX]');
    writeln;
    Mand.Free;
  finally
    SO.Free;
  end;




  Writeln;
  writeln('[Hit return]');
  readln;
end.
