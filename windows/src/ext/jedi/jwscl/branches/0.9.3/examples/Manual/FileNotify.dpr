program FileNotify;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclToken,
  JwsclUtils,
  JwsclTypes;


var
  pBuf         : Pointer;
  dwBufLen     : DWORD;
  dwRead       : DWORD;
  FNI          : PFILE_NOTIFY_INFORMATION absolute pBuf;
  pWork        : Pointer;
  sFileName    : Widestring;
  dwWaitStatus : DWORD;
  hNotifity,
  FhFile : HANDLE;

  Token : TJwSecurityToken;

const MyPath = 'E:\temp\mytemp';
begin
  JwEnablePrivilege(SE_CHANGE_NOTIFY_NAME,pst_Disable);

  write(SE_CHANGE_NOTIFY_NAME,' ');
  if JwIsPrivilegeSet(SE_CHANGE_NOTIFY_NAME) then
    writeln('is available')
  else
    writeln('is NOT available');


  FhFile    := CreateFile(PChar(MyPath),
                          FILE_LIST_DIRECTORY or GENERIC_READ,
                          FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
                          OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,0);
                         
  hNotifity := FindFirstChangeNotification(PChar(MyPath),                    //Verzeichnis
                                           cardinal(false),                               //unterverzeichnisse überwachen
                                           FILE_NOTIFY_CHANGE_FILE_NAME or
                                           FILE_NOTIFY_CHANGE_LAST_WRITE or
                                           FILE_NOTIFY_CHANGE_SIZE or
                                           FILE_ACTION_ADDED or
                                           FILE_ACTION_REMOVED or
                                           FILE_ACTION_MODIFIED);
  if (FhFile = INVALID_HANDLE_VALUE) or (FhFile = 0) then
  begin
    RaiseLastWin32Error;
  end;
  if (hNotifity = INVALID_HANDLE_VALUE) then
  begin
    RaiseLastWin32Error;
  end;

  dwBufLen := 65536;
  pBuf     := AllocMem(dwBufLen);
  try
    while ((FindNextChangeNotification(hNotifity))) do
    begin
      dwWaitStatus := WaitForSingleObject(hNotifity, INFINITE);
      if (dwWaitStatus = WAIT_FAILED) then
      begin
        RaiseLastWin32Error;
      end;
      //if (dwWaitStatus = WAIT_OBJECT_0) then
      begin
        ReadDirectoryChangesW(FhFile,pBuf,dwBufLen,true,
                              FILE_NOTIFY_CHANGE_FILE_NAME or
                              FILE_NOTIFY_CHANGE_DIR_NAME or
                              FILE_NOTIFY_CHANGE_ATTRIBUTES or
                              FILE_NOTIFY_CHANGE_SIZE or
                              FILE_NOTIFY_CHANGE_LAST_WRITE or
                              FILE_NOTIFY_CHANGE_CREATION or
                              FILE_ACTION_ADDED or
                              FILE_ACTION_REMOVED or
                              FILE_ACTION_MODIFIED,
                              @dwRead,nil,nil);
        pWork := pBuf;
        repeat


          writeln(FNI.Action,' : ', String(WideCharToString(FNI.FileName)));

          Inc(Integer(pBuf), FNI.NextEntryOffset);
        until FNI.NextEntryOffset = 0;
      end;
    end;

  finally
    FreeMem(pBuf,dwBufLen);
  end;
end.
