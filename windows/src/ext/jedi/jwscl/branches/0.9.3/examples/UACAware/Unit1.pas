unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  jwaWindows, jwsclKnownSid, math, jwsclVersion, jwsclExceptions,
  jwsclToken, jwsclLsa, jwsclSid, jwsclTypes, jwsclImpersonation,jwsclStrings;

type
  TUACService = class(TService)
    procedure ServiceExecute(Sender: TService);
  private
    { Private-Deklarationen }
  public
    function GetServiceController: TServiceController; override;
    { Public-Deklarationen }
  end;

var
  UACService: TUACService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  UACService.Controller(CtrlCode);
end;

function TUACService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;


type  TCreateProcessParameters = record
        lpApplicationName,
        lpCommandLine         :  TJwString;
        lpProcessAttributes,
        lpThreadAttributes    : PSecurityAttributes;
        bInheritHandles       : Boolean;
        dwCreationFlags       : DWORD;
        lpCurrentDirectory    : TJwString;
      end;

     TCreateProcessInfo = record
       StartupInfo: TStartupInfo; //CreateProcess StartupInfo
       //notimpl: AdditionalGroups : TJwSecurityIdList; //zusätzliche Groups fürs Token
       SourceName : AnsiString; //Source des neuen Tokens
       OriginName : AnsiString;

       SessionID  : Cardinal; //SessionID für den neuen Prozess
       UseSessionID : Boolean;

       DefaultDesktop : Boolean; //wenn true wird StartupInfo.lpDesktop := 'WinSta0\Default';
       LogonProcessName : TJwString; //Prozessname für LSA

       //entweder LogonToken oder LogonSID oder keines!
       LogonToken : TJwSecurityToken; //optionales logon Token für Tokengroups - LogonSid wird in diesem Token gesucht
       LogonSID : TJwSecurityID; //optionales logon SID für Tokengroups - eigenes Token

       Parameters : TCreateProcessParameters; //Parameter für CP

       DebugOutput : Boolean; //Debug OutputDaten erstellen
     end;

     TCreateProcessOut = record
       ProcessInfo: TProcessInformation;  //ProcessInfo von CP

       UserToken,             //Normales Token (in Vista ohne Admin)
       LinkedToken : TJwSecurityToken; //elevated Token (in Vista), sonst nil
       IsLinked : Boolean; //true, wenn Vista Elevation aktiv

       ProfInfo: PROFILEINFO; //LoadUserProfile output -> UnloadUserProfile
       EnvironmentBlock: Pointer;  //Environmentblock ->DestroyEnvBlock

       //LSALogonUser Input
       Source: TTokenSource; //Token source
       LSA : TJwSecurityLsa;  //LSA

       //Output von LsaLogonUser
       ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; //->   LsaFreeReturnBuffer(ProfBuffer);
       ProfBufferLen: Cardinal;
       LogonData : PMSV1_0_INTERACTIVE_LOGON; //-> LocalFree(Cardinal(pLogonData));
       TokenLuid: TLUID;  //LUID des neuen Tokens (UserToken + LinkedToken)
       QuotaLimits: QUOTA_LIMITS;
       SubStatus: integer; //Fehler von LSALogonUser

       DebugOutput : String; //Debug OutputDaten
     end;



procedure CreateProcessAsAdminUser(
   const UserName, Domain, Password : TJwString;
   const InVars : TCreateProcessInfo;
   out OutVars : TCreateProcessOut);

procedure DebugOutput(const Str : String);
begin
  if InVars.DebugOutput then
  begin
    OutVars.DebugOutput :=
      OutVars.DebugOutput + #13#10 + Str;
  end;
end;

var pLogonData : PMSV1_0_INTERACTIVE_LOGON;
    Authlen: Cardinal;
    Groups : TJwSecurityIDList;
    LogonSessionSID,
    SID : TJwSecurityID;
    SessionLogonToken : TJwSecurityToken;

    UserToken : TJwSecurityToken;
    StartupInfo: TStartupInfo;
    LastError : DWORD;
//
    fTokenHandle : Cardinal;

    CurrentDirectory,
    AppName, CmdLine : TJwPchar;

begin
  DebugOutput('Calling JwInitWellKnownSIDs...');
   JwInitWellKnownSIDs;
  DebugOutput('...successfully.');

  ZeroMemory(@OutVars, sizeof(OutVars));

  try //1.
    DebugOutput('Enabling tcb privilege...');
    JwEnablePrivilege(SE_TCB_NAME,pst_Enable);
    DebugOutput('...successfully.');


    DebugOutput('Calling TJwSecurityLsa.Create');
    OutVars.LSA := TJwSecurityLsa.Create(InVars.LogonProcessName);
    DebugOutput('...successfully.');

    ZeroMemory(@OutVars.Source.SourceName, 0);
    StrLCopy(@OutVars.Source.SourceName,PChar(InVars.SourceName),
                Min(sizeof(OutVars.Source.SourceName), Length(InVars.SourceName)));
    AllocateLocallyUniqueID(OutVars.Source.SourceIdentifier);


    pLogonData := JwCreate_MSV1_0_INTERACTIVE_LOGON(
                      MsV1_0InteractiveLogon, Domain, UserName, Password, Authlen);

    Groups := TJwSecurityIDList.Create(True);
    try //2.
      if Assigned(InVars.LogonSID) then
      begin
        DebugOutput('Adding user defined LogonSessionID to TokenGroups');
        SID := TJwSecurityId.Create(InVars.LogonSID);
        SID.Attributes:=SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT or
                        SE_GROUP_LOGON_ID;
        Groups.Add(SID); //"Groups" owns SID now
      end
      else
      begin
        SessionLogonToken := nil;
        try //3.
          DebugOutput('Checking Windows Version...');
          if not Assigned(InVars.LogonToken) and
             (TJwWindowsVersion.IsWindowsXP(true) or
             TJwWindowsVersion.IsWindows2003(true) or
             TJwWindowsVersion.IsWindows2003R2(true) or
             TJwWindowsVersion.IsWindowsVista(true) or
             TJwWindowsVersion.IsWindows2008(true)) then
          begin
            DebugOutput('Getting user token from session: '+IntToStr(InVars.SessionID));
            //DebugOutput('Getting user token from session: -1');

            {
            WARNING:
              TCB priv must be available and process must run under SYSTEM account!
            }
            try
              SessionLogonToken := TJwSecurityToken.CreateWTSQueryUserToken(InVars.SessionID);
            except
              {
              Do the old way if we cannot get a connection
              and the session is should be the current session ID.
              This way the code can run in a none service app if the
               session id remains the current one.
              }
              on E : EJwsclWinCallFailedException do
                if (E.LastError = ERROR_ACCESS_DENIED)
                  and ((InVars.SessionID = INVALID_HANDLE_VALUE) or
                       (InVars.SessionID = WTSGetActiveConsoleSessionId))
                   then
                  SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE)
                else
                  raise;
            end;
          end
          else
          begin
            DebugOutput('Getting user token from session which has explorer.exe: ');
            SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE);
          end;
        except //3.
          on E : Exception do
          begin
            SessionLogonToken := nil;
            if InVars.UseSessionID then
            begin
              //DebugOutput('Could not retrieve a LogonSID: '+IntToStr(InVars.SessionID) + ' '+E.Message);
              DebugOutput('Could not retrieve LogonSID ('+IntToStr(InVars.SessionID) + '): '+E.Message);
              raise EJwsclNoSuchLogonSession.CreateFmt('Could not retrieve a LogonSID %d: %s',[InVars.SessionID,E.Message]);
            end
            else
              DebugOutput('Failed to get LogonSID. Omitting step.');
          end;
        end; //3.
      end;

      if Assigned(SessionLogonToken) or Assigned(InVars.LogonToken) then
      begin
        if Assigned(InVars.LogonToken) then
          SessionLogonToken := InVars.LogonToken;
        try
          DebugOutput('Getting LogonSID...');
          try
            LogonSessionSID := JwGetLogonSID(SessionLogonToken);
          except
            on E : Exception do
            begin
              LogonSessionSID := nil;
              DebugOutput('...call to JwGetLogonSID failed: '+E.Message);
            end;
          end;

          if LogonSessionSID <> nil then
          begin
            DebugOutput('Adding LogonSessionID to TokenGroups');
            SID := LogonSessionSID;
            SID.Attributes:=SE_GROUP_MANDATORY or
                            SE_GROUP_ENABLED or
                            SE_GROUP_ENABLED_BY_DEFAULT or
                            SE_GROUP_LOGON_ID;
            Groups.Add(SID); //"Groups" owns SID now
          end;
        finally
          if not Assigned(InVars.LogonToken) then
            FreeAndNil(SessionLogonToken);
        end;
      end;

      DebugOutput('Adding Administrator group to new token groups.');
      SID := TJwSecurityID.Create(JwAdministratorsSID);
      SID.Attributes:=SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT;
      Groups.Add(SID); //"Groups" owns SID now

      {TODO: if Assigned(InVars.AdditionalGroups) then
        Groups.Assign();  }

      DebugOutput('Calling LsaLogonUser...');
      try //4.
        OutVars.LSA.LsaLogonUser(InVars.OriginName, JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                              pLogonData, Authlen, Groups, OutVars.Source,
                           {out...}
                              Pointer(OutVars.ProfBuffer), OutVars.ProfBufferLen,
                              OutVars.TokenLuid, OutVars.UserToken, OutVars.QuotaLimits, OutVars.SubStatus);
      except //4.
        on E: Exception do
        begin
          DebugOutput('LsaLogonUser failed: '+E.Message);
          raise;
        end;
      end; //4.

      DebugOutput('...successfully.');

      FreeAndNil(Groups);

      try
        OutVars.IsLinked := true;
        OutVars.LinkedToken := OutVars.UserToken.LinkedToken;
        UserToken := OutVars.LinkedToken; //temp for CP
      except
        DebugOutput('Could not get Linked Token. Using token from LsaLogonUser');

        OutVars.IsLinked := false;
        OutVars.LinkedToken := nil;
        UserToken := OutVars.UserToken;
      end;

      StartupInfo := InVars.StartupInfo;
      StartupInfo.cb := SizeOf(StartupInfo);

      if InVars.DefaultDesktop then
        StartupInfo.lpDesktop := 'WinSta0\Default';

      if InVars.UseSessionID then
      begin
        DebugOutput('Setting TokenSession ID...');
        try
          UserToken.TokenSessionId := InVars.SessionID;//WtsGetActiveConsoleSessionID;
        except
          on E : Exception do
          begin
            DebugOutput('...Failed to set TokenSessionId: '+E.Message);
            raise;
          end;
        end;
        DebugOutput('...successfully.');
      end;

      ZeroMemory(@OutVars.Profinfo, Sizeof(OutVars.Profinfo));
      OutVars.ProfInfo.dwSize     := Sizeof(OutVars.Profinfo);
      OutVars.ProfInfo.dwFlags    := 1;
      OutVars.Profinfo.lpUserName := TJwPchar(UserName);

      DebugOutput('Loading user profile...');
      if not LoadUserProfile(UserToken.TokenHandle, OutVars.Profinfo) then
      begin
        DebugOutput('LoadUserProfile failed: '+ EJwsclSecurityException.GetLastErrorMessage());
      end;
      DebugOutput('...sucessfully.');

      CreateEnvironmentBlock(@OutVars.EnvironmentBlock, UserToken.TokenHandle, true);

      try //6.
        AppName := nil;
        CmdLine := nil;
        CurrentDirectory := nil;
        
        if Length(InVars.Parameters.lpApplicationName) > 0 then
          AppName := TJwPChar(InVars.Parameters.lpApplicationName);

        if Length(InVars.Parameters.lpCommandLine) > 0 then
          CmdLine := TJwPChar(InVars.Parameters.lpCommandLine);

        if Length(InVars.Parameters.lpCurrentDirectory) > 0 then
          CurrentDirectory := TJwPChar(InVars.Parameters.lpCurrentDirectory);


        DebugOutput('Calling CreateProcessAsUser...');
        SetLastError(0);
        if CreateProcessAsUser(
              UserToken.TokenHandle,//HANDLE hToken,
              AppName,//__in_opt     LPCTSTR lpApplicationName,
              CmdLine, //__inout_opt  LPTSTR lpCommandLine,
              nil,//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpProcessAttributes), //__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
              nil,//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpThreadAttributes),//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
              InVars.Parameters.bInheritHandles,//__in         BOOL bInheritHandles,
              InVars.Parameters.dwCreationFlags,//__in         DWORD dwCreationFlags,
              OutVars.EnvironmentBlock,//__in_opt     LPVOID lpEnvironment,
              CurrentDirectory,//'',//TJwPChar(InVars.Parameters.lpCurrentDirectory),//__in_opt     LPCTSTR lpCurrentDirectory,
              StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
              OutVars.ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
          ) then
        begin
          DebugOutput('Call to CreateProcessAsUser succeeded. Returning.');
        end
        else
        begin
          LastError := GetLastError();
          DebugOutput('CreateProcessAsUser failed: '+ EJwsclSecurityException.GetLastErrorMessage(LastError));
          //DebugOutput('CreateProcessAsUser failed: '+IntToStr(LastError));

          raise EJwsclCreateProcessFailed.CreateFmtEx(
             'CreateProcessAsUser failed.',
             'CreateProcessAsAdminUser', '', '0',
             0, True, ['CreateProcessAsUser']);
        end;
      except //6.
        DebugOutput('Clean up after CreateProcessAsUser failure....');
        DestroyEnvironmentBlock(OutVars.EnvironmentBlock);
          OutVars.EnvironmentBlock := nil;
        UnloadUserProfile(UserToken.TokenHandle, OutVars.Profinfo.hProfile);
          OutVars.Profinfo.hProfile := 0;

        FreeAndNil(OutVars.LinkedToken);
        FreeAndNil(OutVars.UserToken);
        UserToken := nil;

        DebugOutput('...sucessfully.');
      end; //6.

    except //2.
      FreeAndNil(Groups);
      LocalFree(Cardinal(pLogonData)); pLogonData := nil;

      raise;
    end; //2.

    //vars that must be freed
    FreeAndNil(Groups);
    if pLogonData <> nil then
      LocalFree(Cardinal(pLogonData));
  except //1.
    FreeAndNil(OutVars.LSA);
    //LsaFreeReturnBuffer(ProfBuffer);

    raise;
  end; //1.
end;

procedure TUACService.ServiceExecute(Sender: TService);
var
   InVars : TCreateProcessInfo;
   OutVars : TCreateProcessOut;
//
   F : TextFile;
//
   TokenOwner : TJwSecurityId;
   fTokenHandle : Cardinal;
begin

  //AssignFile(f,'E:\Proggen\Borland\Delphi7\Projects\UACAware\_service.txt');
  AssignFile(f,'C:\Windows\_service.txt');
  rewrite(f);
  
  ZeroMemory(@InVars,sizeof(InVars));

  InVars.DebugOutput := true;

  with InVars.StartupInfo do
  begin
    cb          := SizeOf(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := SW_SHOW;
    //lpDesktop := 'WinSta0\Default';
    lpDesktop := nil;
  end;
  //InVars.DefaultDesktop := true;

  //InVars.SessionID := WTSGetActiveConsoleSessionId;
  InVars.SessionID := INVALID_HANDLE_VALUE;
//  InVars.SessionID := 0;
  //InVars.UseSessionID := true;
  InVars.UseSessionID := false;

  InVars.SourceName := 'User32';
  InVars.OriginName := 'UACTest';
  InVars.LogonProcessName := 'UACLogonProcessTest';


  ZeroMemory(@InVars.Parameters, sizeof(InVars.Parameters));

  //InVars.Parameters.lpApplicationName := 'E:\Proggen\Borland\Delphi7\Projects\UACAware\UACAware.exe';
  //InVars.Parameters.lpApplicationName := 'c:\windows\System32\cmd.exe';
  InVars.Parameters.lpApplicationName := 'c:\windows\system32\cmd.exe';
  //InVars.Parameters.lpCommandLine := InVars.Parameters.lpApplicationName+' /service /winstart';
  InVars.Parameters.lpCommandLine := '';
  //InVars.Parameters.lpCommandLine := {InVars.Parameters.lpApplicationName + }'/service /winstart';

  InVars.Parameters.bInheritHandles := true;
  InVars.Parameters.dwCreationFlags := CREATE_NEW_CONSOLE or
     CREATE_UNICODE_ENVIRONMENT;


  Writeln(f,'user: calling CreateProcessAsAdminUser.');
  try
    CreateProcessAsAdminUser(
    '', //UserName
     '', //Domain
     '',//'test',//'test', //Password
     InVars,//InVars : TCreateProcessInfo;
     OutVars//OutVars : TCreateProcessOut
     );

   
     //Auf Beendigung warten
    WaitForSingleObject(OutVars.ProcessInfo.hProcess, INFINITE);

    //Exitcode des Prozesses ermitteln
    //GetExitCodeProcess(ProcessInfo.hProcess, ExitInt);
  except
    On E : Exception do
    begin
      OutVars.DebugOutput := OutVars.DebugOutput +#13#10#13#10 + 'user:'+ E.Message;
    end;
  end;

    Writeln(F, OutVars.DebugOutput);


    FreeAndNil(InVars.LogonToken);

    if OutVars.ProfBuffer <> nil then
      LsaFreeReturnBuffer(OutVars.ProfBuffer);

    if OutVars.LogonData <> nil then
      LocalFree(Cardinal(OutVars.LogonData));

    if OutVars.EnvironmentBlock <> nil then
      DestroyEnvironmentBlock(OutVars.EnvironmentBlock);
    CloseHandle(OutVars.ProcessInfo.hProcess);
    CloseHandle(OutVars.ProcessInfo.hThread);
    FreeAndNil(OutVars.UserToken);
    FreeAndNil(OutVars.LinkedToken);

   CloseFile(F);
  {fTokenHandle := 0;
  Sleep(2000);
  SetLastError(0);
  if not WTSQueryUserToken(0, fTokenHandle) then
    writeln(F, WtsGetActiveConsoleSessionID,' ', EJwsclSecurityException.GetLastErrorMessage())
  else
    writeln(F,'OK');

  CloseFile(F);
  Sleep(2000);
  CloseHandle(fTokenHandle);    }
end;

(*procedure TUACService.ServiceExecute(Sender: TService);
var
    LogonToken,
    NewToken,
    UserToken,
    LinkedToken : TJwSecurityToken;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    ExitInt : Cardinal;
    F : TextFile;
    I,
    Authlen: Cardinal;

    LSA : TJwSecurityLsa;
    plogonData : PMSV1_0_INTERACTIVE_LOGON;
    AddGroups : TJwSecurityIDList;
    Source: TTokenSource;

    aLogonSID,
    SID: TJwSecurityID;
    SidList : TJwSecurityIdList;

    ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; ProfBufferLen: Cardinal;
    TokenLuid: TLUID; QuotaLimits: QUOTA_LIMITS; SubStatus: integer;

    ProfInfo: PROFILEINFO;
    EnvirBlock: Pointer;
    isLinked : boolean;
begin
  //

  AssignFile(f,'E:\Proggen\Borland\Delphi7\Projects\UACAware\_service.txt');
  rewrite(f);
  Writeln(f,'1');
  try

  JwInitWellKnownSIDs;

  try
    JwEnablePrivilege(SE_TCB_NAME,pst_Enable);

    Writeln(f,'1');
    LSA:=TJwSecurityLsa.Create('StartApplicationsAsAdmin');
    Writeln(f,'2');
    {UserToken := TJwSecurityToken.CreateLogonUser(,
       LOGON32_LOGON_INTERACTIVE,LOGON32_PROVIDER_DEFAULT);}
    pLogonData:=JwCreate_MSV1_0_INTERACTIVE_LOGON(MsV1_0InteractiveLogon, '', 'TestBenutzer','test', Authlen);
    try
      Writeln(f,'3');
      Source.SourceName:='User32';
      AllocateLocallyUniqueID(Source.SourceIdentifier);

      Writeln(f,'4');
      AddGroups := TJwSecurityIDList.Create(True);

      LogonToken := TJwSecurityToken.CreateWTSQueryUserToken();
      try
        aLogonSid := JwGetLogonSID(LogonToken);
      finally
        LogonToken.Free;
      end;
      if aLogonSid <> nil then
      begin
        SID := aLogonSid;
        SID.Attributes:=SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT or
                        SE_GROUP_LOGON_ID;
        AddGroups.Add(SID);
      end;
        SID:=TJwSecurityID.Create(JwAdministratorsSID);
        SID.Attributes:=SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT;
        AddGroups.Add(SID);
        {SID:=TJwSecurityID.Create(JwHighIL);
        SID.Attributes:= SE_GROUP_INTEGRITY_ENABLED;
        AddGroups.Add(SID);   }
        try
          Lsa.LsaLogonUser('UserAsAdmin', JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                              pLogonData, Authlen, AddGroups, Source,
                           {out...}
                              Pointer(ProfBuffer), ProfBufferLen,
                              TokenLuid, NewToken, QuotaLimits, SubStatus);
        except
          on E: Exception do
          begin
            Writeln(f,'Logon of User failed Status: ',SubStatus);
            raise;
          end;
        end;
      Writeln(f,'5a');
      {LinkedToken := UserToken.LinkedToken;
      LinkedToken.TokenSessionId := WtsGetActiveConsoleSessionID;}
      try
        isLinked := true;
        LinkedToken := NewToken.LinkedToken;
      except
        Writeln(f,'*** No linked token');
        LinkedToken := NewToken;
        isLinked := false;
      end;
      Writeln(f,'5a_1');

      if Assigned(LinkedToken) then
      begin
        writeln(f,'LinkedToken');
        SidList := LinkedToken.TokenGroups;
        try
          Writeln(F,SidList.GetText(false));
        finally
          SidList.Free;
        end;
      end;




      Writeln(f,'5d');

      //StartupInfo-Struktur initialisieren
      FillChar(StartupInfo, SizeOf(StartupInfo), #0);

      //Werte zuweisen
      StartupInfo.cb          := SizeOf(StartupInfo);
      StartupInfo.dwFlags     := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
      StartupInfo.wShowWindow := SW_SHOW;
      StartupInfo.lpDesktop := 'WinSta0\Default';

      LinkedToken.TokenSessionId := WtsGetActiveConsoleSessionID;

      ZeroMemory(@Profinfo, Sizeof(Profinfo));
      ProfInfo.dwSize:=Sizeof(Profinfo);
      ProfInfo.dwFlags:=1;
      Profinfo.lpUserName:='UserAsAdmin';

      if not LoadUserProfile(LinkedToken.TokenHandle, Profinfo) then
      begin
        Writeln(f,'LoadUserProfile failed');
      end;
      CreateEnvironmentBlock(@Envirblock, LinkedToken.TokenHandle, false);

      Writeln(f,'6');

      try
        //Prozess starten
        if CreateProcessAsUser(
          LinkedToken.TokenHandle,
          //NewToken.TokenHandle,
          //'c:\windows\System32\cmd.exe',
          'E:\Proggen\Borland\Delphi7\Projects\UACAware\UACAware.exe',
          '',
          NIL,
          NIL,
          True,
          NORMAL_PRIORITY_CLASS,
          NIL,
          NIL,
          StartupInfo,
          ProcessInfo)
        then
        begin
          Writeln(f,'7');
          //Auf Beendigung warten
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

          //Exitcode des Prozesses ermitteln
          GetExitCodeProcess(ProcessInfo.hProcess, ExitInt);

          //Handles freigeben
          if ProcessInfo.hProcess <> 0 then
            CloseHandle(ProcessInfo.hProcess);
          if ProcessInfo.hThread <> 0 then
            CloseHandle(ProcessInfo.hThread);
        end
        else
        begin

          Writeln(f,'raisLastError');
          RaiseLastOSError;
        end;
      finally
        UnloadUserProfile(LinkedToken.TokenHandle,ProfInfo.hProfile);
        DestroyEnvironmentBlock(Envirblock);
      end;
    finally
       Writeln(f,'8');

      LsaFreeReturnBuffer(ProfBuffer);
      LocalFree(Cardinal(pLogonData));
      //UserToken.Free;
      if isLinked then
        FreeAndNil(LinkedToken);
      FreeAndNil(NewToken);
      LSA.Free;
    end;
  except
  on E : Exception do
  begin
    Writeln(f,PCHAR(E.Message));
  end;
 end;
 finally
   CloseFile(f);
 end;
end;
        *)
end.
