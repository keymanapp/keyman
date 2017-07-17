unit UMainForm;

interface

uses
  JwaWindows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  ProcessList,
  JwsclToken,
  JwsclProcess,
  JwsclUtils,
  JwsclTypes,
  JwsclResource,
  JwsclKnownSid,
  JwsclExceptions,
  JwsclStrings, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    SessionEdit: TEdit;
    ListView1: TListView;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
    ProcessList : TProcessList;

    procedure OnListChange(Sender : TProcessList; Index : Integer;
       ProcessEntry : TProcessEntry;
       ChangeType : TListChangeType);

  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};
  Output : TJwProcessOutputInformation;
begin
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  JwCreateProcessInSession('c:\windows\system32\cmd.exe','','',
    StrToIntDef(SessionEdit.Text, 1),//const SessionID : DWORD;
    CREATE_NEW_CONSOLE, //const CreationFlags : DWORD;
    'default',// const Desktop: TJwString;
    StartupInfo,// : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};
    false,//WaitForProcess : Boolean;
    Output,//out Output : TJwProcessOutputInformation;
    nil//LogServer : IJwLogServer
   );
  DestroyEnvironmentBlock(Output.EnvBlock);
  Output.UserToken.Free;
  CloseHandle(Output.ProcessInfo.hThread);

  ProcessList.Add(Output.ProcessInfo.hProcess);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ProcessList := nil;

  JwInitWellKnownSIDs;
  ProcessList := TProcessList.Create;
  ProcessList.OnListChange := OnListChange;
  ProcessList.CloseAllOnDestroy := true;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ProcessList.Free;
end;

procedure TForm1.OnListChange(Sender: TProcessList; Index: Integer;
  ProcessEntry : TProcessEntry;
  ChangeType: TListChangeType);
var i : Integer;
  L : TListItem;
begin
  case ChangeType of
    lctAdd :
      begin
        L := ListView1.Items.Add;
        L.Caption := IntToStr(ProcessEntry.Session);
        L.SubItems.Add(IntToStr(ProcessEntry.ID));
        L.SubItems.Add(IntToHex(ProcessEntry.Handle,6));
        L.Data := Pointer(ProcessEntry.ID);
      end;
    lctRemove :
      begin
        for i := ListView1.Items.Count-1 downto 0 do
        begin
          if ListView1.Items[i].Data = Pointer(ProcessEntry.ID) then
            ListView1.Items.Delete(i);
        end;
      end;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not JwCheckAdministratorAccess then
  begin
    //MessageDlg('This application needs administrative powers! Closing now...', mtError, [mbOK], 0);
   // Close;
    //exit;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TerminateProcess(StrToInt('$'+ListView1.Selected.SubItems[1]), 0);
end;

end.
