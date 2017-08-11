{******************************************************************************}
{ JEDI API example <name> 													   }
{ http://jedi-apilib.sourceforge.net										   }
{ 																			   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																			   }
{ Author(s): Benjamin Schwarze														   }
{ Creation date: 29th April 2008 					   				   }
{ Last modification date:	5th June 2008												   }
{ 																			   }
{ Description: It demonstrates monitoring active jobs, adding new jobs and }
{ 						 accessing the finale files													   }
{ 																			   }
{ Preparations: JWA must be ready to use.							   }
{ 																			   }
{ Article link: http://blog.delphi-jedi.net/2008/04/30/bits-part-2-downloading-files/							   }
{ 																			   }
{ Version history: 29th April 2008											   }
{ 																			   }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 		   }
{ productive environments.													   }
{ The code has surely some errors that need to be fixed. In such a case	   	   }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.													   }
{******************************************************************************}

unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComObj, StdCtrls, ActiveX,
{$IFDEF JWA_WINDOWS}
  JwaBits,
{$ELSE}
  JwaWindows,
{$ENDIF JWA_WINDOWS}
  ExtCtrls, ComCtrls, Menus;

type
  TForm1 = class(TForm, IBackgroundCopyCallback)
    btn_AddJob: TButton;
    lv_Jobs: TListView;
    tm_RefreshList: TTimer;
    pum_Jobs: TPopupMenu;
    Cancel1: TMenuItem;
    Resume1: TMenuItem;
    Suspend1: TMenuItem;
    Complete1: TMenuItem;
    dlg_Save: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_AddJobClick(Sender: TObject);
    procedure tm_RefreshListTimer(Sender: TObject);
    procedure Cancel1Click(Sender: TObject);
    procedure Resume1Click(Sender: TObject);
    procedure Suspend1Click(Sender: TObject);
    procedure Complete1Click(Sender: TObject);
  private
    FManager : IBackgroundCopyManager;

    {$REGION 'IBackgroundCopyCallback'}
    function JobTransferred(pJob: IBackgroundCopyJob): HRESULT; stdcall;
    function JobError(pJob: IBackgroundCopyJob; pError: IBackgroundCopyError): HRESULT; stdcall;
    function JobModification(pJob: IBackgroundCopyJob; dwReserved: DWORD): HRESULT; stdcall;
    {$ENDREGION}
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn_AddJobClick(Sender: TObject);
var
  Job : IBackgroundCopyJob;
  JobID : TGUID;
begin
  if dlg_Save.Execute then
  begin
    if Succeeded(FManager.CreateJob('My Download',
                                     BG_JOB_TYPE_DOWNLOAD,
                                     JobID,
                                     Job)) then
    begin

      if Succeeded(Job.AddFile('http://blog.delphi-jedi.net/wp-content/uploads/2008/04/jwabits.zip',
        PWideChar(WideString(dlg_Save.FileName)))) then
        Job.SetNotifyInterface(Self);
    end;
  end;
end;

procedure TForm1.Cancel1Click(Sender: TObject);
var
  Job : IBackgroundCopyJob;
begin
  if lv_Jobs.ItemIndex > -1 then
  begin
    FManager.GetJob(StringToGUID(lv_Jobs.Selected.SubItems[1]), Job);
    Job.Cancel;
  end;
end;

procedure TForm1.Complete1Click(Sender: TObject);
var
  Job : IBackgroundCopyJob;
begin
  if lv_Jobs.ItemIndex > -1 then
  begin
    FManager.GetJob(StringToGUID(lv_Jobs.Selected.SubItems[1]), Job);
    Job.Complete;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Res : HRESULT;
begin         
  Res := CoCreateInstance(CLSID_BackgroundCopyManager,
                        nil,
                        CLSCTX_LOCAL_SERVER,
                        IID_IBackgroundCopyManager,
                        FManager);
  if not Succeeded(Res) then
    raise Exception.Create('Can not create BackgroundCopyManager. Is the service running?');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FManager := nil;
end;

function TForm1.JobError(pJob: IBackgroundCopyJob;
  pError: IBackgroundCopyError): HRESULT;
begin
  Result := S_OK;
end;

function TForm1.JobModification(pJob: IBackgroundCopyJob;
  dwReserved: DWORD): HRESULT;
begin
  Result := S_OK;

  tm_RefreshListTimer(nil);
end;

function TForm1.JobTransferred(pJob: IBackgroundCopyJob): HRESULT;
begin
  Result := S_OK;

  pJob.Complete;
end;

procedure TForm1.Resume1Click(Sender: TObject);
var
  Job : IBackgroundCopyJob;
begin
  if lv_Jobs.ItemIndex > -1 then
  begin
    FManager.GetJob(StringToGUID(lv_Jobs.Selected.SubItems[1]), Job);
    Job.Resume;
  end;
end;

procedure TForm1.Suspend1Click(Sender: TObject);
var
  Job : IBackgroundCopyJob;
begin
  if lv_Jobs.ItemIndex > -1 then
  begin
    FManager.GetJob(StringToGUID(lv_Jobs.Selected.SubItems[1]), Job);
    Job.Suspend;
  end;
end;

procedure TForm1.tm_RefreshListTimer(Sender: TObject);
var
  Job : IBackgroundCopyJob;
  Jobs : IEnumBackgroundCopyJobs;
  Res : HRESULT;
  DisplayName : PWideChar;
  Fetched : ULong;
  State : BG_JOB_STATE;
  JobID : TGUID;

  Item : TListItem;
begin
  lv_Jobs.Clear;

  Res := FManager.EnumJobs(BG_JOB_ENUM_ALL_USERS, Jobs);

  if not Succeeded(Res) then
    Res := FManager.EnumJobs(0, Jobs);

  if not Succeeded(Res) then
    raise Exception.Create('Can not enum BackgroundCopyJobs');

  while Succeeded(Jobs.Next(1, Job, @Fetched)) and (Fetched = 1) do
  begin
    Item := lv_Jobs.Items.Add;
    Item.SubItems.Add('unknown');  //state of the job
    Item.SubItems.Add('unknown');  //GUID of the job

    if Succeeded(Job.GetDisplayName(DisplayName)) then
    begin
      Item.Caption := DisplayName;

      CoTaskMemFree(DisplayName);
    end;

    if Succeeded(Job.GetState(State)) then
    begin
      case State of
        BG_JOB_STATE_QUEUED:          Item.SubItems[0] := 'queued';
        BG_JOB_STATE_CONNECTING:      Item.SubItems[0] := 'connecting';
        BG_JOB_STATE_TRANSFERRING:    Item.SubItems[0] := 'transfering';
        BG_JOB_STATE_SUSPENDED:       Item.SubItems[0] := 'suspended';
        BG_JOB_STATE_ERROR:           Item.SubItems[0] := 'error';
        BG_JOB_STATE_TRANSIENT_ERROR: Item.SubItems[0] := 'transient error';
        BG_JOB_STATE_TRANSFERRED:     Item.SubItems[0] := 'transferred';
        BG_JOB_STATE_ACKNOWLEDGED:    Item.SubItems[0] := 'acknowledged';
        BG_JOB_STATE_CANCELLED:       Item.SubItems[0] := 'cancelled';
      end;
    end;

    if Succeeded(Job.GetId(JobID)) then
      Item.SubItems[1] := GUIDToString(JobID);

    Job := nil;
 
  end;
end;

initialization
  CoInitFlags := COINIT_APARTMENTTHREADED;

end.
