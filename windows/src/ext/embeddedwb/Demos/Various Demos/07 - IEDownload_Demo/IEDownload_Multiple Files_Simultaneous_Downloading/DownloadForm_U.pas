//*************************************************************************
//                                                                        *
//                     IE Downloag Mgr                                    *
//                       For Delphi                                       *
//                                                                        *
//                     Freeware Demo                                      *
//  By:                                                                   *
//  Eran Bodankin -bsalsa(bsalsa@gmail.com)                               *
//                                                                        *
//                                                                        *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit DownloadForm_U;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  IEDownload, ComCtrls, UrlMon, ActiveX, DownloadRequestForm,
  ShellApi, Menus, ExtCtrls, IEDownloadTools, Graphics;

const
  SEE_MASK_NOZONECHECKS = $00800000;

type
  TDownloadForm = class(TForm)

    ListView: TListView;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IEDownloadStartBinding(Sender: TBSCB; var Cancel: Boolean; pib:
    IBinding; const FileName: WideString; const FileSize: integer);
    procedure IEDownloadProgress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded, ElapsedTime,
      Speed, RemainingTime, Status, Percent: string);
  private
  protected
  public
    { Public declarations }
    procedure FileDownload(inUrl: string; pmk: IMoniker; pbc: IBindCtx);

  end;

var
  IED: TIEDownload;
  DownloadForm: TDownloadForm;

implementation

{$R *.dfm}


procedure TDownloadForm.FormDestroy(Sender: TObject);
begin
  DownloadForm := nil;
end;

procedure TDownloadForm.FileDownload(inUrl: string; pmk: IMoniker; pbc:
  IBindCtx);
begin
  if (not Visible) then
    Show
  else
    BringToFront;
    IED := TIEDownload.Create(Self);
  try
    with IED do
    begin
      OnProgress:= IEDownloadProgress;
      OnStartBinding:= IEDownloadStartBinding;
      Go(inUrl);
    end;
  finally
  if IED <> nil then
    FreeAndNil(IED);
  end;
end;


function FindStringInLV(listItems: TListItems; inString: string): Integer;
var
  idx: Integer;
begin
  Result := -1;
  for idx := 0 to listItems.Count - 1 do
    if (listItems[idx].Caption = inString) then
    begin
      Result := idx;
      Break;
    end;
end;

function FindThreadIDInLV(listItem: TListItems; SubIdx, ID: integer): Integer;
var
  idx: Integer;
begin
  Result := -1;
  for idx := 0 to listItem.Count - 1 do
    if (listItem[idx].SubItems[SubIdx] = IntToStr(ID)) then
    begin
      Result := idx;
      Break;
    end;
end;

procedure TDownloadForm.IEDownloadProgress(Sender: TBSCB; ulProgress,
  ulProgressMax, ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
  ElapsedTime, Speed, RemainingTime, Status, Percent: string);
var
  lvItem: TListItem;
  idx: integer;
  pb: TProgressBar;
begin
  if ListView.Items.Count = 0 then
    Exit;
  begin {New downlaod}
    idx := FindThreadIDInLV(Listview.Items, 5, Sender.ThreadID);
    lvItem := ListView.Items[idx];
    if (Assigned(lvItem)) then
    begin
      lvItem.SubItems[0] := Status;
      if ((ulStatusCode = BINDSTATUS_ENDDOWNLOADDATA) or
        (ulStatusCode = BINDSTATUS_DOWNLOADINGDATA)) then
      begin
        with lvItem do
        begin
          SubItems[1] := FormatSize(FileSize);
          SubItems[2] := Percent;
          SubItems[3] := Speed;
          SubItems[4] := RemainingTime;
      //    SubItems[6] := IntToStr(IED.ActiveConnections);
        end;
        pb := TProgressBar(Listview.Items[idx].Data);
        pb.Max := ulProgressMax;
        pb.Position := ulProgress;
      end;
    end;
  end;
end;

procedure TDownloadForm.IEDownloadStartBinding(Sender: TBSCB; var Cancel: Boolean; pib:
    IBinding; const FileName: WideString; const FileSize: integer);
var
  DownloadRequest: TDownloadRequest;
  ModRes: TModalResult;
  lvItem: TListItem;
  idx: Integer;
  pbRect: TRect;
  pb: TProgressBar;
begin
  idx := FindStringInLV(Listview.Items, IED.FileName);
  if (idx > -1) then
  begin
    lvItem := Listview.Items[idx];
    with lvItem do
    begin
      SubItems[0] := 'Resuming...';
      SubItems.Add(''); // Size
      SubItems.Add(''); // Progress
      SubItems.Add(''); // Speed
      SubItems.Add('UnKnown'); // Remaining Time
      SubItems.Add(IntToStr(Sender.ThreadID));
      //SubItems.Add(IntToStr(IED.ActiveConnections));
      SubItems.Add(''); //ProgressBar
    end;
    pb := TProgressBar.Create(nil);
    pb.Parent := Listview;
    lvItem.Data := pb;
    pbRect := lvItem.DisplayRect(drBounds);
    pbRect.Left := 540 + pbRect.Left + ListView.Columns[7].Width;
    pbRect.Right := pbRect.Left+ Listview.Columns[7].Width+50;
    pb.BoundsRect := pbRect;
  end
  else
  begin
  DownloadRequest := TDownloadRequest.Create(nil);
  try
    DontOpenThisExts.Add('msi');
    DownloadRequest.SetInfo(IED);
    modres := DownloadRequest.ShowModal;
    case ModRes of
      mrCancel:
      begin
        Cancel := True;
        DownloadRequest.Close;
      end;
      mrOpen:
        begin
          IED.OpenDownloadFolder := True;
        end;
    mrSave:
     end;
   idx := FindThreadIDInLV(Listview.Items, 5, Sender.ThreadID);
   if (idx > -1) then
     lvItem := ListView.Items[idx]
  else
    lvItem := Listview.Items.Add;
    with lvItem do
    begin
      Caption := IED.FileName;
      SubItems.Add('Waiting'); // Status
      SubItems.Add(''); // Size
      SubItems.Add(''); // Progress
      SubItems.Add(''); // Speed
      SubItems.Add('UnKnown'); // Remaining Time
      SubItems.Add(IntToStr(Sender.ThreadID));
   //   SubItems.Add(IntToStr(IED.ActiveConnections));
      SubItems.Add(''); //ProgressBar
    end;
    pb := TProgressBar.Create(nil);
    pb.Parent := Listview;
    lvItem.Data := pb;
    pbRect := lvItem.DisplayRect(drBounds);
    pbRect.Left := 540 + pbRect.Left + ListView.Columns[7].Width;
    pbRect.Right := pbRect.Left+ Listview.Columns[7].Width+50;
    pb.BoundsRect := pbRect;
    finally
    DownloadRequest.Free;
  end;
  end;
end;

procedure TDownloadForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if IED<> nil then
  begin
    if IED.Busy then
      IED.CancelAll;
  end;
end;

end.
