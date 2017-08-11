{******************************************************************************}
{ JEDI API example Windows Update Api             			                       }
{ http://jedi-apilib.sourceforge.net					                                 }
{ 									                                                           }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 									                                                           }
{ Author(s): stOrM!, Christian Wimmer   			                                 }
{ Creation date: 16th June 2008 				 	                                     }
{ Last modification date:	18th June 2008 			                                 }
{ 									                                                           }
{ Description: Demonstrates how to use the Windows Update Api through COM      }
{    searches, download and installs Windows Updates to your machine 	         }
{ 	            						                                                   }
{ Preparations: JwaWindows, JwaVersions, VirtualStringTreeView                 }
{			                                                                         }
{ Article link:   							                                               }
{                                                                              }
{                                                                 	           }
{ Version history: 16/06/2008 first release        			                       }
{ 									                                                           }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 	       }
{ productive environments.						                                         }
{ The code has surely some errors that need to be fixed. In such a case	       }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.						                                         }
{ 									                                                           }
{ Be aware that third party components used by this example may have other     }
{ licenses. Please see the corresponding component what they are.              }
{ You have to download them manually.                                          }
{ 									                                                           }
{ The JEDI API Logo is copyrighted and must not be used without permission     }
{ 									                                                           }
{ External components:                                                         }
{   VirtualStringTreeView components:                                          }
{    http://www.soft-gems.net/supplement/download.php?ID=28                    }
{ 	                                                                           }
{******************************************************************************}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, ComCtrls, Consts, CommCtrl, XPMan, ImgList,
  Math, {CompareValue}
  ActiveX,
  ComObj,
  TypInfo,
  WUApiLib_TLB;

{ vst structure }

type
  TRwuaData = record
   wuaUpdateTitel : WideString;
   wuaUpdateDescription : WideString;
  end;
   PRwuaData = ^TRwuaData;

  TOPwuaData = class
  public
    wuaUpdateTitel : WideString;
    wuaUpdateSeverity : WideString;
    wuaUpdateDescription : WideString;
    wuaUpdateUninstallSteps : WideString;
    wuaUpdateReleaseNotes : Widestring;
    wuaUninstallNotes : Widestring;
    wuaIsBetaDownload : WideString;
    wuaSupportUrl : WideString;
    wuaKB : WideString;
    wuaLanguage : WideString;
    wuaEula : WideString;
    wuaIsUninstallable : WideString;
    wuaRecommendedCPUSpeed : WideString;
    wuaRecommendedHardDiskSpace : WideString;
    wuaRecommendedMemory : WideString;
  end;

  TOwuaGroup = class(TOPwuaData)
  private
  public
  end;

{ MarqueeProgressbar which does not flicker on Vista or XP see Borland for the known issue
  http://qc.borland.com/wc/qcmain.aspx?d=38178 }

type
  TMarqueeProgress = class(TProgressbar)
  public
    cs: longint;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetMarquee(ssProgressbar: TProgressbar; ssStart : Boolean; ssSpeed : Integer);
   end;

{ Callbacks for Windows Update Api }

type
  TUpdSearchCallBack = class(TInterfacedObject, ISearchCompletedCallback)
  public
     function Invoke(const searchJob: ISearchJob; const callbackArgs: ISearchCompletedCallbackArgs): HResult; stdcall;
  end;


  TDownloadProgressChangedCallback = class(TInterfacedObject, IDownloadProgressChangedCallback)
  public

    function Invoke(const downloadJob: IDownloadJob;
                    const callbackArgs: IDownloadProgressChangedCallbackArgs): HResult; stdcall;
  end;



  TDownloadCompletedCallback = class(TInterfacedObject, IDownloadCompletedCallback)
  public
    function Invoke(const downloadJob: IDownloadJob;
                    const callbackArgs: IDownloadCompletedCallbackArgs): HResult; stdcall;
  end;

type
  TwuaMain = class(TForm)
    lbSummary: TLabel;
    Bevel1: TBevel;
    lbStatus: TLabel;
    lbAvailAupdates: TLabel;
    lbStart: TLabel;
    lbDuration: TLabel;
    lbEnd: TLabel;
    pbMarquee: TProgressBar;
    pbCurrent: TProgressBar;
    pbOverall: TProgressBar;
    Label2: TLabel;
    Label3: TLabel;
    XPManifest1: TXPManifest;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    vst: TVirtualStringTree;
    btnSearch: TButton;
    btnDownload: TButton;
    btnInstall: TButton;
    imgJedi: TImage;
    lbSeverity: TLabel;
    cbSeverity: TComboBox;
    btnStop: TButton;
    btnExit: TButton;
    ImageList1: TImageList;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure vstInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure vstBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure vstMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure vstPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure btnSearchClick(Sender: TObject);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbSeverityChange(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
   Starttime, EndTime, Duration : TDateTime;
  public
{ WUA }
  SrcResult : ISearchResult;
  aSearchJob : ISearchJob;
  UpdSearcher : IUpdateSearcher;
  UpdSession : IUpdateSession;
  UpdSrcJob : ISearchJob;
  UpdToInstall : IUpdateCollection;
  Upd : IUpdate;
  UpdStatus : OleVariant;

  SrcCompleted : ISearchCompletedCallback;
  UpdIsApplicable : Boolean;
  UpdDownloader : IUpdateDownloader;
  UpdDownResult : IDownloadResult;
  UpdProgress   : IDownloadProgress;
  UpdDownloadJob: IDownLoadJob;
  UpdDownloadState : OleVariant;

  IUpdateInstall : IUpdateInstaller;
  IInstallRes : IInstallationResult;
  InstBehaviour : IInstallationBehavior;

  State : OleVariant;
  res : HRESULT;

  SearchCompletedCallback : ISearchCompletedCallback;

  aDownloadProgressChangedCallback : IDownloadProgressChangedCallback;
  aDownloadCompletedCallback : IDownloadCompletedCallback;

  MyPtr : Variant;

  PrgMarquee : TMarqueeProgress;
    procedure FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
  end;

var
  wuaMain: TwuaMain;

const
  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = wm_user +10;

implementation

{$R *.dfm}

{
Converts normal Imagelist to a 32Bit Imagelist which supports alphablending, needed
for XP Icons, otherwise they will look ugly...
http://discuss.joelonsoftware.com/default.asp?joel.3.8105.4
btw. I would suggest using PNG Images they look always nice, for this look for
a PNG Imagelist...
}

procedure ConvertTo32BitImageList(const ImageList: TImageList); const
  Mask: array[Boolean] of Longint = (0, ILC_MASK); var
  TempList: TImageList;
begin
  if Assigned(ImageList) then
  begin
    TempList := TImageList.Create(nil);
    try
      TempList.Assign(ImageList);
      with ImageList do
      begin
        Handle := ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked], 0, AllocBy);
        if not HandleAllocated then
          raise EInvalidOperation.Create(SInvalidImageList);
      end;
      Imagelist.AddImages(TempList);
    finally
      FreeAndNil(TempList);
    end;
  end;
end;

procedure TwuaMain.FormCreate(Sender: TObject);
begin
{
 set the nodedatasize if you forget this, you run into an exception,
 because the tree does not know anything about your nodes...
}
  vst.NodeDataSize := sizeof(TOPwuaData);
{ 32bit images looks nicer!!! }
  ConvertTo32BitImageList(ImageList1);
  ImageList1.AddIcon(Image1.Picture.Icon);
  ImageList1.AddIcon(Image2.Picture.Icon);
  ImageList1.AddIcon(Image3.Picture.Icon);
{ assign the imagelist to the tree }
  vst.Images := ImageList1;
end;

procedure TwuaMain.vstInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Column: TColumnIndex;
  i:Integer;
begin
{ we tell the tree which column is the one, which needs to be set to multiline }
  Node.Align := 20; // Alignment of expand/collapse button nearly at the top of the node.
  for i:= 0 to vst.Header.columns.count-1 do begin
    Column := i;
    case Column of
      0: // nicht multiline
      begin
        vst.NodeHeight[Node] := 40;
        Include(InitialStates, ivsHasChildren);
      end;
      1,3,7,8,10,11: 
      begin
        vst.NodeHeight[Node] := 120;
        Include(InitialStates, ivsMultiline);
      end;
    end;
  end;
end;

procedure TwuaMain.vstInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
{ allow childs to display }
  ChildCount := 1;
end;

procedure TwuaMain.vstBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
{ draw a nice grey for every second line }
  if Odd(Node.Index) then
  begin
    TargetCanvas.Brush.Color := RGB(250,250,250);
    TargetCanvas.FillRect(TargetCanvas.ClipRect);
  end;
end;

procedure TwuaMain.vstMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
{ we need to adjust the nodesheight here if its multiline node }
  if Sender.MultiLine[Node] = true then
  begin
    TargetCanvas.Font := Sender.Font;
    NodeHeight := Vst.ComputeNodeHeight(TargetCanvas, Node, 1)+ 21;
  end
   else
    NodeHeight := 21;
end;

procedure TwuaMain.vstPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
{ change fontstyle for the updatetitel to fsbold otherwise use normal fontstyle }
case Column of
  0: TargetCanvas.Font.Style := [fsBold];
  4:
   begin
{ kburl underline and blue }   	
     TargetCanvas.Font.Style := [fsUnderline];
     TargetCanvas.Font.Color := clBlue;
    end;
  1,2,3,5,6,7,8,9,10,11,12,13,14:
  begin
    TargetCanvas.Font.Style := [];
    TargetCanvas.Font.Color := clBlack;
    end;
end;
end;

procedure TwuaMain.btnSearchClick(Sender: TObject);
  var
    data: TOPwuaData;
    group: TOwuaGroup;
    node: PVirtualNode;
    I,
    W,
    UU,
    lg,
    KBID : Integer;
begin
{ create Marquee }
  PrgMarquee := TMarqueeProgress.Create(wuaMain);
  PrgMarquee.Parent := wuaMain;
  PrgMarquee.Left := 175;
  PrgMarquee.Top := 104;
  PrgMarquee.Width := 193;
  PrgMarquee.Anchors := [akleft, aktop];
  PrgMarquee.Visible := True;
  PrgMarquee.SetMarquee(PRGMarquee, true, 40);

  btnSearch.Enabled := false;
  btnStop.Enabled := true;
{ creating CoClasses for WUA }
  UpdSession := CoUpdateSession.Create;
  UpdSearcher := CoUpdateSearcher.Create;
  UpdToInstall := CoUpdateCollection.Create;

  UpdIsApplicable := false;
  StartTime := now;
  lbStatus.Caption := Format('Status: %s',['search in progress, please wait...']);
  lbStart.Caption := Format('Start: %s',[TimeToStr(StartTime)]);
{ creating the searchcallback and start async search, we search for software not installed
on the machine except driver... }
  SearchCompletedCallback := TUpdSearchCallBack.Create;
  aDownloadProgressChangedCallback := TDownloadProgressChangedCallback.Create;
  aDownloadCompletedCallback := TDownloadCompletedCallback.Create;

  aSearchJob := UpdSearcher.BeginSearch('IsInstalled=0  and Type=''Software''', SearchCompletedCallback, Variant(Integer(Self)));

  aSearchjob.AsyncState; // don't forget this otherwise you run into an ole exception !
{ update gui elements }
  while not aSearchJob.IsCompleted do
    Application.ProcessMessages;
    aSearchJob.CleanUp;

{ fill our tree... }

 For I := 0 To SrcResult.Updates.Count -1 do
 begin
  Upd := SrcResult.Updates.Item[I];

{ add titel as new group to our tree
  set checkbox support for this group and set them to checked ...}
  group := TOwuaGroup.Create;
  group.wuaUpdateTitel := Upd.Title;
  group.wuaUpdateSeverity := Upd.MsrcSeverity;
  node:=vst.AddChild(nil, group);
  vst.CheckType[node] := ctCheckBox;
  vst.CheckState[node] := csCheckedNormal;

{ create childs for the new group }
  data := TOPwuaData.Create;
  data.wuaUpdateDescription := Upd.Description;
  data.wuaIsBetaDownload := BoolToStr(Upd.IsBeta, true);
  data.wuaIsUninstallable := BoolToStr(Upd.IsUninstallable, true);
  data.wuaSupportUrl := Upd.SupportUrl;
  data.wuaUninstallNotes := Upd.UninstallationNotes;
  data.wuaUpdateReleaseNotes := Upd.ReleaseNotes;
  data.wuaEula := Upd.EulaText;
  data.wuaRecommendedCPUSpeed := IntToStr(upd.RecommendedCpuSpeed);
  data.wuaRecommendedHardDiskSpace := IntToStr(upd.RecommendedHardDiskSpace);
  data.wuaRecommendedMemory := IntToStr(upd.RecommendedMemory);

  for KBID := 0 to upd.KBArticleIDs.Count -1 do
  begin
    data.wuaKB := upd.KBArticleIDs.Item[KBID];
   end;

    for lg := 0 to upd.Languages.Count -1 do
    begin
      data.wuaLanguage := upd.Languages.Item[lg];
     end;

      for uu := 0 to upd.UninstallationSteps.Count -1 do
      begin
        data.wuaUpdateUninstallSteps := upd.UninstallationSteps.Item[uu];
       end;
        node := vst.AddChild(node, data);
       end;

  For w := 0 to SrcResult.Warnings.Count -1 do
  begin
     memo1.Lines.add(srcresult.Warnings.Item[w].Message);
    end;
end;

procedure TwuaMain.vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    data: TOPwuaData;
begin
{ we draw the nodes caption here }
  data := TOPwuaData(vst.GetNodeData(node)^);
  case Column of
    0: celltext := data.wuaUpdateTitel;
    1: celltext := data.wuaUpdateDescription;
    2: celltext := data.wuaUpdateSeverity;
    3: celltext := data.wuaIsBetaDownload;
    4: celltext := data.wuaSupportUrl;
    5: celltext := data.wuaKB;
    6: celltext := data.wuaLanguage;
    7: celltext := data.wuaEula;
    8: celltext := data.wuaUpdateReleaseNotes;
    9: celltext := data.wuaIsUninstallable;
   10: celltext := data.wuaUninstallNotes;
   11: celltext := data.wuaUpdateUninstallSteps;
   12: celltext := data.wuaRecommendedCPUSpeed;
   13: celltext := data.wuaRecommendedHardDiskSpace;
   14: celltext := data.wuaRecommendedMemory;
  end;
   //ShowMessage('unhandled column!');
end;

procedure TwuaMain.vstGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data2  : TOPwuaData;
begin
{ lets see if the msrvcseverity is critical, important or moderate
  draw the icon depending on the severity state in column 2... 
remember on vista msseverity seems never to be filled, no idea whats the reason for this yet  }
  case Kind of
    ikNormal, ikSelected:
      begin
        Data2 := TOPwuaData(vst.getnodedata(node)^);
        case Column of
          -1, // general case
          2:  // severity
           if Data2.wuaUpdateSeverity = 'Critical' then
            ImageIndex := 0
            else
              if Data2.wuaUpdateSeverity = 'Important' then
               ImageIndex := 1
               else
                if Data2.wuaUpdateSeverity = 'Moderate' then
                ImageIndex := 2
                else
                  ImageIndex := -1;
                end;
              end;
         { we don't use overlays here, but it could be adjusted easily }
         ikOverlay:
      begin
      end;
  end;
end;

procedure TwuaMain.vstCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var
    data1, data2: TOPwuaData;
begin
{ sorting severitys...
  you need to set toAutoSort = true in AutoOptions and in
  Header set SortColumn = 2 which is the MsrcSeverity Column in our tree
}
  data1 := TOPwuaData( vst.GetNodeData(node1) ^);
  data2 := TOPwuaData( vst.GetNodeData(node2) ^);
  case Column of
   2: result := CompareText(Data1.wuaUpdateSeverity, Data2.wuaUpdateSeverity);
   end;
end;

procedure TwuaMain.vstHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{ Headerclick for sorting, ascending / descending... 
needs to be adjusted! }
  if Button=mbLeft then
  begin
    if Column = vst.Header.SortColumn then
      vst.Header.SortDirection := TSortdirection(1-ord(vst.Header.SortDirection))
    else
      vst.Header.SortColumn:=Column;  
  end;
end;

{ Here we set the visibility of our nodes depending on whats selected in the filter combobox...
if nothing selected e.g. text is empty show all nodes otherwise search for the severity }

procedure TwuaMain.FilterCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  dat: TObject;
  I : Integer;
begin
  I := cbSeverity.ItemIndex;
  dat := TOPwuaData(vst.GetNodeData(node)^);
  if dat is TOPwuaData then
    vst.IsVisible[node]:=(cbSeverity.Items.Strings[i]='') or (pos(cbSeverity.Items.Strings[i], TOPwuaData(dat).wuaUpdateSeverity)>0);
end;

{ we iterate the tree, including our filter callback ... }

procedure TwuaMain.cbSeverityChange(Sender: TObject);
begin
  vst.IterateSubtree(nil, FilterCallback ,nil);
end;

{ setup for the marquee progressbar 
maybe its better to build a new component for this...}

procedure TMarqueeProgress.SetMarquee(ssProgressbar: TProgressbar; ssStart: Boolean; ssSpeed: Integer);
begin
  Case ssStart of
  true:
   begin
     cs := getwindowlong(ssProgressbar.Handle, gwl_style);
     setwindowlong(ssProgressbar.Handle, gwl_style, cs or PBS_MARQUEE);
     sendmessage(ssProgressbar.Handle, PBM_SETMARQUEE, 1, ssSpeed);
   end;
  false:
  begin
    cs := getwindowlong(ssProgressbar.Handle, gwl_style);
    setwindowlong(ssProgressbar.Handle, gwl_style, cs or PBS_MARQUEE);
    sendmessage(ssProgressbar.Handle, PBM_SETMARQUEE, 0, 0);
  end;
 end;
end;

{ this avoids flickering on vista, cauze vista sends a wm_erasebackground message itself }

procedure TMarqueeProgress.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  DefaultHandler(Message);
end;

{ TUpdSearchCallBack }

function TUpdSearchCallBack.Invoke(const searchJob: ISearchJob;
  const callbackArgs: ISearchCompletedCallbackArgs): HResult;
  var
    wuaMain : TwuaMain;
begin
 wuaMain := TwuaMain(Pointer(Integer(Variant(SearchJob.AsyncState))));
 wuaMain.SrcResult := wuaMain.UpdSearcher.EndSearch(SearchJob);
 wuaMain.PrgMarquee.SetMarquee(wuaMain.PRGMarquee, false, 0);

 wuaMain.btnSearch.Enabled := true;
 wuaMain.btnDownload.Enabled := true;
 wuaMain.btnInstall.Enabled := true;
 wuaMain.cbSeverity.Enabled := true;
 wuaMain.btnStop.Enabled := false;
 wuaMain.EndTime := now;
 wuaMain.Duration := wuaMain.StartTime - wuaMain.EndTime;
 wuaMain.lbEnd.Caption := Format('End: %s',[TimeToStr(wuaMain.EndTime)]);
 wuaMain.lbDuration.Caption := Format('Duration: %s',[TimeToStr(wuaMain.Duration)]);
 wuaMain.lbStatus.Caption := Format('Status: %s',['search finished...']);
 result := S_OK;
end;

{ TDownloadProgressChangedCallback }

function TDownloadProgressChangedCallback.Invoke(
  const downloadJob: IDownloadJob;
  const callbackArgs: IDownloadProgressChangedCallbackArgs): HResult;
var ID : IDownloadProgress;
    wuaMain : TwuaMain;
begin
  wuaMain := TwuaMain(Pointer(Integer(Variant(downloadJob.AsyncState))));
{  wuaMain.UpdDownResult := wuaMain.UpdDownloader.EndDownload(downloadJob);
  ID := downloadJob.GetProgress;
  wuaMain.pbOverall.Max := ID.TotalBytesToDownload.Lo64;
  wuaMain.pbCurrent.Position := ID.TotalBytesDownloaded.Lo64;}
  wuaMain.pbOverall.Max := downloadJob.GetProgress.TotalBytesToDownload.Lo64;
  wuaMain.pbOverall.Position := downloadJob.GetProgress.TotalBytesDownloaded.Lo64;
  wuaMain.pbOverall.Hint := Format('%d of %d kib downloaded : %0.0f ',
    [wuaMain.pbOverall.Position div 1024, wuaMain.pbOverall.Max div 1024,
     wuaMain.pbOverall.Position / wuaMain.pbOverall.Max * 100
    ]);


  wuaMain.pbCurrent.Max := downloadJob.GetProgress.CurrentUpdateBytesToDownload.Lo64;
  wuaMain.pbCurrent.Position := downloadJob.GetProgress.CurrentUpdateBytesDownloaded.Lo64;
  wuaMain.pbCurrent.Hint := Format('%d of %d kib downloaded : %0.0f ',
    [wuaMain.pbOverall.Position div 1024, wuaMain.pbOverall.Max div 1024,
     wuaMain.pbOverall.Position / wuaMain.pbOverall.Max * 100
    ]);

  //if Abbrechen? then
  //  downloadJob.RequestAbort;
  result := S_OK;
end;

{ TDownloadCompletedCallback }

function TDownloadCompletedCallback.Invoke(const downloadJob: IDownloadJob;
  const callbackArgs: IDownloadCompletedCallbackArgs): HResult;
begin
  if wuaMain.UpdDownloader <> nil then
    wuaMain.UpdDownloader.EndDownload(DownloadJob);
  result := S_OK;
end;

procedure TwuaMain.btnStopClick(Sender: TObject);
begin
  if aSearchJob <> nil then
    aSearchJob.RequestAbort;
end;

{ otherwise you run into a memory leak, because if you use widestrings the tree will not free nodes automatically! }

procedure TwuaMain.vstFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: TOwuaGroup;
begin
  Data := TOwuaGroup(vst.GetNodeData(node)^);
  if not Assigned(Data) then
    exit
  else
    Data.Free;
end;
          
procedure TwuaMain.btnDownloadClick(Sender: TObject);
  var
    I : Integer;
begin
  if updSession <> nil then
    exit;
    
  UpdDownloader := updSession.CreateUpdateDownloader;
  UpdDownloader.ClientApplicationID := '{49F4962F-C39E-4B1D-A43B-B66743D42348}';
  UpdDownloader.IsForced := false;
  UpdDownloader.Priority := 2;


  For I := 0 To {SrcResult.Updates.Count -1}0 do
  begin
    upd := SrcResult.Updates.Item[I];
    updIsApplicable := True;
    updToInstall.Add(upd);
    Memo1.Lines.Add('Downloads: ' + IntToStr(I));

    try

      // downloads will be located in this folder --> C:\WINDOWS\SoftwareDistribution\Download
     // UpdDownResult := UpdDownloader.Download();

{      Case UpdDownResult.ResultCode of
        0 : Memo1.Lines.Add('Download not started...' + UpdDownloader.Updates.Item[I].Title);
        1 : Memo1.Lines.Add('Download in Progress...' + UpdDownloader.Updates.Item[I].Title);
        2 : Memo1.Lines.Add('Download succeded...' + UpdDownloader.Updates.Item[I].Title);
        3 : Memo1.Lines.Add('Download succeeded with errors...' + UpdDownloader.Updates.Item[I].Title);
        4 : Memo1.Lines.Add('Download failed...' + UpdDownloader.Updates.Item[I].Title);
        5 : Memo1.Lines.Add('Download aborded...' + UpdDownloader.Updates.Item[I].Title);
       end; }
        except
          on E : EOleSysError do
          begin
           // Memo1.Lines.Add('download discovered the following problem: ' + IntToStr(UpdDownResult.ResultCode));
       end;
  end;
  end;

  UpdDownloader.Updates := updToInstall;

  i := UpdDownloader.Updates.Count;


  UpdDownloadJob := UpdDownloader.BeginDownload(aDownloadProgressChangedCallback, aDownloadCompletedCallback, Variant(Integer(Self)));
 // UpdDownloadJob.AsyncState; // don't forget this otherwise you run into an ole exception !

{ update gui elements }
{  while not UpdDownloadJob.IsCompleted do
    Application.ProcessMessages;
    UpdDownloadJob.CleanUp;
 }
//  wuaMain.UpdDownResult := wuaMain.UpdDownloader.EndDownload(UpdDownloadJob);
end;

{ COM initialization / uninitialization }

procedure TwuaMain.btnExitClick(Sender: TObject);
begin
  close;
end;

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
