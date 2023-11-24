(*
  Name:             UfrmMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Rework messages as TTntMemo
                    23 Aug 2006 - mcdurdin - Rework menu as TSp-TbxPopupMenu
                    14 Sep 2006 - mcdurdin - Double click selects whole line for error
                    14 Sep 2006 - mcdurdin - View messages window when message added
                    28 Sep 2006 - mcdurdin - Fix multi-line messages
                    04 Jan 2007 - mcdurdin - Select whole line when moving to next message
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmMessages;  // I3306   // I4796

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.Menus,
  Winapi.Messages,
  Winapi.Windows,

  CaptionPanel,
  JvComponentBase,
  JvDockControlForm,

  Keyman.Developer.System.Project.ProjectLog,
  UfrmTikeDock,
  UfrmTike, Vcl.ComCtrls;

type
  TMessageItem = class
    FileName: string;
    Msg: string;
    MsgCode, Line: Integer;
  end;

  TfrmMessages = class(TTikeDockForm)
    dlgSave: TSaveDialog;
    memoMessage: TRichEdit;
    mnuPopup: TPopupMenu;
    cmdmClear: TMenuItem;
    cmdmSaveToFile: TMenuItem;
    mnuSeparator2: TMenuItem;
    cmdmPreviousMessage: TMenuItem;
    cmdmNextMessage: TMenuItem;
    mnuViewItem: TMenuItem;
    mnuSeparator1: TMenuItem;
    procedure memoMessageDblClick(Sender: TObject);
    procedure cmdmSaveToFileClick(Sender: TObject);
    procedure cmdmClearClick(Sender: TObject);
    procedure memoMessageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memoMessageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmdmNextMessageClick(Sender: TObject);
    procedure cmdmPreviousMessageClick(Sender: TObject);
    procedure mnuPopupPopup(Sender: TObject);
    procedure mnuViewItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMessageItems: TObjectList;
    function GetSelLine: Integer;
    procedure SetSelLine(Value: Integer);
  protected
    function GetHelpTopic: string; override;
    property SelLine: Integer read GetSelLine write SetSelLine;
  public
    procedure RefreshOptions;
    procedure Add(state: TProjectLogState; filename, msg: WideString; MsgCode, line: Integer);
    procedure Clear;
    procedure NextMessage;
    procedure PrevMessage;
    procedure FirstMessage;
    function HasPrevMessage: Boolean;
    function HasNextMessage: Boolean;
  end;

var
  frmMessages: TfrmMessages;

implementation

uses
  UfrmMain,
  UfrmMDIEditor,
  dmActionsMain,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUI;

{$R *.DFM}


{-------------------------------------------------------------------------------
 - Message functions                                                           -
 -------------------------------------------------------------------------------}

procedure TfrmMessages.Add(state: TProjectLogState; filename, msg: WideString; MsgCode, line: Integer);
var
  mi: TMessageItem;
  FColor: TColor;
begin
  mi := TMessageItem.Create;
  mi.FileName := filename;
  mi.Msg := msg;
  mi.MsgCode := MsgCode;
  mi.Line := line;

  case state of
    plsInfo: FColor := clBlack;
    plsWarning: FColor := clOlive;
    plsError: FColor := clRed;
    plsFatal: FColor := clRed;
    plsSuccess: FColor := clGreen;
    plsFailure: FColor := clRed;
  else
    FColor := clBlack;
  end;

  memoMessage.SelStart := Length(memoMessage.Text);
  memoMessage.SelLength := 0;
  memoMessage.SelAttributes.Color := FColor;
  memoMessage.Lines.Add(TProjectLog.FormatMessage(state, filename, msg, msgcode, line));
  memoMessage.SelAttributes.Color := clBlack;

  FMessageItems.Add(mi);
  if not memoMessage.Focused then SelLine := memoMessage.Lines.Count - 1;
  modActionsMain.actViewMessages.Update;
  if not modActionsMain.actViewMessages.Checked then
    modActionsMain.actViewMessages.Execute;
end;

procedure TfrmMessages.Clear;
begin
  FMessageItems.Clear;
  memoMessage.Clear;
end;

procedure TfrmMessages.memoMessageDblClick(Sender: TObject);
var
  frm: TForm;
  pf: TProjectFile;
  mi: TMessageItem;
  FFilename: string;
  line: Integer;
begin
  if not IsGlobalProjectUIReady then
    Exit;

  line := SelLine; if line < 0 then Exit;

  SelLine := line;

  mi := FMessageItems[line] as TMessageItem;
  FFilename := mi.FileName;

  if FFileName = FGlobalProject.FileName then
  begin
    frmKeymanDeveloper.ShowProject;
    Exit;
  end;

  frm := frmKeymanDeveloper.FindEditorByFileName(FFileName);
  if not Assigned(frm) then
  begin
    pf := FGlobalProject.FindFile(FFileName);
    if Assigned(pf) then (pf.UI as TProjectFileUI).DefaultEvent(Self)   // I4687
    else if FileExists(FFileName) then frmKeymanDeveloper.OpenFile(FFileName, False);
    frm := frmKeymanDeveloper.FindEditorByFileName(FFileName);
    if not Assigned(frm) then
    begin
      ShowMessage('Could not find source file editor.');
      Exit;
    end;
  end;

  if frm is TfrmTikeEditor then (frm as TfrmTikeEditor).FindError(mi.FileName, mi.Msg, mi.Line);   // I4081
end;

procedure TfrmMessages.memoMessageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    memoMessageDblClick(Sender)
  else if Key = VK_ESCAPE then
  begin
    if Assigned(frmKeymanDeveloper.ActiveChild) then frmKeymanDeveloper.ActiveChild.SetFocus;
  end
  else
    Exit;
  Key := 0;
end;

procedure TfrmMessages.cmdmSaveToFileClick(Sender: TObject);
begin
  if dlgSave.Execute then
    memoMessage.Lines.SaveToFile(dlgSave.FileName, TEncoding.UTF8);  // I3337
end;

procedure TfrmMessages.cmdmClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TfrmMessages.cmdmNextMessageClick(Sender: TObject);
begin
  NextMessage;
end;

procedure TfrmMessages.cmdmPreviousMessageClick(Sender: TObject);
begin
  PrevMessage;
end;

procedure TfrmMessages.NextMessage;
begin
  if SelLine < memoMessage.Lines.Count - 1 then
    SelLine := SelLine + 1;
  memoMessageClick(memoMessage);
  memoMessageDblClick(memoMessage);
end;

procedure TfrmMessages.PrevMessage;
begin
  if SelLine > 0 then
    SelLine := SelLine - 1;
  memoMessageClick(memoMessage);
  memoMessageDblClick(memoMessage);
end;

procedure TfrmMessages.FirstMessage;
begin
  if memoMessage.Lines.Count > 0 then
    SelLine := 0;
  memoMessageClick(memoMessage);
  memoMessageDblClick(memoMessage);
end;

procedure TfrmMessages.RefreshOptions;
begin

end;

function TfrmMessages.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Messages;
end;

function TfrmMessages.GetSelLine: Integer;
var
  i, x, n: Integer;
begin
  Result := -1; x := 0;
  n := memoMessage.SelStart;
  for I := 0 to memoMessage.Lines.Count - 1 do
  begin
    x := x + Length(memoMessage.Lines[i]) + 1;
    if x > n then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TfrmMessages.HasNextMessage: Boolean;
begin
  Result := SelLine < memoMessage.Lines.Count - 1;
end;

function TfrmMessages.HasPrevMessage: Boolean;
begin
  Result := SelLine > 0;
end;

procedure TfrmMessages.SetSelLine(Value: Integer);
var
  x, i: Integer;
begin
  x := 0;
  if Value >= memoMessage.Lines.Count then
  begin
    memoMessage.SelStart := Length(memoMessage.Text);
    Exit;
  end;
  for i := 0 to Value - 1 do
  begin
    x := x + Length(memoMessage.Lines[i]) + 1;
  end;
  memoMessage.SelStart := x;
  memoMessage.SelLength := Length(memoMessage.Lines[Value]);
end;

procedure TfrmMessages.mnuPopupPopup(Sender: TObject);
var
  e: Boolean;
begin
  e := memoMessage.Lines.Count > 0;
  cmdmNextMessage.Enabled := e and (SelLine < memoMessage.Lines.Count - 1);
  cmdmPreviousMessage.Enabled := e and (SelLine > 0);
  mnuViewItem.Enabled := e;
  cmdmClear.Enabled := e;
  cmdmSaveToFile.Enabled := e;
end;

procedure TfrmMessages.mnuViewItemClick(Sender: TObject);
begin
  memoMessageDblClick(memoMessage);
end;

procedure TfrmMessages.memoMessageClick(Sender: TObject);
begin
  ;
end;

procedure TfrmMessages.FormCreate(Sender: TObject);
begin
  inherited;
  FMessageItems := TObjectList.Create;
end;

procedure TfrmMessages.FormDestroy(Sender: TObject);
begin
  FMessageItems.Free;
end;

end.


