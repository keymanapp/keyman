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
  Winapi.RichEdit,

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

const // ABGR - text on white bg, Colors similar to Light+ VSCode color theme
  Color_Filename = $96791A;
  Color_Line     = $197975;
  Color_Text     = $333333;
  Color_Code     = $666666;

  Color_Info     = $333333;
  Color_Hint     = $A25015;
  Color_Warn     = $156193;
  Color_Error    = $3434C9;
  Color_Fatal    = $3434C9;

  Color_SuccessText  = $1C8816;
  Color_InfoText     = $6B6B6B;
  Color_FailureText  = $3434C9;

  // Hard coded message numbers from infrastructureMessages.ts

  NAMESPACE_Infrastructure = $5000;
  INFO_FileBuiltSuccessfully = NAMESPACE_Infrastructure or $0006;
  INFO_FileNotBuiltSuccessfully = NAMESPACE_Infrastructure or $0007;
  INFO_ProjectBuiltSuccessfully = NAMESPACE_Infrastructure or $000B;
  INFO_ProjectNotBuiltSuccessfully = NAMESPACE_Infrastructure or $000C;


procedure TfrmMessages.Add(state: TProjectLogState; filename, msg: WideString; MsgCode, line: Integer);
type
  TSegment = record
    text: string;
    color: TColor;
  end;
var
  mi: TMessageItem;
  FColor: TColor;
  FTextColor: TColor;
  Segments: array of TSegment;

  procedure AddText(const text: string; color: TColor);
  var
    s: TSegment;
  begin
    s.text := text;
    s.color := color;
    SetLength(Segments, Length(Segments)+1);
    Segments[High(Segments)] := s;
  end;

  procedure AddLine;
  var
    t: string;
    s: TSegment;
    cr: TCharRange;
    cf: TCharFormat;
    gtle: TGetTextLengthEx;
  begin
    // TRichEdit's wrapper of the RichEdit control is very slow, so we instead
    // use direct messages to the RichEdit control to emit formatted text
    FillChar(cf, sizeof(TCharFormat), 0);
    cf.cbSize := sizeof(TCharFormat);
    cf.dwMask := CFM_COLOR;

    gtle.flags := GTL_PRECISE or GTL_NUMCHARS;
    gtle.codepage := 1200;
    cr.cpMin := SendMessage(memoMessage.Handle, EM_GETTEXTLENGTHEX, NativeUint(@gtle), 0);
    cr.cpMax := cr.cpMin;
    SendMessage(memoMessage.Handle, EM_EXSETSEL, 0, NativeUint(@cr));

    for s in Segments do
      t := t + s.text;
    memoMessage.SelText := t;

    for s in Segments do
    begin
      cr.cpMin := cr.cpMax;
      cr.cpMax := cr.cpMax + s.text.Length;
      SendMessage(memoMessage.Handle, EM_EXSETSEL, 0, NativeUint(@cr));
      cf.crTextColor := ColorToRGB(s.color);
      SendMessage(memoMessage.Handle, EM_SETCHARFORMAT, SCF_SELECTION, NativeUint(@cf));
    end;
    cr.cpMin := cr.cpMax;
    SendMessage(memoMessage.Handle, EM_EXSETSEL, 0, NativeUint(@cr));
  end;

var
  eventMask: DWord;
begin

  eventMask := SendMessage(memoMessage.Handle, EM_SETEVENTMASK, 0, 0);
  SendMessage(memoMessage.Handle, WM_SETREDRAW, 0, 0);
  try
    mi := TMessageItem.Create;
    mi.FileName := filename;
    mi.Msg := msg;
    mi.MsgCode := MsgCode;
    mi.Line := line;

    FColor := clBlack;
    FTextColor := Color_Text;

    // Override formatting for 4 known messages
    if (MsgCode = INFO_FileBuiltSuccessfully) or
       (MsgCode = INFO_ProjectBuiltSuccessfully) then
      state := plsSuccess
    else if (MsgCode = INFO_FileNotBuiltSuccessfully) or
       (MsgCode = INFO_ProjectNotBuiltSuccessfully) then
      state := plsFailure;

    case state of
      plsInfo: begin FColor := Color_Info; FTextColor := Color_InfoText; end;
      plsHint: FColor := Color_Hint;
      plsWarning: FColor := Color_Warn;
      plsError: FColor := Color_Error;
      plsFatal: FColor := Color_Error;
      plsSuccess: begin FColor := Color_Info; FTextColor := Color_SuccessText; end;
      plsFailure: begin FColor := Color_Info; FTextColor := Color_FailureText; end;
    end;

    // Add a log entry, following the color scheme that we use in kmc, matching
    // colors from VSCode's Light+ theme, which work reasonably well on white
    // background.

    AddText(ExtractFileName(Filename), Color_Filename);
    if line > 0 then
    begin
      AddText(':', Color_Text);
      AddText(IntToStr(Line), Color_Line);
    end;
    AddText(' - ', Color_Text);
    AddText(ProjectLogStateTitle[state], FColor);
    AddText(' KM'+IntToHex(MsgCode, 5), Color_Code);
    AddText(': ', Color_Text);
    AddText(StringReplace(msg, #13#10, '   ', [rfReplaceAll]), FTextColor);

    AddText(#13#10, clBlack);
    AddLine;
  finally
    SendMessage(memoMessage.Handle, WM_SETREDRAW, 1, 0);
    SendMessage(memoMessage.Handle, EM_SETEVENTMASK, 0, eventMask);
  end;

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


