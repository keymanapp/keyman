(*
  Name:             UfrmMDIEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    6 Nov 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Support tab child instead of MDI child
                    23 Aug 2006 - mcdurdin - Refactor generic editor functionality
                    14 Sep 2006 - mcdurdin - Add DontCheckForReload property
                    04 Dec 2006 - mcdurdin - Add DefaultExt
                    04 Jan 2007 - mcdurdin - Add FileNameChanged function
                    22 Jan 2007 - mcdurdin - Save the project after setting changes
                    19 Mar 2007 - mcdurdin - I707 - Make FileNameChanged virtual
                    16 May 2007 - mcdurdin - I841 - If Save As fails, restore original filename
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    19 Nov 2007 - mcdurdin - I1154 - old file name kept
                    14 Jun 2008 - mcdurdin - I1221 - Improve performance of project
                    18 Mar 2011 - mcdurdin - I2733 - '&' in a .kmn or .kps title becomes an underscore in the Dev tab title
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    13 Dec 2012 - mcdurdin - I3680 - V9.0 - If user cancels the New File dialog for .kct, when trying to open non-existent .kct, then Developer crashes
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    30 Apr 2015 - mcdurdin - I4679 - V9.0 - Remember focus for active editor when swapping editors
                    06 Nov 2015 - mcdurdin - I4918 - Text editor is not refreshing correctly with new theme
*)
unit UfrmMDIEditor;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UfrmMDIChild, Keyman.Developer.System.Project.ProjectFile, UserMessages;

type
  TfrmTikeEditor = class(TfrmTikeChild)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FModified: Boolean;
    FFileName: WideString;
    FOpenFileAge: TDateTime;
    FCodeFont: TFont;
    FCharFont: TFont;
    FDontCheckForReload: Boolean;
    FLastActiveChild: TWinControl;   // I4679

    function GetUntitled: Boolean;
    //procedure SetFileName(const Value: String); virtual;
    //function GetFileName: String; virtual;

    procedure CharCodeFontChanged(Sender: TObject);

    procedure SetModified(const Value: Boolean);
    procedure SetFileName(Value: WideString);
    procedure SetCharFont(const Value: TFont);
    procedure SetCodeFont(const Value: TFont);
    function GetDontCheckForReload: Boolean;
    procedure SetDontCheckForReload(const Value: Boolean);

    procedure WMUserUpdateCaption(var Message: TMessage); message WM_USER_UpdateCaption;   // I4918

    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;   // I4679
  protected

    function ShouldRememberFocus(Control: TWinControl): Boolean; virtual;   // I4679

    procedure UpdateCaption;
    procedure PostUpdateCaption;   // I4918

    procedure LoadSettings; virtual;
    procedure SaveSettings(SaveProject: Boolean); virtual;

    procedure FileNameChanged; virtual;

    procedure CharFontChanged; virtual;
    procedure CodeFontChanged; virtual;

    function GetFileNameFilter: string; virtual; abstract;
    function GetDefaultExt: string; virtual; abstract;

    procedure ModifiedChanged; virtual;

    function DoSaveFile: Boolean; virtual; abstract;
    function DoOpenFile: Boolean; virtual; abstract;

    property DontCheckForReload: Boolean read GetDontCheckForReload write SetDontCheckForReload;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FindError(const Filename: string; s: string; line: Integer); virtual; abstract;   // I4081

    function HasSubfilename(const Filename: string): Boolean; virtual;

    procedure CheckForReload; virtual;   // I4021

    function SaveFile(AFileName: WideString; FKeepFileName: Boolean): Boolean;
    function OpenFile(AFileName: WideString): Boolean;

    function Revert: Boolean;

    procedure ControlKeyPressedAndReleased; virtual;

    procedure SetFocus; override;   // I4679

    property Untitled: Boolean read GetUntitled;
    property FileName: WideString read FFileName;
    property Modified: Boolean read FModified write SetModified;
    property FileNameFilter: string read GetFileNameFilter;
    property DefaultExt: string read GetDefaultExt;

    property CodeFont: TFont read FCodeFont write SetCodeFont;
    property CharFont: TFont read FCharFont write SetCharFont;
  end;

  TfrmTikeEditorClass = class of TfrmTikeEditor;

implementation

uses
  dmActionsMain,
  KeymanDeveloperUtils,
  KMDevResourceStrings,
  Keyman.Developer.System.Project.Project,
  UfrmMain,
  WideStrUtils;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Property functions                                                          -
 -------------------------------------------------------------------------------}

procedure TfrmTikeEditor.UpdateCaption;
var
  s: WideString;
begin
  if Untitled
    then s := '(untitled)'
    else s := WideReplaceStr(ExtractFileName(FileName), '&', '&&');  // I2733

  if Modified
    then Caption := s + ' *'
    else Caption := s;
end;

procedure TfrmTikeEditor.WMUserUpdateCaption(var Message: TMessage);   // I4918
begin
  UpdateCaption;
end;

procedure TfrmTikeEditor.CodeFontChanged;
begin
end;

procedure TfrmTikeEditor.ControlKeyPressedAndReleased;
begin
  // Do nothing
end;

constructor TfrmTikeEditor.Create(AOwner: TComponent);
begin
  FCharFont := TFont.Create;
  FCharFont.OnChange := CharCodeFontChanged;
  FCodeFont := TFont.Create;
  FCodeFont.OnChange := CharCodeFontChanged;
  inherited Create(AOwner);
end;

destructor TfrmTikeEditor.Destroy;
begin
  FreeAndNil(FCharFont);
  FreeAndNil(FCodeFont);
  //FreeAndNilFStandaloneProjectFile);
  inherited Destroy;
end;

procedure TfrmTikeEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  s: string;
begin
  inherited;
  CanClose := True;
  if Untitled then s := '(untitled)' else s := FileName;
  if Modified then
    case MessageDlg('The file '+s+' has changed.'+#13#10+'Do you wish to save changes?',
      mtConfirmation, mbYesNoCancel, 0) of
    mrYes:
      begin
        frmKeymanDeveloper.ActiveChild := Self;
        CanClose := modActionsMain.actFileSave.Execute;
      end;
    mrNo:
      CanClose := True;
    mrCancel:
      CanClose := False;
    end;
  SaveSettings(False);
  if Assigned(FGlobalProject) then
    FGlobalProject.Save;
end;

procedure TfrmTikeEditor.FormCreate(Sender: TObject);
begin
  inherited;
  UpdateCaption;
end;

function TfrmTikeEditor.GetDontCheckForReload: Boolean;
begin
  Result := FDontCheckForReload;
end;

function TfrmTikeEditor.GetUntitled: Boolean;
begin
  Result := FFileName = '';
end;

function TfrmTikeEditor.HasSubfilename(const Filename: string): Boolean;   // I4081
begin
  Result := False;
end;

procedure TfrmTikeEditor.ModifiedChanged;
begin
  PostUpdateCaption;   // I4918
end;

function TfrmTikeEditor.OpenFile(AFileName: WideString): Boolean;
begin
  SetFileName(AFileName);
  FileAge(FFileName, FOpenFileAge);
  Result := DoOpenFile;
  if not Result then   // I3680
    Exit;
  Modified := False;
  UpdateCaption;
  LoadSettings;
end;

procedure TfrmTikeEditor.PostUpdateCaption;   // I4918
begin
  PostMessage(Handle, WM_USER_UpdateCaption, 0, 0);
end;

function TfrmTikeEditor.Revert: Boolean;
begin
  Result := DoOpenFile;
  Modified := False;
end;

function TfrmTikeEditor.SaveFile(AFileName: WideString; FKeepFileName: Boolean): Boolean;
var
  FOldFileName: WideString;
begin
  FOldFileName := FFileName;
  SetFileName(AFileName);
  Result := DoSaveFile;
  if not Result then
  begin
    FFileName := FOldFileName;
    Exit;
  end;

  if not FKeepFileName then FFileName := FOldFileName
  else
  begin
    if Assigned(FProjectFile) then
    begin
      FProjectFile.FileName := AFileName;
    end
    else if Assigned(FStandaloneProjectFile) then
    begin
      FStandaloneProjectFile.FileName := AFileName;
    end;
    SaveSettings(False);
    Modified := False;
    UpdateCaption;
    FileAge(FFileName, FOpenFileAge);
  end;
end;

procedure TfrmTikeEditor.SetCharFont(const Value: TFont);
begin
  FCharFont.Assign(Value);
  if ProjectFile <> nil then  // I1381 - save font for keymanweb test window
    ProjectFile.IDEState['CharFont'] := FontAsString(CharFont);
  CharFontChanged;
end;

procedure TfrmTikeEditor.SetCodeFont(const Value: TFont);
begin
  FCodeFont.Assign(Value);
  if ProjectFile <> nil then  // I1381 - save font for keymanweb test window
    ProjectFile.IDEState['CodeFont'] := FontAsString(CodeFont);
  CodeFontChanged;
end;

procedure TfrmTikeEditor.SetDontCheckForReload(const Value: Boolean);
begin
  FDontCheckForReload := Value;
end;

procedure TfrmTikeEditor.SetFileName(Value: WideString);
var
  buf: array[0..260] of WideChar;
  p: PWideChar;
begin
  GetFullPathNameW(PWideChar(Value), 260, buf, p);
  FFileName := buf;

  FileNameChanged;
end;

procedure TfrmTikeEditor.SetFocus;   // I4679
begin
  inherited;
  if Assigned(FLastActiveChild) and FLastActiveChild.Visible and FLastActiveChild.CanFocus then
  try
    FLastActiveChild.SetFocus;
  except
    ;
  end;
end;

procedure TfrmTikeEditor.SetModified(const Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if Assigned(FProjectFile) then FProjectFile.Modified := Value
    else if Assigned(FStandaloneProjectFile) then FStandaloneProjectFile.Modified := Value;
         
    ModifiedChanged;
  end;
end;

procedure TfrmTikeEditor.CharCodeFontChanged(Sender: TObject);
begin
  if Sender = FCharFont
    then CharFontChanged
    else CodeFontChanged;
end;

procedure TfrmTikeEditor.FileNameChanged;
begin
  Hint := FileName;
end;

procedure TfrmTikeEditor.CharFontChanged;
begin

end;

procedure TfrmTikeEditor.CheckForReload;
var
  s: string;
  FFileAge: TDateTime;
begin
  if Untitled or DontCheckForReload then Exit;
  if FileExists(FFileName) then
  begin
    FileAge(FFileName, FFileAge);
    if FFileAge > FOpenFileAge then
    begin
      FileAge(FFileName, FOpenFileAge);
      if Modified then s := SMsgExternalEditModified else s := SMsgExternalEdit;
      if MessageDlg(Format(s, [FFileName]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
      begin
        DoOpenFile;
        Modified := False;
      end;
    end;
  end;
end;

procedure TfrmTikeEditor.CMFocusChanged(var Message: TCMFocusChanged);   // I4679
begin
  if Assigned(Message.Sender) and IsChild(Handle, Message.Sender.Handle) and ShouldRememberFocus(Message.Sender) then
    FLastActiveChild := Message.Sender;
end;

function TfrmTikeEditor.ShouldRememberFocus(Control: TWinControl): Boolean;   // I4679
begin
  Result := True;
end;

procedure TfrmTikeEditor.LoadSettings;
begin
  if ProjectFile <> nil then
  begin
    SetFontFromString(CharFont, ProjectFile.IDEState['CharFont']);
    SetFontFromString(CodeFont, ProjectFile.IDEState['CodeFont']);
  end;
end;

procedure TfrmTikeEditor.SaveSettings(SaveProject: Boolean);
begin
  if ProjectFile <> nil then
  begin
    ProjectFile.IDEState['CodeFont'] := FontAsString(CodeFont);
    ProjectFile.IDEState['CharFont'] := FontAsString(CharFont);
    if SaveProject then FGlobalProject.Save;
  end;
end;

end.
