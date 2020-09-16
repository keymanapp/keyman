(*
  Name:             KMDActions
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    7 Feb 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add support for IKMDEditActions
                    25 Jan 2007 - mcdurdin - Add TKMDSearchFind, TKMDSearchReplace and TKMDSearchFindNext actions
                    30 May 2007 - mcdurdin - I781 - Unicode search and replace dialogs
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    20 Jul 2008 - mcdurdin - I1503 - Ctrl+F not working in charmap
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    07 Feb 2014 - mcdurdin - I4032 - V9.0 - Add support for Redo to Keyman Developer actions
*)
unit KMDActions;  // I3306  // I3323

interface

uses
  ActnList,
  Actions,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  StrUtils,
  WideStrUtils,
  KMDActionInterfaces;

function IsTextEditor(control: TObject): Boolean;
function GetTextEditorController(control: TObject): IKMDTextEditorActions;

type
  TKMDEditAction = class(TAction)
  private
    FControl: TWinControl;
    procedure SetControl(Value: TWinControl);
  protected
    function GetCustomEdit(Target: TObject): TCustomEdit; virtual;
    function GetInterface(Target: TObject): IKMDEditActions; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    property Control: TWinControl read FControl write SetControl;
  end;

  TKMDEditCut = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TKMDEditCopy = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TKMDEditPaste = class(TKMDEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TKMDEditSelectAll = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TKMDEditUndo = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TKMDEditRedo = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TKMDEditDelete = class(TKMDEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TCommonDialogClass = class of TCommonDialog;

  TKMDBeforeExecuteEvent = procedure(Sender: TObject; var Cancel: Boolean) of object;

  TKMDSearchAction = class(TCustomAction)
  private
    FOnAccept: TNotifyEvent;
    FBeforeExecute: TKMDBeforeExecuteEvent;
    FOnCancel: TNotifyEvent;
    FExecuteResult: Boolean;
    procedure DoAccept;
    procedure DoCancel;
    procedure SetupDialog;
  protected
    FDialog: TCommonDialog;
    FControl: TWinControl;
    FFindFirst: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetDialogClass: TCommonDialogClass; virtual;
    procedure ExecuteTargetInterface(Target: IKMDSearchActions); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure Search(Sender: TObject); virtual;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
    property ExecuteResult: Boolean read FExecuteResult;
    property BeforeExecute: TKMDBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

  TKMDSearchFind = class(TKMDSearchAction)
  private
    function GetFindDialog: TFindDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
    procedure ExecuteTargetInterface(Target: IKMDSearchActions); override;
  published
    property Caption;
    property Dialog: TFindDialog read GetFindDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;

    property OnHint;
    property OnUpdate;
  end;

  TKMDSearchReplace = class(TKMDSearchAction)
  private
    function GetReplaceDialog: TReplaceDialog;
    procedure Replace(Sender: TObject);
  protected
    function GetDialogClass: TCommonDialogClass; override;
    procedure ExecuteTargetInterface(Target: IKMDSearchActions); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption;
    property Dialog: TReplaceDialog read GetReplaceDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
    property OnUpdate;
  end;

  TKMDSearchFindNext = class(TCustomAction)
  private
    FSearchFind: TKMDSearchFind;
  protected
    procedure ExecuteTargetInterface(Target: IKMDSearchActions);
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property SearchFind: TKMDSearchFind read FSearchFind write FSearchFind;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
    property OnUpdate;
  end;

procedure Register;

implementation

uses
  Clipbrd,
  Consts,
  Forms,
  SysUtils,
  Windows;

{ TframeTextEditor interactions }

function IsTextEditor(control: TObject): Boolean;
begin
  Result :=
    Assigned(control) and
    (control is TWinControl);
  if not Result then
    Exit;

  if control.ClassName <> 'TframeCEFHost' then
    control := TWinControl(control).Owner;

  Result := Assigned(control) and
    (control.ClassName = 'TframeCEFHost') and
    Assigned(TControl(control).Owner) and
    Supports(TControl(control).Owner, IKMDTextEditorActions);
end;

function GetTextEditorController(control: TObject): IKMDTextEditorActions;
begin
  if not IsTextEditor(control) then
    Exit(nil);

  if control.ClassName <> 'TframeCEFHost' then
    control := TWinControl(control).Owner;

  Result := TControl(control).Owner as IKMDTextEditorActions;
end;

function GetEditController(control: TObject): IKMDEditActions;
begin
  if not IsTextEditor(control) then
    Exit(nil);

  if control.ClassName <> 'TframeCEFHost' then
    control := TWinControl(control).Owner;

  Result := TControl(control).Owner as IKMDEditActions;
end;

{ TKMDEditAction }

destructor TKMDEditAction.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TKMDEditAction.GetCustomEdit(Target: TObject): TCustomEdit;
begin
  { We could hard cast Target as a TWinControl since HandlesTarget "should" be
    called before ExecuteTarget and UpdateTarget, however, we're being safe. }
  Result := Target as TCustomEdit;
end;

function TKMDEditAction.GetInterface(Target: TObject): IKMDEditActions;
begin
  if IsTextEditor(Target) then
    Result := GetEditController(Target)
  else
  begin
    Result := nil;
    while Assigned(Target) and not Supports(TWinControl(Target), IKMDEditActions, Result) do
    begin
      Target := TWinControl(Target).Parent;
    end;
    if not Assigned(Result) then
      raise Exception.Create('Unable to get interface for edit actions');
  end;
end;

function TKMDEditAction.HandlesTarget(Target: TObject): Boolean;
begin
  if (Control <> nil) and (Target = Control) then
    Result := True
  else if IsTextEditor(Target) then
    Result := True
  else if (Control = nil) and (Target is TWinControl) and TWinControl(Target).HandleAllocated and
    ((GetFocus = TWinControl(Target).Handle) or
    IsChild(TWinControl(Target).Handle, GetFocus)) then
  begin
    if (Target is TCustomEdit) then Result := True
    else
    begin
      while Target <> nil do
      begin
        if Supports(TWinControl(Target), IKMDEditActions) then
          Exit(True);
        Target := TWinControl(Target).Parent;
      end;
      Result := False;
    end;
  end
  else
    Result := False;
end;

procedure TKMDEditAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TKMDEditAction.SetControl(Value: TWinControl);
begin
  if Value <> FControl then
  begin
    if FControl <> nil then FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

{ TKMDEditCut }

procedure TKMDEditCut.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).CutToClipboard
  else GetInterface(Target).CutToClipboard;
end;

procedure TKMDEditCut.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := (GetCustomEdit(Target).SelLength > 0) and not GetCustomEdit(Target).ReadOnly
  else Enabled := GetInterface(Target).CanCut;
end;

{ TKMDEditCopy }

procedure TKMDEditCopy.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).CopyToClipboard
  else GetInterface(Target).CopyToClipboard;
end;

procedure TKMDEditCopy.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := GetCustomEdit(Target).SelLength > 0
  else Enabled := GetInterface(Target).CanCopy;
end;

{ TKMDEditPaste }

procedure TKMDEditPaste.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).PasteFromClipboard
  else GetInterface(Target).PasteFromClipboard;
end;

procedure TKMDEditPaste.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := Clipboard.HasFormat(CF_TEXT) and not GetCustomEdit(Target).ReadOnly
  else Enabled := GetInterface(Target).CanPaste;
end;

{ TKMDEditSelectAll }

procedure TKMDEditSelectAll.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).SelectAll
  else GetInterface(Target).SelectAll;
end;

procedure TKMDEditSelectAll.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := Length(GetCustomEdit(Target).Text) > 0
  else Enabled := GetInterface(Target).CanSelectAll;
end;

{ TKMDEditUndo }

procedure TKMDEditUndo.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).Undo
  else GetInterface(Target).Undo;
end;

procedure TKMDEditUndo.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := GetCustomEdit(Target).CanUndo
  else Enabled := GetInterface(Target).CanUndo;
end;

{ TKMDEditRedo }

procedure TKMDEditRedo.ExecuteTarget(Target: TObject);
begin
  GetInterface(Target).Redo;   // I4032
end;

procedure TKMDEditRedo.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := False   // I4032
  else Enabled := GetInterface(Target).CanRedo;   // I4032
end;

{ TKMDEditDelete }

procedure TKMDEditDelete.ExecuteTarget(Target: TObject);
begin
  if Target is TCustomEdit then GetCustomEdit(Target).ClearSelection
  else GetInterface(Target).ClearSelection;
end;

procedure TKMDEditDelete.UpdateTarget(Target: TObject);
begin
  if Target is TCustomEdit then Enabled := (GetCustomEdit(Target).SelLength <> 0) and not GetCustomEdit(Target).ReadOnly
  else Enabled := GetInterface(Target).CanClearSelection;
end;

procedure Register;
begin
  RegisterActions('KeymanEdit', [TKMDEditCut, TKMDEditCopy, TKMDEditPaste, TKMDEditSelectAll, TKMDEditUndo, TKMDEditRedo, TKMDEditDelete, TKMDSearchFind, TKMDSearchFindNext, TKMDSearchReplace], nil);
end;

{ TKMDSearchAction }

function WideSearchEdit(EditControl: TCustomEdit; SearchString: string;
  Options: TFindOptions; FindFirst: Boolean = False): Boolean;
var
  Buffer, P: PWideChar;
  Size: Word;
  SearchOptions: TStringSearchOptions;
  s: string;
  AnsiBuffer: PChar;
  AP: PChar;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := EditControl.GetTextLen;
  if (Size = 0) then Exit;
  Buffer := WStrAlloc(Size + 1);
  try
    SearchOptions := [];
    if frDown in Options then
      Include(SearchOptions, soDown);
    if frMatchCase in Options then
      Include(SearchOptions, soMatchCase);
    if frWholeWord in Options then
      Include(SearchOptions, soWholeWord);
    if EditControl is TCustomEdit then
    begin
      s := (EditControl as TCustomEdit).Text;
      P := SearchBuf(PChar(s), Length(s), EditControl.SelStart, EditControl.SelLength, SearchString, SearchOptions);
      if P <> nil then
      begin
        EditControl.SelStart := P - PWideChar(s);
        EditControl.SelLength := Length(SearchString);
        Result := True;
      end;
    end
    else
    begin
      AnsiBuffer := StrAlloc(Size + 1);
      try
        EditControl.GetTextBuf(AnsiBuffer, Size + 1);
        if FindFirst then
          AP := SearchBuf(AnsiBuffer, Size, 0, EditControl.SelLength,
                 SearchString, SearchOptions)
        else
          AP := SearchBuf(AnsiBuffer, Size, EditControl.SelStart, EditControl.SelLength,
                 SearchString, SearchOptions);
        if AP <> nil then
        begin
          EditControl.SelStart := AP - AnsiBuffer;
          EditControl.SelLength := Length(SearchString);
          Result := True;
        end;
      finally
        StrDispose(AnsiBuffer);
      end;
    end;
  finally
    WStrDispose(Buffer);
  end;
end;


constructor TKMDSearchAction.Create(AOwner: TComponent);
begin
  inherited;
  SetupDialog;
end;

destructor TKMDSearchAction.Destroy;
begin
  FDialog.RemoveFreeNotification(Self);

  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);

  inherited;
end;

procedure TKMDSearchAction.DoAccept;
begin
  if Assigned(FOnAccept) then FOnAccept(Self);
end;

procedure TKMDSearchAction.DoCancel;
begin
  if Assigned(FOnCancel) then FOnCancel(Self);
end;

procedure TKMDSearchAction.ExecuteTargetInterface(Target: IKMDSearchActions);
begin
  // No Action
end;

procedure TKMDSearchAction.ExecuteTarget(Target: TObject);
var
  FCancel: Boolean;
  x: IKMDSearchActions;
begin
  if Target is TWinControl then
  begin
    x := GetTextEditorController(Target as TWinControl);
    if Assigned(x) then
    begin
      Self.ExecuteTargetInterface(x);
      Exit;
    end;
  end;

  if Target is TCustomEdit then
  begin
    FControl := TWinControl(Target);
    if Assigned(FControl) then
      FControl.FreeNotification(Self);

    FExecuteResult := False;
    if Assigned(FDialog) then
    begin
      if Assigned(FBeforeExecute) then
      begin
        FCancel := False;
        FBeforeExecute(Self, FCancel);
        if FCancel then
          Exit;
      end;
      FExecuteResult := FDialog.Execute;
      if FExecuteResult then
        DoAccept
      else
        DoCancel;
    end;
  end
  else if Assigned(FBeforeExecute) then
  begin
    FCancel := False;
    FBeforeExecute(Self, FCancel);
    if FCancel then
      Exit;
  end;
end;

function TKMDSearchAction.GetDialogClass: TCommonDialogClass;
begin
  Result := nil;
end;

function TKMDSearchAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result :=
    IsTextEditor(Screen.ActiveControl) or
    (Screen.ActiveControl is TCustomEdit) or
    ((Screen.ActiveControl <> nil) and (Screen.ActiveControl.Owner.ClassName = 'TfrmCharacterMapNew'));
  if not Result then
    Result := inherited HandlesTarget(Target);
end;

procedure TKMDSearchAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
  if not (csDestroying in ComponentState) and (Operation = opRemove) and
     (AComponent = FDialog) then
    SetupDialog;
end;

procedure TKMDSearchAction.SetupDialog;
var
  DialogClass: TCommonDialogClass;
begin
  DialogClass := GetDialogClass;
  if Assigned(DialogClass) then
  begin
    FDialog := DialogClass.Create(Self);
    FDialog.Name := Copy(DialogClass.ClassName, 2, Length(DialogClass.ClassName));
    FDialog.SetSubComponent(True);
    FDialog.FreeNotification(Self);

    TFindDialog(FDialog).OnFind := Search;
    FFindFirst := False;
  end;
end;

procedure TKMDSearchAction.Search(Sender: TObject);
begin
  if Assigned(FControl) then
  begin
    if not WideSearchEdit(TCustomEdit(FControl), TFindDialog(FDialog).FindText,
       TFindDialog(FDialog).Options, FFindFirst) then
      ShowMessage(WideFormat(STextNotFound, [TFindDialog(FDialog).FindText]));
  end;
end;

procedure TKMDSearchAction.UpdateTarget(Target: TObject);
begin
  if IsTextEditor(Target) then
    Enabled := GetTextEditorController(Target).CanEditFind
  else if Target is TCustomEdit then
    Enabled := TCustomEdit(Target).GetTextLen > 0
  else
    inherited UpdateTarget(Target);
end;

{ TKMDSearchFind }

procedure TKMDSearchFind.ExecuteTargetInterface(Target: IKMDSearchActions);
begin
  Target.EditFind;
end;

function TKMDSearchFind.GetDialogClass: TCommonDialogClass;
begin
  Result := TFindDialog;
end;

function TKMDSearchFind.GetFindDialog: TFindDialog;
begin
  Result := TFindDialog(FDialog);
end;

{ TKMDSearchReplace }

procedure TKMDSearchReplace.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  TReplaceDialog(FDialog).OnReplace := Replace;
end;

procedure TKMDSearchReplace.ExecuteTargetInterface(Target: IKMDSearchActions);
begin
  Target.EditReplace;
end;

function TKMDSearchReplace.GetDialogClass: TCommonDialogClass;
begin
  Result := TReplaceDialog;
end;

function TKMDSearchReplace.GetReplaceDialog: TReplaceDialog;
begin
  Result := TReplaceDialog(FDialog);
end;

procedure TKMDSearchReplace.Replace(Sender: TObject);
var
  Found: Boolean;
  FoundCount: Integer;
begin
  // FControl gets set in ExecuteTarget
  Found := False;
  FoundCount := 0;
  if Assigned(FControl) then
    if FControl is TCustomEdit then
    begin
      with Sender as TReplaceDialog do
      begin
        if (Length(TCustomEdit(FControl).SelText) > 0) and
           (not (frReplaceAll in Dialog.Options) and
           (AnsiCompareText(TCustomEdit(FControl).SelText, FindText) = 0) or
           (frReplaceAll in Dialog.Options) and (TCustomEdit(FControl).SelText = FindText)) then
        begin
          TCustomEdit(FControl).SelText := ReplaceText;
          WideSearchEdit(TCustomEdit(FControl), Dialog.FindText, Dialog.Options, FFindFirst);
          if not (frReplaceAll in Dialog.Options) then exit;
        end;
        repeat
          Found := WideSearchEdit(TCustomEdit(FControl), Dialog.FindText, Dialog.Options, FFindFirst);
          if Found then
          begin
            TCustomEdit(FControl).SelText := ReplaceText;
            Inc(FoundCount);
          end;
        until not Found or not (frReplaceAll in Dialog.Options);
      end;
    end;

  if not Found and (FoundCount = 0) then
    ShowMessage(WideFormat(STextNotFound, [Dialog.FindText]));
end;

{ TKMDSearchFindNext }

constructor TKMDSearchFindNext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := False;
end;

procedure TKMDSearchFindNext.ExecuteTarget(Target: TObject);
begin
  if IsTextEditor(Target) then
    ExecuteTargetInterface(GetTextEditorController(Target))
  else
  begin
    if not Assigned(SearchFind) then Exit;
    SearchFind.FControl := TWinControl(Target);
    SearchFind.Search(Target);
  end;
end;

procedure TKMDSearchFindNext.ExecuteTargetInterface(Target: IKMDSearchActions);
begin
  Target.EditFindNext;
end;

function TKMDSearchFindNext.HandlesTarget(Target: TObject): Boolean;
begin
  Result :=
    IsTextEditor(Target) or
    (Assigned(FSearchFind) and FSearchFind.Enabled and
    (Length(FSearchFind.Dialog.FindText) <> 0));
  Enabled := Result;
end;

procedure TKMDSearchFindNext.UpdateTarget(Target: TObject);
begin
  if IsTextEditor(Target) then
    Enabled := GetTextEditorController(Target).CanEditFindNext
  else if Assigned(FSearchFind) then
    Enabled := FSearchFind.Enabled and (Length(FSearchFind.Dialog.FindText) <> 0)
  else
    Enabled := False;
end;

end.
