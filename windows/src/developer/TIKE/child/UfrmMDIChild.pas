(*
  Name:             UfrmMDIChild
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
  History:          01 Aug 2006 - mcdurdin - Support tab child instead of mdi child
                    23 Aug 2006 - mcdurdin - Add TextFileFormat functionality
                    23 Aug 2006 - mcdurdin - Add ChangeTab function
                    04 Dec 2006 - mcdurdin - Add StandaloneProjectFile for files not in a project
                    30 May 2007 - mcdurdin - I671 - Fix crashes related to project changes
                    12 Oct 2007 - mcdurdin - I940 - Handle crash when closing Keyman Developer when child forms have already been destroyed
                    19 Nov 2007 - mcdurdin - Minor memory leak
                    24 Jan 2012 - mcdurdin - I3166 - Race condition focusing window causes crash in rare cases
                    06 Feb 2012 - mcdurdin - I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3505 - V9.0 - Merge of I3166 - Race condition focusing window causes crash in rare cases
                    30 Apr 2015 - mcdurdin - I4678 - V9.0 - Fixup Ctrl+PgUp, Ctrl+PgDn, Alt+Left, Alt+Right hotkeys
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    27 May 2015 - mcdurdin - I4720 - Compiling a standalone keyboard crashes Developer [CrashID:tike.exe_9.0.503.0_00A4316C_EAccessViolation]
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmMDIChild;   // I4796

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Keyman.Developer.System.Project.ProjectFile, Keyman.Developer.UI.Project.ProjectFileUI, CharacterMapSettings, Contnrs,
  CharMapInsertMode, UfrmTike, TextFileFormat;

type
  TCodeDesignView = (cdvDesign, cdvCode);   // I4678

  TfrmTikeChild = class(TTikeForm, IProjectFileFreeNotification)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FOnCloseFile: TNotifyEvent;
    FCharMapSettings: TCharMapSettings;
    FFormCreating: Boolean;
    procedure SetProjectFile(const Value: TProjectFile);   // I4687
    function GetProjectFileUI: TProjectFileUI;   // I4687

  protected

    FProjectFile: TProjectFile;
    FStandaloneProjectFile: TProjectFile;

    function GetProjectFile: TProjectFile; virtual;

    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;

    property FormCreating: Boolean read FFormCreating;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartClose; virtual;

    procedure TextFileFormatClick; virtual;
    function CanTextFileFormatClick: Boolean; virtual;

    procedure ReloadAsTextFileFormatClick(FileFormat: TTextFileFormat); virtual;  // I3082   // I3502
    function CanReloadAsTextFileFormatClick: Boolean; virtual;  // I3082   // I3502

    procedure RefreshOptions; virtual;

    function CanChangeTab(FForward: Boolean): Boolean; virtual;
    procedure ChangeTab(FForward: Boolean); virtual;

    function CanChangeView(FView: TCodeDesignView): Boolean; virtual;   // I4678
    procedure ChangeView(FView: TCodeDesignView); virtual;   // I4678

    procedure ProjectFileDestroying(ProjectFile: TProjectFile);

    //procedure CharmapInsertCode(const str: WideString; InsertType: TCharMapInsertMode); virtual;
    //function CanAcceptCharmapInsertion: Boolean; virtual;

    property OnCloseFile: TNotifyEvent read FOnCloseFile write FOnCloseFile;
    property ProjectFile: TProjectFile read GetProjectFile write SetProjectFile;
    property ProjectFileUI: TProjectFileUI read GetProjectFileUI;   // I4687

    property CharMapSettings: TCharMapSettings read FCharMapSettings;
  end;

  TChildWindowList = class(TObjectList)
  private
    function GetItem(Index: Integer): TfrmTikeChild;
    procedure SetItem(Index: Integer; const Value: TfrmTikeChild);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[Index: Integer]: TfrmTikeChild read GetItem write SetItem; default;
  end;

implementation

uses UfrmMain;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 -------------------------------------------------------------------------------}

procedure TfrmTikeChild.FormCreate(Sender: TObject);
begin
  FFormCreating := True;
  inherited;
  //Parent := frmKeymanDeveloper.panClientArea;
  frmKeymanDeveloper.ChildWindows.Add(Self); {Also sets parent}
  FCharMapSettings := TCharMapSettings.Create;
  Align := alClient;
  Visible := True;
  frmKeymanDeveloper.ActiveChild := Self;
  FFormCreating := False;
end;

procedure TfrmTikeChild.FormActivate(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    if ActiveControl.Enabled and ActiveControl.Visible and ActiveControl.CanFocus and ActiveControl.Showing then
    try
      ActiveControl.SetFocus;
    except
      on E:EInvalidOperation do ; // I3166 - Let's avoid the crash in rare race conditions where despite being enabled,visible,canfocus and showing it changes before the focus gets there...   // I3505
    end;
end;

procedure TfrmTikeChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Visible := False;
  Parent := nil;
  Action := caFree;
end;

procedure TfrmTikeChild.FormDestroy(Sender: TObject);
begin
  if Assigned(FStandaloneProjectFile) then
    FStandaloneProjectFile.RemoveFreeNotification(Self);
  if Assigned(FProjectFile) then
    FProjectFile.RemoveFreeNotification(Self);
  FStandaloneProjectFile := nil;
  FProjectFile := nil;

  Parent := nil;
  FreeAndNil(FCharMapSettings);
  if Assigned(FOnCloseFile) then FOnCloseFile(Self);
  if Assigned(frmKeymanDeveloper) and Assigned(frmKeymanDeveloper.ChildWindows) then
    // I940 - These may be destroyed already in a shutdown scenario 
    frmKeymanDeveloper.ChildWindows.Extract(Self);
end;

function TfrmTikeChild.GetProjectFile: TProjectFile;
begin
  if Assigned(FStandaloneProjectFile)
    then Result := FStandaloneProjectFile
    else Result := FProjectFile;
end;

function TfrmTikeChild.GetProjectFileUI: TProjectFileUI;   // I4687
begin
  Result := GetProjectFile.UI as TProjectFileUI;   // I4720
end;

{-------------------------------------------------------------------------------
 - Inheritable functions                                                       -
 -------------------------------------------------------------------------------}

{ File Menu }

function TfrmTikeChild.CanChangeTab(FForward: Boolean): Boolean;
begin
  Result := False;
end;

function TfrmTikeChild.CanChangeView(FView: TCodeDesignView): Boolean;   // I4678
begin
  Result := False;
end;

function TfrmTikeChild.CanReloadAsTextFileFormatClick: Boolean; // I3082   // I3502
begin
  Result := False;
end;

function TfrmTikeChild.CanTextFileFormatClick: Boolean;
begin
  Result := False;
end;

procedure TfrmTikeChild.ChangeTab(FForward: Boolean);
begin

end;

procedure TfrmTikeChild.ChangeView(FView: TCodeDesignView);   // I4678
begin

end;

{ Other Functions }

procedure TfrmTikeChild.RefreshOptions;
begin

end;


procedure TfrmTikeChild.ReloadAsTextFileFormatClick(  
  FileFormat: TTextFileFormat);  // I3082   // I3502
begin

end;

procedure TfrmTikeChild.SetProjectFile(const Value: TProjectFile);
begin
  if Assigned(FStandaloneProjectFile) then
    FStandaloneProjectFile.RemoveFreeNotification(Self);
  if Assigned(FProjectFile) then
    FProjectFile.RemoveFreeNotification(Self);

  FreeAndNil(FStandaloneProjectFile);

  FProjectFile := Value;
  if Assigned(FProjectFile) then
    FProjectFile.AddFreeNotification(Self);
end;

procedure TfrmTikeChild.StartClose;
begin
  // Called to close down chromium windows
end;

{-------------------------------------------------------------------------------
 - Special message overrides                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmTikeChild.TextFileFormatClick;
begin
end;

procedure TfrmTikeChild.CMTextchanged(var Message: TMessage);
begin
  frmKeymanDeveloper.UpdateChildCaption(Self);
end;

constructor TfrmTikeChild.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TfrmTikeChild.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FStandaloneProjectFile);
end;

procedure TfrmTikeChild.ProjectFileDestroying(ProjectFile: TProjectFile);
begin
  Self.ProjectFile := nil;
end;

{ TMDIWindowList }

function TChildWindowList.GetItem(Index: Integer): TfrmTikeChild;
begin
  Result := inherited GetItem(Index) as TfrmTikeChild;
end;

procedure TChildWindowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(frmKeymanDeveloper) then
    frmKeymanDeveloper.NotifyChildWindowChange(TfrmTikeChild(Ptr), Action);
end;

procedure TChildWindowList.SetItem(Index: Integer; const Value: TfrmTikeChild);
begin
  inherited SetItem(Index, Value);
end;

end.

