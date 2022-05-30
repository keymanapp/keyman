{
This demo moves or copies files (found by a filemask) from a source to
target directory. If the application cannot access the file, it takes
the ownership and rearranges the security to acess the file (only in move mode!).
The directory structure will be remained so the targets can be copied back.
If you copy them back, usually the files will get back their original
security descriptor, because they are not marked as protected. But it does not
mean that the owner of the file is changed. It will remain the group Administrators.
In this way, only Administrators can access the files. Previously it was
"Trusted Installer" only.

Use on your own risk.

Dual-License: MPL 1.1 or GPL 3 (see JWA/JWSCL headers)

Original author is: Christian Wimmer
This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/

Version 1.0
Creation date: 10.03.2009

}
unit UMainForm;

interface

uses
  JwaWindows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, 
  
  AdvFileSearch, //See readme.txt

  JwsclTypes,
  JwsclConstants,
  JwsclUtils,
  JwsclToken,
  JwsclSecureObjects,
  JwsclAcl,
  JwsclKnownSid,
  JwsclDescriptor,
  JwsclMapping,
  JwsclElevation,
  JwsclExceptions, StdCtrls, JvBaseDlg, JvBrowseFolder, ComCtrls, JvExControls,
  JvWaitingGradient;

type
  TFormMain = class(TForm)
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    EditSource: TEdit;
    ButtonBrowseSource: TButton;
    ButtonBrowseTarget: TButton;
    EditTarget: TEdit;
    MemoStatus: TMemo;
    ButtonRun: TButton;
    LabelStatus: TLabel;
    EditMask: TEdit;
    ProgressBar1: TProgressBar;
    LabelErrors: TLabel;
    CheckBoxCopy: TCheckBox;
    CheckBoxListOnly: TCheckBox;
    LabelFileCount: TLabel;
    Label1: TLabel;
    JvWaitingGradient1: TJvWaitingGradient;
    Label2: TLabel;
    Button1: TButton;
    procedure ButtonBrowseSourceClick(Sender: TObject);
    procedure ButtonBrowseTargetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure CheckBoxListOnlyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    fAdvFileSearch : TAdvFileSearch;
    fAbort,
    fIsRunning : Boolean;
    LengthSource : Integer;
    fFileCount,
    fErrors : Integer;
    fExtensions : TStringList;
  public
    procedure IncError;
    procedure IncFile;
    { Public-Deklarationen }
    procedure ItemFoundEvent(ItemName : TString; ItemType : TItemType; ItemsFound : Integer);
    procedure FinishedEvent(ItemsFound, msNeeded : Integer; Aborted : Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
begin
  MessageDlg('TakeControl' + #13#10 + 'Written by Christian Wimmer @ 2009' +
    #13#10 + 'Uses TAdvFileSearch, JEDI API&WSCL, JVCL and Eurekalog (sponsored)' + #13#10#13#10 +
    'Use it on your own risk!'+ #13#10#13#10 +
    'This application searches for files with a given filemask, gets full access to '+
    'the files (by setting the right file security) and copies or moves the file to a target folder'
    + #13#10 +
    'which has the same path as the source. In this way you can copy the files back.'
    + #13#10+'The source folder itself won''t be changed! Target files are overwritten.'+ #13#10+
    'The copied/moved files will not have the same security settings! The new security allows full access to your user.',  mtInformation, [mbOK], 0);

end;

procedure TFormMain.ButtonBrowseSourceClick(Sender: TObject);
begin
  JvBrowseForFolderDialog1.Directory := EditSource.Text;
  if JvBrowseForFolderDialog1.Execute then
    EditSource.Text := JvBrowseForFolderDialog1.Directory;
end;

procedure TFormMain.ButtonBrowseTargetClick(Sender: TObject);
begin
  JvBrowseForFolderDialog1.Directory := EditTarget.Text;
  if JvBrowseForFolderDialog1.Execute then
    EditTarget.Text := JvBrowseForFolderDialog1.Directory;
end;

procedure TFormMain.ButtonRunClick(Sender: TObject);
begin
  if fIsRunning then
  begin
    fAbort := true;
    fAdvFileSearch.AbortSearch;
    exit;
  end;

  if fExtensions.Count = 0 then
  begin
    MessageDlg('Enter at least one extension.',  mtError, [mbOK], 0);
    exit;
  end;

  EditSource.Text := Trim(IncludeTrailingBackslash(EditSource.Text));
  EditTarget.Text := Trim(IncludeTrailingBackslash(EditTarget.Text));

  if not DirectoryExists(EditSource.Text) or
     not DirectoryExists(EditTarget.Text) then
  begin
    MessageDlg('Source or Target folder does not exist,',  mtError, [mbOK], 0);
    exit;
  end;

  if Comparetext(EditSource.Text, EditTarget.Text) = 0 then
  begin
    MessageDlg('Source and Target must not be equal.',  mtError, [mbOK], 0);
    exit;
  end;

  if MessageDlg('Run now? DO NOT move DLL files! It may render your system useless.',  mtWarning, [mbYes, mbNo], 0) <> mrYes then
    exit;

  if not JwCheckAdministratorAccess and
     (not CheckBoxCopy.Checked or CheckBoxCopy.Enabled) then
  begin //someone is messing around
    CheckBoxCopy.Checked := true;
    CheckBoxCopy.Visible := false;
  end;

  fIsRunning := true;
  fAbort := false;
  fErrors := -1;
  IncError;
  fFileCount := -1;
  IncFile;

  ButtonRun.Caption := 'Abort';



  LengthSource := Length(EditSource.Text);


  MemoStatus.Clear;
  fAdvFileSearch.StartDir := EditSource.Text;
  fAdvFileSearch.IncludeSubDirs := true;
  fAdvFileSearch.OnItemFound := ItemFoundEvent;
  fAdvFileSearch.OnSearchFinished := FinishedEvent;

  MemoStatus.Lines.Add('Process started...');
  JvWaitingGradient1.Active := true;

  fAdvFileSearch.AdvanceSearch;
end;



procedure TFormMain.CheckBoxListOnlyClick(Sender: TObject);
begin
  //
  ButtonBrowseTarget.Enabled := not CheckBoxListOnly.Checked;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  JwInitWellKnownSIDs;

  if not JwCheckAdministratorAccess then
  begin
    MessageDlg('The move mode is deactivated because the application was not started as admin. Copy mode won''t change the source security to get access.',
      mtError, [mbOK], 0);
    CheckBoxCopy.Enabled := false;
    CheckBoxCopy.Checked := true;
  end;

  EditSource.Text := '';
  EditTarget.Text := '';

  fExtensions := TStringList.Create;
  fExtensions.Sorted := true;
  fExtensions.Delimiter := ';';
  fExtensions.DelimitedText := EditMask.Text;

  fAdvFileSearch := TAdvFileSearch.Create(self);
  fIsRunning := false;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
//  FreeAndNil(fAdvFileSearch);
  FreeAndNil(fExtensions);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Button1Click(Sender);
end;

function OnMoveFileEvent(TotalFileSize: LARGE_INTEGER;
    TotalBytesTransferred: LARGE_INTEGER;
    StreamSize: LARGE_INTEGER;
    StreamBytesTransferred: LARGE_INTEGER;
    dwStreamNumber: DWORD;
    dwCallbackReason: DWORD;
    hSourceFile: HANDLE;
    hDestinationFile: HANDLE;
    lpData: LPVOID): DWORD; stdcall;
var
  Self : TFormMain absolute lpData;
  Per : Integer;
begin

  try
    if Self.CheckBoxCopy.Checked then
      Self.LabelStatus.Caption := 'Copying file'
    else
      Self.LabelStatus.Caption := 'Moving file';

    if TotalBytesTransferred.QuadPart > 0 then
    begin
      Per := Round((TotalBytesTransferred.QuadPart / TotalFileSize.QuadPart) * 100) ;
      self.ProgressBar1.Position := Per;
    end;

    Application.ProcessMessages;
  except
  end;

  if Self.fAbort then
    result := PROGRESS_CANCEL
  else
    result := PROGRESS_CONTINUE;
end;


procedure TFormMain.IncError;
begin
  Inc(fErrors);
  LabelErrors.Caption := 'Errors: '+IntToStr(fErrors);
  Application.ProcessMessages;
end;

procedure TFormMain.IncFile;
begin
  Inc(fFileCount);
  LabelFileCount.Caption := 'Files: '+IntToStr(fFileCount);
  Application.ProcessMessages;
end;


procedure TFormMain.ItemFoundEvent(ItemName: TString; ItemType: TItemType;
  ItemsFound: Integer);

  function TakeControlOfFile : Boolean;
  var SD : TJwSecurityDescriptor;
  begin
    result := false;
    MemoStatus.Lines.Add('* Taking security control of file');

    try
      //first we try to check if we already have read and delete access
      //so we don't need to do anything
      SD := TJwSecureGeneralObject.GetNamedSecurityInfo(ItemNAme, SE_FILE_OBJECT, [siDaclSecurityInformation, siOwnerSecurityInformation, siGroupSecurityInformation]);

      if not TJwSecureGeneralObject.AccessCheck(SD, nil, JwaWindows.DELETE or FILE_READ_ACCESS or READ_CONTROL, TJwSecurityFileFolderMapping) then
      begin
        //In Copy mode, don't change SD of file!
        if CheckBoxCopy.Checked then
        begin
          MemoStatus.Lines.Add('[Error] Access to file denied in copy mode. This won''t happen in move mode.');
          result := false;
          exit;
        end;

        try
          TJwSecureFileObject.TakeOwnerShip(ItemName);
        except
          on E : EJwsclSecurityException do
          begin
            MemoStatus.Lines.Add('[Error] Could not take ownership.');
            IncError;
            exit;
          end;
        end;

        SD.Free;
        try
          SD := TJwSecureGeneralObject.GetNamedSecurityInfo(ItemNAme, SE_FILE_OBJECT, [siDaclSecurityInformation]);

          SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwAdministratorsSID, false));
          TJwSecureGeneralObject.SetNamedSecurityInfo(ItemName, SE_FILE_OBJECT, [siDaclSecurityInformation], SD);
        except
          MemoStatus.Lines.Add('[Error] Could not set file security.');
          IncError;
        end;
      end;
    finally
       SD.Free;
    end;

    result := true;
  end;

  procedure MoveFile;
  var
    Res : Boolean;
    TempS,
    TempDir,
    NewTarget : String;
    Cancel : BOOL;
  begin
    //remove the given source directory
    TempS := Copy(ItemName, LengthSource+1, Length(ItemName));

    if Self.CheckBoxCopy.Checked then
      MemoStatus.Lines.Add('* Copying file')
    else
      MemoStatus.Lines.Add('* Moving file');

    NewTarget := EditTarget.Text + TempS;

    TempDir := ExtractFileDir(NewTarget);
    if not ForceDirectories(TempDir) then
    begin
      MemoStatus.Lines.Add('[Error] Could not create target dir.');
      IncError;
    end
    else
    begin
      if not CheckBoxCopy.Checked then
        Res := JwaWindows.MoveFileWithProgress(
            PChar(ItemName),//__in      LPCTSTR lpExistingFileName,
            PChar(NewTarget),//__in_opt  LPCTSTR lpNewFileName,
            @OnMoveFileEvent,//__in_opt  LPPROGRESS_ROUTINE lpProgressRoutine,
            Self,//__in_opt  LPVOID lpData,
            MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH or MOVEFILE_COPY_ALLOWED//__in      DWORD dwFlags
          )
      else
      begin
        Cancel := false;
        Res := JwaWindows.CopyFileEx(
            PChar(ItemName),//__in      LPCTSTR lpExistingFileName,
            PChar(NewTarget),//__in_opt  LPCTSTR lpNewFileName,
            @OnMoveFileEvent,//__in_opt  LPPROGRESS_ROUTINE lpProgressRoutine,
            Self,//__in_opt  LPVOID lpData,
            Cancel,
            0//__in      DWORD dwFlags
          );
      end;
      if not Res then
      begin
        MemoStatus.Lines.Add('[Error] Moving/Copying file failed.');
        IncError;
      end;
    end;

  end;

var I : Integer;
begin
  if fExtensions.Find(ExtractFileExt(ItemName), I) then
  begin
    LabelStatus.Caption := Format('File: %s - found %d',[ItemName, ItemsFound]);
    MemoStatus.Lines.Add(Format('Processing: %s',[ItemName]));

    IncFile;
    if not CheckBoxListOnly.Checked then
    begin
      if TakeControlOfFile then
        MoveFile;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TFormMain.FinishedEvent(ItemsFound, msNeeded: Integer;
  Aborted: Boolean);
begin
  fIsRunning := false;
  ButtonRun.Caption := 'Run';
  MemoStatus.Lines.Add('***');
  MemoStatus.Lines.Add('Finished');
  JvWaitingGradient1.Active := false;
end;

end.
