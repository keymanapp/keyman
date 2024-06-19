(*
  Name:             UfrmNewFileDetails
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    10 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Put an initial path in the New File edit box
                    28 Sep 2006 - mcdurdin - Require a non-blank file name for the target
                    04 Dec 2006 - mcdurdin - Fix warning for overwriting existing files
                    12 Dec 2006 - mcdurdin - Use My Documents folder if project is not yet saved; position cursor at end of text box
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4798 - Split Path and Name boxes in New File dialogs
                    10 Aug 2015 - mcdurdin - I4852 - Create directories when creating new file
*)
unit UfrmNewFileDetails;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, utilfiletypes, UfrmTike;

type
  TfrmNewFileDetails = class(TTIKEForm)
    lblFileTypeLabel: TLabel;
    lblFileName: TLabel;
    lblFileType: TLabel;
    editFileName: TEdit;
    cmdOK: TButton;
    cmdBrowse: TButton;
    cmdCancel: TButton;
    dlgSave: TSaveDialog;
    Label1: TLabel;
    editFilePath: TEdit;
    procedure cmdOKClick(Sender: TObject);
    procedure editFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
  private
    FFileType: TKMFileType;
    FDefaultExt: string;
    FCanChangePath: Boolean;
    procedure SetFileType(const Value: TKMFileType);
    procedure EnableControls;
    function GetFileName: string;
    procedure SetBaseFileName(Value: string);
    procedure SetCanChangePath(const Value: Boolean);
  protected
    function GetHelpTopic: string; override;
  public
    property FileName: string read GetFileName;
    property FileType: TKMFileType read FFileType write SetFileType;
    property BaseFileName: string write SetBaseFileName;
    property CanChangePath: Boolean write SetCanChangePath;
  end;

implementation

uses
  KeymanDeveloperOptions,
  Keyman.Developer.System.HelpTopics;

{$R *.DFM}

procedure TfrmNewFileDetails.cmdOKClick(Sender: TObject);
var
  s: string;
begin
  s := FileName;
  if FileExists(s) then
  begin
    case MessageDlg('The file "'+s+'" already exists.  Do you want to keep it instead of creating a new file?', mtWarning, mbYesNoCancel, 0) of
      mrYes:    ;
      mrNo:     if MessageDlg('Are you sure you want to overwrite the existing file '+s+'?  If you continue, it will be deleted and a new file created in its place.', mtConfirmation, mbOkCancel, 0) = mrCancel then Exit else DeleteFile(s);
      mrCancel: Exit;
    end;
  end
  else
  begin
    try
      ForceDirectories(editFilePath.Text);   // I4852
      TFileStream.Create(s, fmCreate).Free;
    except
      on E:Exception do
      begin
        ShowMessage('Unable to create the file "'+s+'".  Please check that the path name is correct and that no illegal characters are '+
          'used in the filename.  The error returned was: '+E.Message);
        Exit;
      end;
    end;
    DeleteFile(s);
  end;

  ModalResult := mrOk;
end;

procedure TfrmNewFileDetails.SetBaseFileName(Value: string);
begin
  if ExtractFilePath(Value) = ''   // I4798
    then editFilePath.Text := ExcludeTrailingPathDelimiter(FKeymanDeveloperOptions.DefaultProjectPath)   // I4798
    else editFilePath.Text := ExtractFileDir(Value);   // I4798
  editFileName.Text := '';   // I4798
end;

procedure TfrmNewFileDetails.SetCanChangePath(const Value: Boolean);
begin
  FCanChangePath := Value;
  EnableControls;
end;

procedure TfrmNewFileDetails.SetFileType(const Value: TKMFileType);
begin
  FFileType := Value;
  lblFileType.Caption := FileTypeNames[FFileType];
  dlgSave.Filter := GetFileTypeFilter(FFileType, FDefaultExt) + '|All files (*.*)|*.*';
  dlgSave.DefaultExt := FDefaultExt;
end;

procedure TfrmNewFileDetails.editFileNameChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewFileDetails.EnableControls;
begin
  editFilePath.ReadOnly := not FCanChangePath;
  editFilePath.ParentColor := editFilePath.ReadOnly;
  if not editFilePath.ParentColor then editFilePath.Color := clWindow;

  cmdOK.Enabled := Trim(ChangeFileExt(ExtractFileName(editFileName.Text), '')) <> '';
end;

procedure TfrmNewFileDetails.FormCreate(Sender: TObject);
begin
  inherited;
  EnableControls;
end;

procedure TfrmNewFileDetails.cmdBrowseClick(Sender: TObject);
begin
  dlgSave.FileName := FileName;
  if dlgSave.Execute then
  begin
    editFilePath.Text := ExtractFileDir(dlgSave.FileName);   // I4798
    editFileName.Text := ExtractFileName(dlgSave.FileName);   // I4798
  end;
end;

function TfrmNewFileDetails.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(editFilePath.Text) + editFileName.Text;   // I4798
  if AnsiCompareText(ExtractFileExt(Result), FDefaultExt) <> 0 then
    Result := Result + FDefaultExt;
end;

function TfrmNewFileDetails.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewFileDetails;
end;

end.

