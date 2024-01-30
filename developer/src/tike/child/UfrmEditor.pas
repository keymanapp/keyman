(*
  Name:             UfrmEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    27 Feb 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework for Keyman 7, part 1
                    02 Aug 2006 - mcdurdin - Rework menus as sp-TBX
                    23 Aug 2006 - mcdurdin - Polish print and print preview actions
                    23 Aug 2006 - mcdurdin - Rework menus
                    23 Aug 2006 - mcdurdin - Remove debugger for integration into wizard
                    14 Sep 2006 - mcdurdin - Rework to use TframeTextEditor
                    04 Dec 2006 - mcdurdin - Add active control and default ext
                    12 Dec 2006 - mcdurdin - Add Print Preview and Print
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - I1425 - Fixup bugs in font support
                    14 Jun 2008 - mcdurdin - I1422 - Fixup inability to insert characters in text editor
                    14 Jun 2008 - mcdurdin - I1421 - Fixup problems with text file format support
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    26 Jun 2012 - mcdurdin - I3381 - KM9 - Remove obsolete HeaderTemplate code
                    06 Feb 2012 - mcdurdin - I3082 - Reload text file with specific encoding support
                    03 Nov 2012 - mcdurdin - I3502 - V9.0 - Merge of I3082 - Reload text file with specific encoding support
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
*)
unit UfrmEditor;  // I3323

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, ToolWin, ComCtrls, ImgList, ErrorControlledRegistry,
  RegistryKeys, UfrmMDIChild, MenuImgList, Printers,
  UfrmKeyTest,

  Keyman.Developer.System.Project.ProjectFile, UfrmMDIEditor,
  CaptionPanel, Grids,
  CharMapInsertMode,
  TextFileFormat, KMDActionInterfaces,
  UframeTextEditor, UserMessages, System.ImageList;

type
  TUPCOptions = set of (upcSetError, upcClearError, upcSetBreakPoint, upcClearBreakPoint,
    upcSetExecutionPoint, upcClearExecutionPoint);

  TfrmEditor = class(TfrmTikeEditor, IKMDPrintActions {TODO:, IKMDPrintPreviewActions})
    lstImages: TMenuImgList;
    dlgFonts: TFontDialog;
    dlgPrint: TPrintDialog;
    dlgPrintSetup: TPrinterSetupDialog;
    dlgFind: TFindDialog;
    dlgReplace: TReplaceDialog;
    lstImagesDisabled: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FEditorFrame: TframeTextEditor;

    procedure SetEditorFormat(const Value: TEditorFormat);

    procedure WMUserSyntaxColourChange(var Message: TMessage); message WM_USER_SYNTAXCOLOURCHANGE;
    procedure SetTextFileFormat(const Value: TTextFileFormat);
    procedure UpdateEditorFormat;

    { IKMDPrintActions }
    function PrintFile: Boolean;
    { IKMDPrintPreviewActions }
    //TODO: function PrintPreview: Boolean;
    procedure EditorChanged(Sender: TObject);
    function GetEditorFormat: TEditorFormat;
    function GetTextFileFormat: TTextFileFormat;

  protected
    property EditorFrame: TframeTextEditor read FEditorFrame;

    function GetHelpTopic: string; override;
    function DoSaveFile: Boolean; override;
    function DoOpenFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

    procedure CodeFontChanged; override;
    procedure CharFontChanged; override;
  public
    procedure RefreshOptions; override;

    procedure SetFocus; override;

    procedure TextFileFormatClick; override;    // I1421 - text file format
    function CanTextFileFormatClick: Boolean; override; // I1421 - text file format

    function CanReloadAsTextFileFormatClick: Boolean; override;  // I3082   // I3502
    procedure ReloadAsTextFileFormatClick(TextFileFormat: TTextFileFormat);  // I3082   // I3502
      override;

    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081

    procedure SetEditorText(s: WideString);

    property EditorFormat: TEditorFormat read GetEditorFormat write SetEditorFormat;
    property TextFileFormat: TTextFileFormat read GetTextFileFormat write SetTextFileFormat;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  CharacterDragObject,
  CharMapDropTool,
  ClipBrd,
  dmActionsMain,
  dmActionsTextEditor,
  keymanstrings,
  Keyman.Developer.System.Project.kmnProjectFile,
  kmxfile,
  kwhelp,
  Keyman.Developer.System.Project.Project,
  KeymanDeveloperOptions,
  KMDevResourceStrings,
  KeymanDeveloperUtils,
  TextFileTemplates,
  UfrmBitmapEditor,
  UfrmKeymanWizard,
  UfrmMain,
  UfrmMessages,
  UfrmSelectSystemKeyboard,
  Unicode,
  utilstr;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 -------------------------------------------------------------------------------}

procedure TfrmEditor.FormActivate(Sender: TObject);
begin
  inherited;
  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(TextFileFormat);
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  FEditorFrame := TframeTextEditor.Create(Self);
  FEditorFrame.Parent := Self;

  FEditorFrame.Align := alClient;
  FEditorFrame.EditorFormat := efKMN;
  FEditorFrame.Visible := True;
  FEditorFrame.OnChanged := EditorChanged;
  FEditorFrame.TextFileFormat := tffUTF8;

//  TODO: GetCharMapDropTool.Handle(FEditorFrame.memo, cmimDefault);    // I1422 - insert character map chars

  ActiveControl := FEditorFrame;

  inherited;
end;

{-------------------------------------------------------------------------------
 - File loading/saving functionality                                           -
 -------------------------------------------------------------------------------}

procedure TfrmEditor.SetEditorFormat(const Value: TEditorFormat);
begin
  FEditorFrame.EditorFormat := Value;
end;

procedure TfrmEditor.SetEditorText(s: WideString);
begin
  FEditorFrame.SetTextBuf(PWideChar(s));
end;

procedure TfrmEditor.SetFocus;
begin
  inherited;
  FEditorFrame.SetFocus;
end;

procedure TfrmEditor.UpdateEditorFormat;
var
  s: string;
begin
  s := LowerCase(ExtractFileExt(FileName));
  if s = '.kmn' then EditorFormat := efKMN
  else if Pos('htm', s) > 0 then EditorFormat := efHTML
  else if Copy(s,1,4) = '.php' then EditorFormat := efHTML
  else if Pos('xml', s) > 0 then EditorFormat := efXML
  else if s = '.json' then EditorFormat := efJSON
  else if s = '.js' then EditorFormat := efJS
  else if s = '.css' then EditorFormat := efCSS
  else EditorFormat := efText;
end;

function TfrmEditor.CanReloadAsTextFileFormatClick: Boolean;  // I3082   // I3502
begin
  Result := True;
end;

function TfrmEditor.CanTextFileFormatClick: Boolean;    // I1421 - text file format
begin
  Result := True;
end;

procedure TfrmEditor.CharFontChanged;
begin
  inherited;
  FEditorFrame.CharFont := CharFont;
end;

procedure TfrmEditor.CodeFontChanged; // I1425 - cannot set fonts
begin
  inherited;
  FEditorFrame.CodeFont := CodeFont;
end;

function TfrmEditor.DoOpenFile: Boolean;
begin
  RefreshOptions;

  FEditorFrame.LoadFromFile(FileName);
  UpdateEditorFormat;

  if not FileExists(FileName) then
  begin
    case EditorFormat of    // I1423 - default template
      efXML: SetEditorText(GetXMLTemplate);
      efHTML: SetEditorText(GetHTMLTemplate);
    end;
  end;

  TextFileFormat := FEditorFrame.TextFileFormat; // I1421 - Update the text file format combo

  Result := True;
end;

procedure TfrmEditor.EditorChanged(Sender: TObject);
begin
  Modified := True;
end;

function TfrmEditor.DoSaveFile: Boolean;
begin
  FEditorFrame.SaveToFile(FileName);
  Result := True;
end;

{-------------------------------------------------------------------------------}

{TODO: function TfrmEditor.PrintPreview: Boolean;
begin
  Result := FEditorFrame.PrintPreview(FileName);
end;}

procedure TfrmEditor.RefreshOptions;
begin
  inherited;

end;

procedure TfrmEditor.ReloadAsTextFileFormatClick(TextFileFormat: TTextFileFormat);  // I3082   // I3502
var
  FStream: TFileStream;
begin
  if Modified then
  begin
    if MessageDlg('This action will cause the file to be reloaded from disk and you will lose all changes you have made to the file.  Continue and lose changes?',
        mtConfirmation, mbYesNoCancel, 0) <> mrYes then Exit;
  end;

  try
    FStream := TFileStream.Create(FileName, fmOpenRead);
  except
    on E:EFOpenError do
    begin
      ShowMessage('The file could not be reloaded: '+E.Message);
      Exit;
    end;
  end;
  try
    FEditorFrame.LoadFromStream(FStream, TextFileFormat);
  finally
    FStream.Free;
  end;

  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(TextFileFormat);

  Modified := False;
end;

function TfrmEditor.PrintFile: Boolean;
begin
  Result := FEditorFrame.PrintFile(FileName);
end;

{-------------------------------------------------------------------------------
 - Interface functions                                                         -
 -------------------------------------------------------------------------------}

function TfrmEditor.GetDefaultExt: string;
begin
  Result := 'kmn';
end;

function TfrmEditor.GetEditorFormat: TEditorFormat;
begin
  Result := FEditorFrame.EditorFormat;
end;

function TfrmEditor.GetFileNameFilter: string;
begin
  Result := 'Keyboard definitions (*.kmn)|*.kmn|HTML files (*.htm, *.html)|*.htm?|XML files (*.xml)|*.xml|Text files (*.txt)|*.txt|All files (*.*)|*.*';
end;

function TfrmEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Editor;
end;

function TfrmEditor.GetTextFileFormat: TTextFileFormat;
begin
  Result := FEditorFrame.TextFileFormat;
end;

procedure TfrmEditor.WMUserSyntaxColourChange(var Message: TMessage);
begin
  FEditorFrame.SyntaxColourChange;
end;

procedure TfrmEditor.SetTextFileFormat(const Value: TTextFileFormat);
begin
  frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(Value);
  FEditorFrame.TextFileFormat := Value;
  Modified := True;
end;

procedure TfrmEditor.TextFileFormatClick; // I1421 - text file format
begin
  if Ord(TextFileFormat) = frmKeymanDeveloper.cbTextFileFormat.ItemIndex then Exit;

  if (TextFileFormat <> tffANSI) and
      (frmKeymanDeveloper.cbTextFileFormat.ItemIndex = Ord(tffANSI)) then
    if MessageDlg('You may lose information if you transfer the file to ANSI.  Are you sure you want to do this?',
        mtConfirmation, mbYesNoCancel, 0) <> mrYes then
    begin
      frmKeymanDeveloper.cbTextFileFormat.ItemIndex := Ord(TextFileFormat);
      Exit;
    end;

  TextFileFormat := TTextFileFormat(frmKeymanDeveloper.cbTextFileFormat.ItemIndex);
end;

procedure TfrmEditor.FindError(const Filename: string; s: string; line: Integer);   // I4081
begin
end;

end.

