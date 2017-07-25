(*
  Name:             UframeAttachedFiles
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Apr 2009

  Modified Date:    1 Jan 2013
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Apr 2009 - mcdurdin - I1941 - Collect minidump files, diagnostic log files and zip them with the diagnostic report
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    01 Jan 2013 - mcdurdin - I3672 - V9.0 - tsysinfo attached files seem to be missing
*)
unit UframeAttachedFiles;

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, si_base, Grids, Menus;

type
  TframeAttachedFiles = class(TForm)   // I3672
    gridFiles: TStringGrid;
    PopupMenu1: TPopupMenu;
    cmdOpen: TMenuItem;
    cmdOpenContainingFolder: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure cmdOpenClick(Sender: TObject);
    procedure cmdOpenContainingFolderClick(Sender: TObject);
    procedure gridFilesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    function SelectedDir: string;
    function SelectedFile: string;
    { Private declarations }
  public
    { Public declarations }
    procedure AddFiles(Files: TStrings);
  end;

implementation

uses
  ShellApi, shlobj, utilexecute, utilsystem, sysinfo_util;

{$R *.dfm}

{ TframeAttachedFiles }

procedure TframeAttachedFiles.AddFiles(Files: TStrings);
var
  i: Integer;
  f: TSearchRec;
  s: string;
begin
  gridFiles.Cells[0,0] := 'Filename';
  gridFiles.Cells[1,0] := 'Size';
  gridFiles.Cells[2,0] := 'File Type';
  gridFiles.Cells[3,0] := 'Path';
  gridFiles.RowCount := Files.Count + 1;
  gridFiles.ColWidths[0] := 150;
  gridFiles.ColWidths[1] := 75;
  gridFiles.ColWidths[2] := 75;
  gridFiles.ColWidths[3] := gridFiles.ClientWidth - 303;
  for i := 0 to Files.Count - 1 do
  begin
    gridFiles.Cells[0,i+1] := ExtractFileName(Files[i]);
    if FindFirst(Files[i], 0, f) = 0 then
    begin
      gridFiles.Cells[1,i+1] := FileSizeKB(f.Size);
      FindClose(f);
    end
    else
      gridFiles.Cells[1,i+1] := 'unknown';

    gridFiles.Cells[3,i+1] := ExtractFileDir(Files[i]);
    s := LowerCase(ExtractFileExt(Files[i]));
    if (s = '.log') or (s = '.errlog') then
      gridFiles.Cells[2, i+1] := 'Log file'
    else if (s = '.tmp') then
      gridFiles.Cells[2, i+1] := 'Crash dump file'
    else
      gridFiles.Cells[2, i+1] := 'Other file';
  end;
end;

function TframeAttachedFiles.SelectedFile: string;
begin
  Result := gridFiles.Cells[3,gridFiles.Row]+'\'+gridFiles.Cells[0,gridFiles.Row];
end;

function TframeAttachedFiles.SelectedDir: string;
begin
  Result := gridFiles.Cells[3,gridFiles.Row];
end;

procedure TframeAttachedFiles.cmdOpenClick(Sender: TObject);
begin
  if not TUtilExecute.Shell(Handle, SelectedFile, SelectedDir) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TframeAttachedFiles.cmdOpenContainingFolderClick(Sender: TObject);
begin
  OpenContainingFolder(SelectedFile);
end;

procedure TframeAttachedFiles.FormResize(Sender: TObject);
begin
  inherited;
  gridFiles.ColWidths[3] := gridFiles.ClientWidth - 303;
end;

procedure TframeAttachedFiles.gridFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
begin
  inherited;
  if Button = mbRight then
  begin
    gridFiles.MouseToCell(X, Y, ACol, ARow);
    if (ARow > 0) then
    begin
      gridFiles.Row := ARow;
      with gridFiles.ClientToScreen(Point(X,Y)) do
        PopupMenu1.Popup(X, Y);
    end;
  end;
end;

end.
