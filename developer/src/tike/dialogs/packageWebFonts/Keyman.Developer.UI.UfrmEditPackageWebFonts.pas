(*
  Name:             UfrmEditPackageWebFonts
  Copyright:        Copyright (C) SIL International.
  Date:             15 Aug 2023
  Authors:          mcdurdin
*)
unit Keyman.Developer.UI.UfrmEditPackageWebFonts;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.CheckLst,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,

  packageinfo,
  UfrmTike;

type
  TfrmEditPackageWebFonts = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    clbFonts: TCheckListBox;
    procedure cmdOKClick(Sender: TObject);
  private
    FSelectedFonts: TPackageContentFileReferenceList;
    FFiles: TPackageContentFileList;
    procedure SetFiles(const Value: TPackageContentFileList);
    procedure SetSelectedFonts(const Value: TPackageContentFileReferenceList);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property Files: TPackageContentFileList read FFiles write SetFiles;
    property SelectedFonts: TPackageContentFileReferenceList read FSelectedFonts write SetSelectedFonts;
  end;

implementation

uses
  System.Generics.Collections,

  Keyman.Developer.System.HelpTopics,
  utilfiletypes;

{$R *.dfm}

{ TfrmEditPackageWebFonts }

procedure TfrmEditPackageWebFonts.cmdOKClick(Sender: TObject);
var
  i: Integer;
begin
  FSelectedFonts.Clear;
  for i := 0 to clbFonts.Items.Count - 1 do
    if clbFonts.Checked[i] then
      FSelectedFonts.Add(clbFonts.Items.Objects[i] as TPackageContentFile);
  ModalResult := mrOk;
end;

function TfrmEditPackageWebFonts.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_EditPackageWebFonts;
end;

procedure TfrmEditPackageWebFonts.SetFiles(
  const Value: TPackageContentFileList);
var
  f: TPackageContentFile;
begin
  FFiles := Value;
  for f in FFiles do
  begin
    if GetFileTypeFromFileName(f.FileName) = ftFont then
      clbFonts.AddItem(ExtractFileName(f.FileName), f);
  end;
end;

procedure TfrmEditPackageWebFonts.SetSelectedFonts(
  const Value: TPackageContentFileReferenceList);
var
  n: Integer;
  f: TPackageContentFile;
begin
  FSelectedFonts := Value;
  for f in FSelectedFonts do
  begin
    n := clbFonts.Items.IndexOfObject(f);
    if n >= 0 then
      clbFonts.Checked[n] := True;
  end;
end;

end.
