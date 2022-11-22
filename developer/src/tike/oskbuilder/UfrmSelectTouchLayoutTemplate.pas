(*
  Name:             UfrmSelectTouchLayoutTemplate
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      10 Jan 2014

  Modified Date:    10 Jan 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    
*)
unit UfrmSelectTouchLayoutTemplate;

interface

uses
  System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSelectTouchLayoutTemplate = class(TForm)
    cmdOk: TButton;
    lbTemplate: TListBox;
    cmdCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdOkClick(Sender: TObject);
  private
    FPromptChange: Boolean;   // I4021
    procedure EnableControls;
    function GetTemplateFilename: string;
  public
    property TemplateFilename: string read GetTemplateFilename;
    property PromptChange: Boolean read FPromptChange write FPromptChange;   // I4021
  end;

implementation

uses
  RedistFiles,
  TouchLayoutUtils,
  utilfiletypes;

{$R *.dfm}

procedure TfrmSelectTouchLayoutTemplate.cmdOkClick(Sender: TObject);
begin
  if FPromptChange and   // I4021
      (MessageDlg('Changing template may lose some layout data if keys are not '+
      'present on the new template.  Continue?', mtConfirmation, mbOkCancel, 0) = mrCancel) then
    Exit;
  ModalResult := mrOk;
end;

procedure TfrmSelectTouchLayoutTemplate.EnableControls;
begin
  cmdOK.Enabled := lbTemplate.ItemIndex >= 0;
end;

procedure TfrmSelectTouchLayoutTemplate.FormCreate(Sender: TObject);
var
  f: TSearchRec;
  Error, Root: string;
begin
  Root := GetLayoutBuilderPath;
  if FindFirst(Root + '*'+Ext_KeymanTouchLayout, 0, f) = 0 then
  begin
    repeat
      if IsValidTouchLayoutFile(Root + f.Name, Error) then
        lbTemplate.Items.Add(f.Name);
    until FindNext(f) <> 0;
    FindClose(f);
  end;

  if lbTemplate.Items.Count > 0 then
    lbTemplate.ItemIndex := 0;

  EnableControls;
end;

function TfrmSelectTouchLayoutTemplate.GetTemplateFilename: string;
begin
  Result := GetLayoutBuilderPath + lbTemplate.Items[lbTemplate.ItemIndex];
end;

end.
