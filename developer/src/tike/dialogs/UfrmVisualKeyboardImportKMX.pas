(*
  Name:             UfrmVisualKeyboardImportKMX
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Mar 2007

  Modified Date:    27 Mar 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Mar 2007 - mcdurdin - I683 - Fixed import of kmx to on screen keyboard
                    30 May 2007 - mcdurdin - I726 - Fixed 102 key not displaying when it should
                    10 Sep 2008 - mcdurdin - I1471 - Add key caps to all non-matched keys (option)
                    10 Jan 2014 - mcdurdin - I4022 - V9.0 - Rework Import KMX to work with V9.0 Debug Host Keyboard
                    19 Mar 2014 - mcdurdin - I4143 - V9.0 - Support modifier layers of KVK when importing a KMX
                    27 Mar 2015 - mcdurdin - I4156 - V9.0 - Debug host keyboard needs to map through forced keyboard's preserved keys
*)
unit UfrmVisualKeyboardImportKMX;   // I4022

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.System.VisualKeyboardImportKMX,
  UfrmTike,
  VisualKeyboard;

type
  TfrmVisualKeyboardImportKMX = class(TTIKEForm)
    Label1: TLabel;
    lblStatus: TLabel;
  private
    importer: TVisualKeyboardImportKMX;
    FFileName: WideString;
    FShow102Key: Boolean;
    FLeftRightCtrlAlt: Boolean;
    FVK: TVisualKeyboard;

    procedure SetStatus(const msg: string);
  protected
    function GetHelpTopic: string; override;
    procedure FormShown; override;
  public
    property FileName: WideString read FFileName write FFileName;
    property VK: TVisualKeyboard read FVK write FVK;
    property LeftRightCtrlAlt: Boolean read FLeftRightCtrlAlt;
    property Show102Key: Boolean read FShow102Key;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.DFM}

procedure TfrmVisualKeyboardImportKMX.FormShown;
begin
  SetStatus('Loading keyboard data');

  try
    importer := TVisualKeyboardImportKMX.Create(FFileName, FVK);
    try
      SetStatus('Importing keys');
      importer.ImportKeys;

      // Read metadata for presentation of keyboard
      FShow102Key := importer.Show102Key;
      FLeftRightCtrlAlt := importer.LeftRightCtrlAlt;
    finally
      importer.Free;
    end;
  except
    on E:EVisualKeyboardImportKMX do
    begin
      ShowMessage(E.Message);
      ModalResult := mrCancel;
      Exit;
    end;
  end;

  ModalResult := mrOk;
end;

procedure TfrmVisualKeyboardImportKMX.SetStatus(const msg: string);
begin
  lblStatus.Caption := msg;
  lblStatus.Update;
end;

function TfrmVisualKeyboardImportKMX.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_VisualKeyboardImportKMX;
end;


end.

