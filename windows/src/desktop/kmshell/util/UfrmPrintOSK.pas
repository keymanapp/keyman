(*
  Name:             UfrmOSKOnScreenKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 Jan 2011 - mcdurdin - I2329 - Print OSK helper window
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2675 - Print OSK dialog should be On Top and BringToFront as well
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmPrintOSK;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtShiftState, UserMessages,
  VisualKeyboard, Menus, ExtCtrls, keymanapi_TLB,
  StdCtrls, UfrmKeymanBase;

{$MESSAGE HINT 'TODO: Support printing OSK'}
type
  TfrmPrintOSK = class(TfrmKeymanBase)
    TntLabel1: TLabel;
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure TntFormDestroy(Sender: TObject);
    procedure webPrintTemplateTeardown(ASender: TObject;
      const pDisp: IDispatch);
  private
    TempFileName, TempPath: WideString;
    procedure DeleteWebPage;
    function SaveWebPage(vk: TVisualKeyboard; const s: string): Boolean;
    procedure WMUserPrintKeyboard(var Message: TMessage); message WM_USER_PrintKeyboard;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  public
    function PrintKeyboard(kbd: IKeymanKeyboardInstalled): Boolean;
  end;

implementation

uses
  KLog,
  kmint,
  MessageIdentifierConsts,
  messageidentifiers,
  Types,
  utildir,
  VisualKeyboardExportHTML,
  VisualKeyboardExportXML;

{$R *.dfm}

function TfrmPrintOSK.PrintKeyboard(kbd: IKeymanKeyboardInstalled): Boolean;
var
  buf: array[0..260] of char;
  vv: TVisualKeyboard;
begin
  Result := False;

  if not Assigned(kbd.VisualKeyboard) then Exit;

  vv := TVisualKeyboard.Create;
  try
    vv.LoadFromFile(kbd.VisualKeyboard.Filename);
    GetTempPath(260, buf);
    TempPath := IncludeTrailingPathDelimiter(buf);
    TempFileName := 'Keyman Keyboard '+kbd.ID+'.html';
    if not SaveWebPage(vv, TempPath + TempFileName) then Exit;
//    web.Navigate(TempPath + TempFileName);
  finally
    vv.Free;
  end;

  Result := ShowModal = mrOk;
end;

procedure TfrmPrintOSK.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  PostMessage(Handle, WM_USER_PrintKeyboard, 0, 0);
end;

procedure TfrmPrintOSK.webPrintTemplateTeardown(ASender: TObject;
  const pDisp: IDispatch);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TfrmPrintOSK.WMUserFormShown(var Message: TMessage);
begin
end;

procedure TfrmPrintOSK.WMUserPrintKeyboard(var Message: TMessage);
begin
  SetFocus;       // I2675
  BringToFront;   // I2675
//  web.Print;
end;

procedure TfrmPrintOSK.DeleteWebPage;
begin
  if (TempPath <> '') and (TempFileName <> '') and FileExists(TempFileName) then
  begin
    DeleteFile(TempPath + TempFileName);
    RecursiveDelete(ChangeFileExt(TempPath + TempFileName, ''));
  end;
end;

function TfrmPrintOSK.SaveWebPage(vk: TVisualKeyboard; const s: string): Boolean;
begin
  Screen.Cursor := crHourglass;
  try
    with TVisualKeyboardExportHTML.Create(vk) do
    try
      ExportToFile(s);
    finally
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  Result := True;
end;

procedure TfrmPrintOSK.TntFormDestroy(Sender: TObject);
begin
  inherited;
  DeleteWebPage;
end;

end.
