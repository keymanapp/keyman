(*
  Name:             UfrmHTML
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    06 Oct 2006 - mcdurdin - Add DoShowWelcome for showing package welcome screens
                    04 Dec 2006 - mcdurdin - Localize, polish display
                    15 Jan 2007 - mcdurdin - Use font from locale.xml
                    23 Aug 2007 - mcdurdin - Refactor package welcome
                    23 Aug 2007 - mcdurdin - I919 - Add print button
                    27 Mar 2008 - mcdurdin - Use TfrmKeymanBase instead of TfrmKMShell
                    25 May 2010 - mcdurdin - I2393 - Forward, Back and new window functions in Keyboard Welcome
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade EmbeddedWB (also I2393)
                    11 Jan 2011 - mcdurdin - I2641 - Print button now prints, not page setup
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit UfrmHTML;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, OleCtrls, SHDocVw, EmbeddedWB,
  keymanapi_TLB, UfrmKeymanBase, SHDocVw_EWB, EwbCore,
  KeymanEmbeddedWB, Vcl.AppEvnts;

type
  TfrmHTML = class(TfrmKeymanBase)
    cmdOK: TButton;
    panHTML: TPanel;
    web: TKeymanEmbeddedWB;
    cmdPrint: TButton;
    cmdBack: TButton;
    cmdForward: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdPrintClick(Sender: TObject);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
    procedure cmdBackClick(Sender: TObject);
    procedure cmdForwardClick(Sender: TObject);
    procedure webCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
  private
    FFileName: string;
    procedure SetText(const Value: string);
    procedure SetURL(const Value: string);
  public
    procedure ShowFile(const FileName: WideString);
    property Text: string write SetText;
    property URL: string write SetURL;
  end;

procedure DoShowPackageWelcome(pkg: IKeymanPackage; ShowMessageIfNoWelcome: Boolean);
//procedure DoShowWelcome(const Title, FileName: WideString);

implementation

uses
  CustInterfaces,
  kmint,
  MessageIdentifiers,
  MessageIdentifierConsts,
  StockFileNames,
  utilexecute;

{$R *.DFM}

{ TfrmHTML }

procedure TfrmHTML.ShowFile(const FileName: WideString);
begin
  if FileExists(FileName) and Assigned(web) then
    web.Navigate(FileName);
end;

procedure TfrmHTML.webCommandStateChange(ASender: TObject; Command: Integer;
  Enable: WordBool);
begin
  if Command = CSC_NAVIGATEFORWARD then
    cmdForward.Enabled := Enable
  else if Command = CSC_NAVIGATEBACK then
    cmdBack.Enabled := Enable;
end;

procedure TfrmHTML.webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmHTML.webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
  var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
  bstrUrl: WideString);
begin
  Cancel := True;
  TUtilExecute.URL(bstrUrl);  // I3349
end;

procedure TfrmHTML.SetText(const Value: string);
var
  buf, buf1: array[0..260] of char;
begin
  GetTempPath(260, buf1);
  GetTempFileName(buf1, 'kmn', 0, buf);
  FFileName := buf;
  with TStringList.Create do
  try
    Text := Value;
    SaveToFile(FFileName);
  finally
    Free;
  end;

  ShowFile(FFileName);
end;

procedure TfrmHTML.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if (Msg.message = WM_SYSCOMMAND) and (Msg.wParam = SC_RESTORE) then
  begin
    // Handle the case where Win+M pressed, window never restores
    SendMessage(Application.Handle, WM_SHOWWINDOW, 1, 0);
    SendMessage(Handle, WM_SHOWWINDOW, 1, SW_PARENTOPENING);
    OpenIcon(Handle);
  end;
end;

procedure TfrmHTML.cmdBackClick(Sender: TObject);
begin
  web.GoBack;
end;

procedure TfrmHTML.cmdForwardClick(Sender: TObject);
begin
  web.GoForward;
end;

procedure TfrmHTML.cmdPrintClick(Sender: TObject);
begin
  web.Print;  // I2641
end;

procedure TfrmHTML.FormCreate(Sender: TObject);
var
  s: string;
begin
  inherited;
  s := MsgFromId(SK_UIFontName);
  if s <> '' then Font.Name := s;
  cmdOK.Caption := MsgFromId(SKButtonOK);
  cmdPrint.Caption := MsgFromId(SKButtonPrint);
end;

procedure TfrmHTML.FormDestroy(Sender: TObject);
begin
  if FFileName <> '' then DeleteFile(FFileName);
end;

procedure TfrmHTML.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmHTML.SetURL(const Value: string);
begin
  if Assigned(web) then web.Navigate(Value);
end;

procedure DoShowWelcome(const Title, FileName: WideString);
begin
  with TfrmHTML.Create(Screen.ActiveForm) do
  try
    Width := Screen.Width * 6 div 10;
    Height := Screen.Height * 6 div 10;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    Caption := Title;
    ShowFile(FileName);
    ShowModal;
  finally
    Free;
  end;
end;

procedure DoShowPackageWelcome(pkg: IKeymanPackage; ShowMessageIfNoWelcome: Boolean);
var
  FWelcomeFile: IKeymanPackageContentFile;
begin
  FWelcomeFile := pkg.WelcomeFile;
  if Assigned(FWelcomeFile) then
    DoShowWelcome(pkg.Name, FWelcomeFile.FullFilename)
  else if ShowMessageIfNoWelcome then
    ShowMessage(MsgFromIdFormat(SKPackageDoesNotIncludeWelcome, [pkg.Name]));
end;

end.

