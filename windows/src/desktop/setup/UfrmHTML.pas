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
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade E-mbeddedWB (also I2393)
                    11 Jan 2011 - mcdurdin - I2645 - Refactor install process - move license to bootstrapper
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit UfrmHTML;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, OleCtrls, SHDocVw;

type
  TfrmHTML = class(TForm)
    cmdOK: TButton;
    panHTML: TPanel;
    cmdPrint: TButton;
    cmdBack: TButton;
    cmdForward: TButton;
    web: TWebBrowser;
    procedure cmdPrintClick(Sender: TObject);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
    procedure cmdBackClick(Sender: TObject);
    procedure cmdForwardClick(Sender: TObject);
    procedure webCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure FormDestroy(Sender: TObject);
  private
    FFilename: string;
  public
    procedure ShowFile(const FileName: WideString);
    procedure ShowText(const Text: string);
  end;

implementation

uses
  utildir,
  utilexecute;

{$R *.DFM}

{ TfrmHTML }

procedure TfrmHTML.ShowFile(const FileName: WideString);
begin
  if FileExists(FileName) and Assigned(web) then
    web.Navigate(FileName);
end;

procedure TfrmHTML.ShowText(const Text: string);
var
  stream: TStringStream;
begin
  FFilename := KGetTempFileName('.txt');
  stream := TStringStream.Create(Text, TEncoding.UTF8);
  try
    stream.SaveToFile(FFilename);
  finally
    stream.Free;
  end;
  ShowFile(FFileName);
end;

procedure TfrmHTML.webCommandStateChange(ASender: TObject; Command: Integer;
  Enable: WordBool);
begin
  if Command = CSC_NAVIGATEFORWARD then
    cmdForward.Enabled := Enable
  else if Command = CSC_NAVIGATEBACK then
    cmdBack.Enabled := Enable;
end;

procedure TfrmHTML.webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
  var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
  bstrUrl: WideString);
begin
  Cancel := True;
  TUtilExecute.URL(bstrUrl);  // I3349
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
  web.ExecWB(OLECMDID_PRINT, 0);
end;

procedure TfrmHTML.FormDestroy(Sender: TObject);
begin
  if FFileName <> '' then
    System.SysUtils.DeleteFile(FFileName);
end;

end.

