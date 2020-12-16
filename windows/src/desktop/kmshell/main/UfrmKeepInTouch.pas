(*
  Name:             UfrmKeepInTouch
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      15 Apr 2015

  Modified Date:    6 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          15 Apr 2015 - mcdurdin - I4658 - V9.0 - Add Keep in Touch screen
                    06 Jun 2015 - mcdurdin - I4740 - Don't show "keep in touch" for non-keyman products
*)
unit UfrmKeepInTouch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmWebContainer;

type
  TfrmKeepInTouch = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormShow(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Content_Render(const Query: string = ''); override;
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    { Public declarations }
  end;

procedure ShowKeepInTouchForm(Force: Boolean);

implementation

{$R *.dfm}

uses
  kmint,
  ErrorControlledRegistry,
  OnlineConstants,
  RegistryKeys,
  Upload_Settings,
  UtilCheckOnline,
  UtilExecute;

procedure ShowKeepInTouchForm(Force: Boolean);
begin
  if not Force then
    with TRegistryErrorControlled.Create do
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if ValueExists(SRegValue_KeepInTouchShown) and ReadBool(SRegValue_KeepInTouchShown) then
          Exit;
      end;
    finally
      Free;
    end;

  if not IsOnline then Exit;

  with TRegistryErrorControlled.Create do
  try
    if OpenKey(SRegKey_KeymanDesktop_CU, True) then
    begin
      WriteBool(SRegValue_KeepInTouchShown, True);
    end;
  finally
    Free;
  end;

  with TfrmKeepInTouch.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmKeepInTouch.Content_Render(const Query: string);
var
  FPath: string;
begin
  FPath := MakeKeymanUrl(URLPath_KeepInTouch) + '?embed=1';
  cef.Navigate(FPath);   // I4181
end;

procedure TfrmKeepInTouch.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'close' then
    Close
  else
    inherited;
end;

procedure TfrmKeepInTouch.TntFormCreate(Sender: TObject);
begin
  inherited;
  Position := poScreenCenter;
end;

procedure TfrmKeepInTouch.TntFormShow(Sender: TObject);
begin
  inherited;
  Do_Content_Render;
end;

end.
