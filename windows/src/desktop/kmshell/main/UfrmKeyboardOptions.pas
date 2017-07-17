(*
  Name:             UfrmKeyboardOptions
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      25 May 2010

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade EmbeddedWB (also I2393)
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit UfrmKeyboardOptions;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer, OleCtrls, SHDocVw, EmbeddedWB, keymanapi_TLB,
  UserMessages, SHDocVw_EWB, EwbCore, KeymanEmbeddedWB;

type
  TfrmKeyboardOptions = class(TfrmWebContainer)
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
  private
    kbd: IKeymanKeyboardInstalled;
    FKeyboardName: WideString;
    procedure SetKeyboardName(const Value: WideString);
    procedure WMUser_ContentRender(var Message: TMessage); message WM_USER_ContentRender;
    procedure DoNavigate;
  protected
    function ShouldProcessAllCommands: Boolean; override;
    procedure FireCommand(const command: WideString; params: TStringList); override;

  public
    { Public declarations }
    property KeyboardName: WideString write SetKeyboardName;
  end;

function ShowKeyboardOptions(AOwner: TWinControl; const AKeyboardName: WideString): Boolean;

implementation

uses
  EwbTools,
  kmint,
  utilexecute,
  WideStrings;

{$R *.dfm}

function ShowKeyboardOptions(AOwner: TWinControl; const AKeyboardName: WideString): Boolean;
begin
  with TfrmKeyboardOptions.Create(AOwner) do
  try
    KeyboardName := AKeyboardName;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

{ TfrmKeyboardOptions }

procedure TfrmKeyboardOptions.FireCommand(const command: WideString;
  params: TStringList);
var
  i: Integer;
begin
  if command = 'cancel' then
    ModalResult := mrCancel
  else if command = 'ok' then
  begin
    for i := 0 to kbd.Options.Count - 1 do
      kbd.Options[i].Value := '';

    for i := 1 to params.Count - 1 do
      kbd.Options[params.Names[i]].Value := params.ValueFromIndex[i];
    ModalResult := mrOk;
  end
  else
  begin
    TUtilExecute.URL(params[0]);  // I3349
  end;
end;

procedure TfrmKeyboardOptions.SetKeyboardName(const Value: WideString);
begin
  FKeyboardName := Value;
  DoNavigate;
end;

function TfrmKeyboardOptions.ShouldProcessAllCommands: Boolean;
begin
  Result := True;
end;

procedure TfrmKeyboardOptions.webBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  if Pos('options.htm', URL) > 0 then Exit
  else inherited;
end;

procedure TfrmKeyboardOptions.webNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
begin
  if Pos('options.htm', string(bstrURL)) > 0 then
  begin
    Cancel := True;
  end
  else inherited;
end;

procedure TfrmKeyboardOptions.DoNavigate;
var
  n: Integer;
  I: Integer;
  s: WideString;
  v: OleVariant;
begin
  n := kmcom.Keyboards.IndexOf(FKeyboardName);
  if n < 0 then Exit;
  kbd := kmcom.Keyboards[n] as IKeymanKeyboardInstalled;

  s := '';

  for I := 0 to kbd.Options.Count - 1 do
  begin
    if I > 0 then s := s + '&';
    s := s + EwbTools.Encode(kbd.Options[I].Name) + '=' + EwbTools.Encode(kbd.Options[I].Value);
  end;

  if s <> '' then s := '?' + s;

  s := ExtractFilePath(kbd.OwnerPackage.Filename) + 'options.htm' + s;

  web.OnBeforeNavigate2 := nil;
  v := navNoHistory or navNoReadFromCache or navNoWriteToCache;
  web.Navigate(s, v);
  web.OnBeforeNavigate2 := webBeforeNavigate2;
end;

procedure TfrmKeyboardOptions.WMUser_ContentRender(var Message: TMessage);
begin
  DoNavigate;
end;

end.
