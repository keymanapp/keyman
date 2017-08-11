(*
  Name:             UfrmHint
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1248 - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    28 May 2014 - mcdurdin - I4242 - Crash when OSK closed/reopened without dismissing hint window [CrashID:keyman.exe_9.0.449.0_2C405C5D_EInvalidPointer]
*)
unit UfrmHint;  // I3306   // I4242

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer, OleCtrls, SHDocVw, EmbeddedWB, HintConsts,
  SHDocVw_EWB, EwbCore, KeymanEmbeddedWB, UserMessages;

type
  TfrmHint = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormShow(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    FHint: TKeymanHint;
    FButtons: TMsgDlgButtons;
    FDefaultResult: TModalResult;

    class var FInstance: TfrmHint;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
    { Private declarations }
  public
    { Public declarations }
    property Hint: TKeymanHint read FHint write FHint;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property DefaultResult: TModalResult read FDefaultResult write FDefaultResult;

    class property Instance: TfrmHint read FInstance;
  end;

implementation

{$R *.dfm}

uses
  Hints, XMLRenderer, GenericXMLRenderer;

procedure TfrmHint.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'ok' then
  begin
    ModalResult := mrOk;
  end
  else if command = 'cancel' then
  begin
    ModalResult := mrCancel;
  end
  else if command = 'showhint' then
    EnableHint(FHint)
  else if command = 'dontshowhint' then
    DisableHint(FHint)
  else
    inherited;
end;

procedure TfrmHint.TntFormCreate(Sender: TObject);
begin
  FInstance := Self;
  inherited;
  Position := poScreenCenter;
  XMLRenderers.RenderTemplate := 'Hint.xsl';
end;

procedure TfrmHint.TntFormDestroy(Sender: TObject);
begin
  inherited;
  FInstance := nil;
end;

procedure TfrmHint.TntFormShow(Sender: TObject);
var
  FXML: WideString;
begin
  FXML :=
    '<Hint ID="'+GetHintName(FHint) + '" />' +
    '<Buttons>';

  if mbOK in FButtons then FXML := FXML + '<Button ID="OK" />';
  if mbCancel in FButtons then FXML := FXML + '<Button ID="Cancel" />';

  FXML := FXML + '</Buttons>';

  XMLRenderers.Add(TGenericXMLRenderer.Create(FXML));
  Content_Render;
  inherited;
end;

procedure TfrmHint.WMUserFormShown(var Message: TMessage);
begin
  FormStyle := fsStayOnTop;
end;

end.
