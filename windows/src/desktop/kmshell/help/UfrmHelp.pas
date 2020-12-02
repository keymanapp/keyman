(*
  Name:             UfrmHelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1251 - Integrate keyboard help with Keyman help
                    14 Jun 2008 - mcdurdin - Go directly to help if the help template does not exist
                    28 Jul 2008 - mcdurdin - I1510 - Context help fix
                    25 Jan 2011 - mcdurdin - I2569 - Load keyboard help via kmshell
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 May 2014 - mcdurdin - I4209 - V9.0 - Help dialog appears below OSK and is inaccessible
*)
unit UfrmHelp;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer,
  keymanapi_TLB, UfrmKeymanBase, UserMessages;

type
  THelpFormHelpTarget = (htNone, htProduct, htKeyboard, htTutorial);
  TfrmHelp = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
  private
    FActiveKeyboard: IKeymanKeyboardInstalled;
    FHelpTarget: THelpFormHelpTarget;
    FHelpJump: WideString;
    procedure DoHelpTarget(Target: THelpFormHelpTarget);
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  protected
    procedure FireCommand(const command: WideString; params: TStringList);
      override;
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute(const Context, ActiveKeyboardID: string);
    procedure OpenKeyboardHelp;
    procedure OpenProductHelp;
    property ActiveKeyboard: IKeymanKeyboardInstalled read FActiveKeyboard write FActiveKeyboard;
    property HelpTarget: THelpFormHelpTarget read FHelpTarget;
    property HelpJump: WideString read FHelpJump write FHelpJump;
  end;

implementation

{$R *.dfm}

uses
  help,
  kmint,
  MessageIdentifierConsts,
  MessageIdentifiers,
  utildir,
  utilexecute,
  utilhttp;

{ TfrmHelp }

class procedure TfrmHelp.Execute(const Context, ActiveKeyboardID: string);
begin
  with TfrmHelp.Create(nil) do   // I1251 - Combine help with keyboard help
  try
    HelpJump := Context;
    if ActiveKeyboardID = ''
      then ActiveKeyboard := nil
      else ActiveKeyboard := kmint.kmcom.Keyboards.Items[ActiveKeyboardID];

    ShowModal;

    case HelpTarget of
      htNone: ;
      htProduct: OpenProductHelp;
      htKeyboard: OpenKeyboardHelp;
    end;
  finally
    Free;
  end;
end;

procedure TfrmHelp.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'openkeyboardhelp' then
    DoHelpTarget(htKeyboard)
  else if command = 'openproducthelp' then
    DoHelpTarget(htProduct)
  else if command = 'opentutorial' then
    DoHelpTarget(htTutorial)
  else if command = 'cancel' then
    DoHelpTarget(htNone) //Close
  else
    inherited;
end;

procedure TfrmHelp.DoHelpTarget(Target: THelpFormHelpTarget);
begin
  FHelpTarget := Target;
  Close;
end;

procedure TfrmHelp.OpenProductHelp;
begin
  if FHelpJump = ''
    then OpenHelp('index')
    else OpenHelp(FHelpJump);
end;

procedure TfrmHelp.TntFormCreate(Sender: TObject);
begin
  inherited;
  FRenderPage := 'help';
end;

procedure TfrmHelp.WMUserFormShown(var Message: TMessage);
var
  FQuery: string;
begin
  FormStyle := fsStayOnTop;   // I4209
  if FActiveKeyboard <> nil
    then FQuery := Format('?keyboard=%s', [UrlEncode(FActiveKeyboard.Name)])
    else FQuery := '';

  Content_Render(FQuery);
  inherited;
end;

procedure TfrmHelp.OpenKeyboardHelp;
var
  kbd: IKeymanKeyboardInstalled;
begin
  kbd := FActiveKeyboard;
  if not Assigned(kbd) then Exit;
  kmcom.Control.ShowKeyboardWelcome(kbd);
end;

end.
