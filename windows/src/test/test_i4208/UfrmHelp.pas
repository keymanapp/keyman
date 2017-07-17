(*
  Name:             UfrmHelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    18 May 2012
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
*)
unit UfrmHelp;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer, OleCtrls, SHDocVw, EmbeddedWB,
   UfrmKeymanBase, SHDocVw_EWB, EwbCore,
  KeymanEmbeddedWB;

type
  THelpFormHelpTarget = (htNone, htProduct, htKeyboard, htTutorial);
  TfrmHelp = class(TfrmWebContainer)
  private
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
    procedure OpenKeyboardHelp;
    procedure OpenProductHelp;
    procedure OpenTutorial;
    property HelpTarget: THelpFormHelpTarget read FHelpTarget;
    property HelpJump: WideString read FHelpJump write FHelpJump;
  end;

implementation

{$R *.dfm}

{ TfrmHelp }

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
end;

procedure TfrmHelp.OpenTutorial;
begin
end;

procedure TfrmHelp.WMUserFormShown(var Message: TMessage);
var
  FXML: WideString;
begin
  Content_Render(False, FXML);
  inherited;
end;

procedure TfrmHelp.OpenKeyboardHelp;
begin
end;

end.
