(*
  Name:             UfrmMustIncludeDebug
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          
*)
unit UfrmMustIncludeDebug;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UfrmTike;

type
  TfrmMustIncludeDebug = class(TTIKEForm)
    Label1: TLabel;
    Label2: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    chkAutoRecompile: TCheckBox;
    procedure cmdOKClick(Sender: TObject);
  private
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeymanDeveloperOptions;

{$R *.DFM}

procedure TfrmMustIncludeDebug.cmdOKClick(Sender: TObject);
begin
  if FKeymanDeveloperOptions.DebuggerAutoRecompileWithDebugInfo <> chkAutoRecompile.Checked then
  begin
    FKeymanDeveloperOptions.DebuggerAutoRecompileWithDebugInfo := chkAutoRecompile.Checked;
    FKeymanDeveloperOptions.Write;
    ModalResult := mrOk;
  end
  else
    ModalResult := mrYes;
end;

function TfrmMustIncludeDebug.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_MustIncludeDebug;
end;

end.
