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
unit Keyman.Developer.UI.UfrmMessageDlgWithSave;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UfrmTike;

type
  TfrmMessageDlgWithSave = class(TTikeForm)
    lblMessage: TLabel;
    cmdYes: TButton;
    cmdNo: TButton;
    cmdCancel: TButton;
    chkSave: TCheckBox;
  private
    FHelpTopic: string;
  protected
    function GetHelpTopic: string; override;
  public
    // Always has Yes, No and Cancel
    class function Execute(const Message, SaveCaption, HelpTopic: string; DefaultSaveChecked: Boolean; var SaveOption: Boolean): TModalResult;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeymanDeveloperOptions;

{$R *.DFM}

class function TfrmMessageDlgWithSave.Execute(const Message, SaveCaption, HelpTopic: string;
  DefaultSaveChecked: Boolean; var SaveOption: Boolean): TModalResult;
var
  dlg: TfrmMessageDlgWithSave;
begin
  dlg := TfrmMessageDlgWithSave.Create(Application);
  try
    dlg.chkSave.Checked := DefaultSaveChecked;
    dlg.chkSave.Caption := SaveCaption;
    dlg.lblMessage.Caption := Message;
    dlg.FHelpTopic := HelpTopic;
    Result := dlg.ShowModal;
    SaveOption := dlg.chkSave.Checked;
  finally
    dlg.Free;
  end;
end;

function TfrmMessageDlgWithSave.GetHelpTopic: string;
begin
  Result := FHelpTopic;
end;

end.
