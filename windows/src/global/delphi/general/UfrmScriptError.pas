(*
  Name:             UfrmScriptError
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Feb 2012

  Modified Date:    4 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Feb 2012 - mcdurdin - I2992 - Handle script errors more cleanly
                    04 Nov 2012 - mcdurdin - I3544 - V9.0 - Merge of I2992 - Handle script errors more cleanly
*)
unit UfrmScriptError;   // I3544

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmScriptError = class(TForm)
    lblBlurb1: TLabel;
    lblError: TLabel;
    lblBlurb2: TLabel;
    cmdContinue: TButton;
    cmdExit: TButton;
    cmdCancel: TButton;
    chkTellKeymanSupport: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

/// Return values. mrYes = continue, mrCancel = cancel, mrAbort = exit app   
function ShowScriptErrorDialog(Owner: TComponent; Message: WideString; var TellKeymanSupport: Boolean): TModalResult;

implementation

{$R *.dfm}

function ShowScriptErrorDialog(Owner: TComponent; Message: WideString; var TellKeymanSupport: Boolean): TModalResult;
begin
  with TfrmScriptError.Create(Owner) do
  try
    lblError.Caption := Message;
    Result := ShowModal;
    TellKeymanSupport := chkTellKeymanSupport.Checked;
  finally
    Free;
  end;
end;

end.
