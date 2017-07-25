(*
  Name:             UfrmTike
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Add help support
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetText
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    30 Apr 2015 - mcdurdin - I4677 - V9.0 - Move Developer help to online only
                    03 Aug 2015 - mcdurdin - I4822 - Form sizes are incorrect with new theming
                    09 Aug 2015 - mcdurdin - I4841 - Restructure version 9 developer help
                    24 Aug 2015 - mcdurdin - I4873 - Branding editor needs smoother interactions with test window
*)
unit UfrmTike;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UserMessages;

type
  TTikeForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);   // I4873
  private
    { Private declarations }
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;   // I4873
  protected
    procedure FormShown; virtual;   // I4873
  public
    { Public declarations }
    procedure GetHelpTopic(var s: string); virtual;
  end;

implementation

{$R *.dfm}

type
  TFormHelper = class helper for Forms.TCustomForm   // I4822
  private
    procedure RestoreDesignClientSize;
  end;

procedure TTikeForm.FormCreate(Sender: TObject);
begin
  RestoreDesignClientSize;   // I4822
  inherited;
  HelpContext := 0;
  HelpKeyword := 'context_'+Copy(ClassName, 2, MAXINT);   // I4677   // I4841
end;

procedure TTikeForm.FormShow(Sender: TObject);   // I4873
begin
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure TTikeForm.FormShown;   // I4873
begin
end;

procedure TTikeForm.GetHelpTopic(var s: string);
begin
end;

procedure TTikeForm.WMUserFormShown(var Message: TMessage);   // I4873
begin
  FormShown;
end;



{ TFormHelper }

procedure TFormHelper.RestoreDesignClientSize;   // I4822
begin
(*  if BorderStyle in [bsSingle, bsDialog] then
  begin
    if Self.FClientWidth > 0 then ClientWidth := Self.FClientWidth;
    if Self.FClientHeight > 0 then ClientHeight := Self.FClientHeight;
  end;  *)
end;

end.
