(*
  Name:             KeymanTextEditorRichEdit
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
  History:          27 Mar 2008 - mcdurdin - Initial version
                    11 Jan 2011 - mcdurdin - I2632 - RichEdit fails to display Bengali characters correctly
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit KeymanTextEditorRichEdit;  // I3306

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ComCtrls,
  Messages, RichEdit;

type
  TKeymanTextEditorRichEdit = class(TRichEdit)
  private
    wm_kmselectlang: UINT;
    FOnKeymanSelectLang: TNotifyEvent;
    procedure KeymanSelectLang;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnKeymanSelectLang: TNotifyEvent read FOnKeymanSelectLang write FOnKeymanSelectLang;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [TKeymanTextEditorRichEdit]);
end;

{ TKeymanTextEditorRichEdit }

constructor TKeymanTextEditorRichEdit.Create(AOwner: TComponent);
begin
  wm_kmselectlang := RegisterWindowMessage('WM_KMSELECTLANG');
  inherited Create(AOwner);
end;

procedure TKeymanTextEditorRichEdit.KeymanSelectLang;
begin
  if Assigned(FOnKeymanSelectLang) then
    FOnKeymanSelectLang(Self);
end;

procedure TKeymanTextEditorRichEdit.WndProc(var Message: TMessage);
begin
  if Message.Msg = wm_kmselectlang then
    KeymanSelectLang;
  inherited;
end;

end.
