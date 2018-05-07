(*
  Name:             UfrmDebugStatus_Child
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    03 Aug 2015 - mcdurdin - I4809 - Track keystrokes in debug status form
*)
unit UfrmDebugStatus_Child;  // I3323

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, debugkeyboard, KeymanDeveloperMemo, UfrmTike, UfrmDebug;

type
  TfrmDebugStatus_Child = class(TTIKEForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDebugKeyboard: TDebugKeyboard;
    FDisplayFont: TFont;
    FEditorMemo: TKeymanDeveloperMemo;
  protected
    property EditorMemo: TKeymanDeveloperMemo read FEditorMemo;
    property debugkeyboard: TDebugKeyboard read FDebugKeyboard;
    property DisplayFont: TFont read FDisplayFont;

    function memoDebug: TKeymanDeveloperMemo;
    function frmDebugStatus: TForm;
    function DebugForm: TfrmDebug;

    procedure DebugKeyboardChanged; virtual;
    procedure DisplayFontChanged; virtual;
  public
    procedure SetDisplayFont(Value: TFont);
    procedure SetDebugKeyboard(const Value: TDebugKeyboard);
    procedure SetEditorMemo(Value: TKeymanDeveloperMemo);
  end;

implementation

uses
  UfrmDebugStatus;

{$R *.dfm}

{ TfrmDebugStatus_Child }

function TfrmDebugStatus_Child.DebugForm: TfrmDebug;
begin
  Result := (frmDebugStatus as TfrmDebugStatus).DebugForm;
end;

procedure TfrmDebugStatus_Child.DebugKeyboardChanged;
begin

end;

procedure TfrmDebugStatus_Child.DisplayFontChanged;
begin

end;

procedure TfrmDebugStatus_Child.FormCreate(Sender: TObject);
begin
  inherited;
  FDisplayFont := TFont.Create;
  DisplayFontChanged;
end;

procedure TfrmDebugStatus_Child.FormDestroy(Sender: TObject);
begin
  FDisplayFont.Free;
end;

function TfrmDebugStatus_Child.frmDebugStatus: TForm;
begin
  Result := Owner as TForm;
end;

function TfrmDebugStatus_Child.memoDebug: TKeymanDeveloperMemo;
begin
  if DebugForm <> nil then   // I4809
    Result := DebugForm.memo
  else
    Result := nil;
end;

procedure TfrmDebugStatus_Child.SetDebugKeyboard(const Value: TDebugKeyboard);
begin
  FDebugKeyboard := Value;
  DebugKeyboardChanged;
end;

procedure TfrmDebugStatus_Child.SetDisplayFont(Value: TFont);
begin
  FDisplayFont.Assign(Value);
  DisplayFontChanged;
end;

procedure TfrmDebugStatus_Child.SetEditorMemo(Value: TKeymanDeveloperMemo);
begin
  FEditorMemo := Value;
end;

end.
