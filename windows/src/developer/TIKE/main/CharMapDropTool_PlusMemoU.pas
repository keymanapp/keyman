(*
  Name:             CharMapDropTool_PlusMemoU
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
*)
unit CharMapDropTool_PlusMemoU;  // I3323

interface

uses
  CharacterDragObject,
  CharMapDropTool,
  CharMapInsertMode,
  Controls,
  KeymanDeveloperMemo;

type
  TCharMapDropToolControl_PlusMemoU = class(TCharMapDropToolControl)
  private
    function memo: TKeymanDeveloperMemo;
  public
    class function Handles: TControlClass; override;
    procedure Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean); override;
    procedure Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer); override;
  end;

implementation

uses
  Winapi.Messages,
  Winapi.Windows;

type
  TKeymanDeveloperMemoCracker = class(TKeymanDeveloperMemo)
  end;

procedure TCharMapDropToolControl_PlusMemoU.Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean);
begin
  Accept := (X >= 0) and (Y >= 0);
  if Accept then
  begin
{$IFDEF USE_PLUSMEMO}
    TKeymanDeveloperMemoCracker(memo).MouseMove([], x, y);
    TKeymanDeveloperMemoCracker(memo).SelStart := TKeymanDeveloperMemoCracker(memo).MouseNav.Pos;
    CreateCaret(TKeymanDeveloperMemoCracker(memo).Handle, 0, TKeymanDeveloperMemoCracker(memo).GetCaretWidth,
      TKeymanDeveloperMemoCracker(memo).LineHeight);
    TKeymanDeveloperMemoCracker(memo).PlaceCaret;
    ShowCaret(TKeymanDeveloperMemoCracker(memo).Handle);
{$ELSE}
    memo.SelStart := SendMessage(memo.Handle, EM_CHARFROMPOS, 0, MAKELONG(x, y));
    memo.SelLength := 0;
{$ENDIF}
  end;
end;

procedure TCharMapDropToolControl_PlusMemoU.Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer);
begin
  memo.SelText := CharMapDropTool_Insert(AInsertType, AObject, memo.LinesArray[memo.SelLine], memo.SelCol);
  memo.SetFocus;
end;

class function TCharMapDropToolControl_PlusMemoU.Handles: TControlClass;
begin
  Result := TKeymanDeveloperMemo;
end;

function TCharMapDropToolControl_PlusMemoU.memo: TKeymanDeveloperMemo;
begin
  Result := Control as TKeymanDeveloperMemo;
end;

initialization
  TCharMapDropToolControl_PlusMemoU.Register;
end.
