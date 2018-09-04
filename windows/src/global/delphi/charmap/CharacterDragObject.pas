(*
  Name:             CharacterDragObject
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    19 Nov 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
*)
unit CharacterDragObject;

interface

uses
  Controls, CharMapInsertMode;

type
  TCharacterDragObject = class(TDragObject)
  private
    FDefaultInsertMode: TCharMapInsertMode;
    FFontName: WideString;
    FText: array[TCharMapInsertMode] of WideString;
    FDragImages: TDragImageList;
    FInsertType: TCharMapInsertMode;
    function GetText(Index: TCharMapInsertMode): WideString;
    procedure SetText(Index: TCharMapInsertMode; Value: WideString);
    procedure CreateDragCursor;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HideDragImage; override;
    procedure ShowDragImage; override;

    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    procedure SetDragCursorOptions(ADefaultInsertMode: TCharMapInsertMode; AFontName: WideString);
    property Text[Index: TCharMapInsertMode]: WideString read GetText write SetText;
    property InsertType: TCharMapInsertMode read FInsertType write FInsertType; // used for custom insertion only
  end;

implementation

uses
  Windows,
  CleartypeDrawCharacter,
  Graphics,
  SysUtils,
  Types;

{ TCharacterDragObject }

destructor TCharacterDragObject.Destroy;
begin
  FreeAndNil(FDragImages);
  inherited Destroy;
end;

function TCharacterDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := crDrag
  else
    Result := crNoDrop;
end;

function TCharacterDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

function TCharacterDragObject.GetText(Index: TCharMapInsertMode): WideString;
begin
  Result := FText[Index];
end;

procedure TCharacterDragObject.HideDragImage;
begin
  FDragImages.HideDragImage;
end;

procedure TCharacterDragObject.SetDragCursorOptions(
  ADefaultInsertMode: TCharMapInsertMode; AFontName: WideString);
begin
  FDefaultInsertMode := ADefaultInsertMode;
  FFontName := AFontName;
end;

constructor TCharacterDragObject.Create;
begin
  FDragImages := TDragImageList.Create(nil);
  AlwaysShowDragImages := True;
  inherited Create;
end;

procedure TCharacterDragObject.CreateDragCursor;
var
  FDrawChar: TClearTypeDrawCharacter;
  FBitmap: TBitmap;
  pt: TPoint;
begin
  FBitmap := TBitmap.Create;
  FDrawChar := TClearTypeDrawCharacter.Create;
  try
    FBitmap.SetSize(32, 32);
    FDrawChar.SetFontDetails(FFontName, 24);
    FDrawChar.DisplayQuality := ctCleartype;
    FDrawChar.Color := clBlack;
    FDrawChar.DrawText(FBitmap.Canvas.Handle, TA_LEFT or TA_TOP or TA_UPDATECP, 0, 0, Rect(0, 0, 240, 64), FText[FDefaultInsertMode]);
    MoveToEx(FBitmap.Canvas.Handle, 0, 0, @pt);
    FBitmap.SetSize(pt.X + 6, FDrawChar.TextExtent(FBitmap.Canvas.Handle, FText[FDefaultInsertMode]).Y + 6);
    with FBitmap.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
      Brush.Color := clBlack;
      FrameRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
      Brush.Style := bsClear;
      FDrawChar.DrawText(Handle, TA_LEFT or TA_TOP, 3, 3, Rect(1, 1, FBitmap.Width-1, FBitmap.Height-1), FText[FDefaultInsertMode]);
    end;
    FDragImages.Clear;
    FDragImages.Width := FBitmap.Width;
    FDragImages.Height := FBitmap.Height;
    FDragImages.DragHotspot := Point(-12,-12);
    FDragImages.Add(FBitmap, nil);
  finally
    FDrawChar.Free;
    FBitmap.Free;
  end;
end;

procedure TCharacterDragObject.SetText(Index: TCharMapInsertMode; Value: WideString);
begin
  FText[Index] := Value;
  if Index = FDefaultInsertMode then
    CreateDragCursor;
end;

procedure TCharacterDragObject.ShowDragImage;
begin
  FDragImages.ShowDragImage;
end;

end.
