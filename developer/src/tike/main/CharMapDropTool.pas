(*
  Name:             CharMapDropTool
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
                    14 Sep 2006 - mcdurdin - Refactor TCharacterDragObject into separate unit
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit CharMapDropTool;  // I3306

interface

uses
  System.Types,
  Windows, Classes, Controls, Contnrs, Forms, SysUtils, CharacterDragObject, CharMapInsertMode, CleartypeDrawCharacter;

type

  TCharMapDropTool = class;

  TCharMapDropToolControl = class(TComponent)
  private
    FControl: TControl;
    FInsertMode: TCharMapInsertMode;
    FTool: TCharMapDropTool;
    FOnDragOver: TDragOverEvent;
    FOnDragDrop: TDragDropEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class procedure Register;
    class function Handles: TControlClass; virtual; abstract;
    constructor Create(ATool: TCharMapDropTool; AControl: TControl; AInsertMode: TCharMapInsertMode; AOnDragOver: TDragOverEvent; AOnDragDrop: TDragDropEvent); reintroduce;
    destructor Destroy; override;
    procedure Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean); virtual; abstract;
    procedure Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer); virtual; abstract;
    property Control: TControl read FControl;
    property InsertMode: TCharMapInsertMode read FInsertMode;
  end;

  TCharMapDropToolControlDefault = class(TCharMapDropToolControl)
  public
    class function Handles: TControlClass; override;
    procedure Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean); override;
    procedure Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer); override;
  end;

  TCharMapDropToolControlClass = class of TCharMapDropToolControl;

  TCharMapDropToolControls = class(TObjectList)
  private
    function GetItem(Index: Integer): TCharMapDropToolControl;
    procedure SetItem(Index: Integer; const Value: TCharMapDropToolControl);
  public
    function IndexOfControl(AControl: TControl): Integer;
    property Items[Index: Integer]: TCharMapDropToolControl read GetItem write SetItem; default;
  end;

  TCharMapDropToolControlClasses = TClassList;

  TCharMapDropTool = class
  private
    FControls: TCharMapDropToolControls;
    FControlClasses: TCharMapDropToolControlClasses;

    procedure DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Handle(Control: TControl; AInsertMode: TCharMapInsertMode = cmimDefault; AOnDragOver: TDragOverEvent = nil; AOnDragDrop: TDragDropEvent = nil);
    procedure InsertToControl(AControl: TControl; AObject: TCharacterDragObject); overload;
    procedure InsertToControl(AControl: TControl; const AText: WideString; AInsertMode: TCharMapInsertMode); overload;
    property Controls: TCharMapDropToolControls read FControls;
  end;

function GetCharMapDropTool: TCharMapDropTool;

function CharMapDropTool_GetCharContext(const s: WideString; col: Integer): Integer;
function CharMapDropTool_Insert(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; const ALine: WideString; ASelCol: Integer): WideString;

implementation

uses
  Graphics,
  UfrmCharacterMapNew;

var FCharMapDropTool: TCharMapDropTool = nil;

type
  TControlExpose = class(TControl);

function GetCharMapDropTool: TCharMapDropTool;
begin
  if not Assigned(FCharMapDropTool) then
    FCharMapDropTool := TCharMapDropTool.Create;
  Result := FCharMapDropTool;
end;

{ TCharMapDropTool }

constructor TCharMapDropTool.Create;
begin
  inherited Create;
  FControls := TCharMapDropToolControls.Create;
  FControlClasses := TCharMapDropToolControlClasses.Create;
end;

destructor TCharMapDropTool.Destroy;
begin
  FControlClasses.Free;
  FControls.Free;
  inherited Destroy;
end;

procedure TCharMapDropTool.DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  n: Integer;
  im: TCharMapInsertMode;
begin
  n := FControls.IndexOfControl(Sender as TControl);
  if n >= 0 then
  begin
    if (FControls[n].FInsertMode = cmimCustom) and Assigned(FControls[n].FOnDragDrop) then
    begin
      FControls[n].FOnDragDrop(Sender, Source, X, Y);
    end
    else
    begin
      if FControls[n].FInsertMode = cmimDefault
        then im := frmCharacterMapNew.InsertMode
        else im := FControls[n].FInsertMode;
      if im in [cmimDefault, cmimCustom] then Exit; { no drop }
      FControls[n].Drop(im, Source as TCharacterDragObject, X, Y);
    end;
  end;
end;

procedure TCharMapDropTool.DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  n: Integer;
begin
  Accept := False;
  if (Source is TCharacterDragObject) and (Sender is TControl) then
  begin
    n := FControls.IndexOfControl(Sender as TControl);
    if n >= 0 then
    begin
      if (FControls[n].FInsertMode = cmimCustom) and Assigned(FControls[n].FOnDragOver)
        then FControls[n].FOnDragOver(Sender, Source, X, Y, State, Accept)
        else FControls[n].Drag(Source as TCharacterDragObject, X, Y, Accept);
    end;
  end;
end;

procedure TCharMapDropTool.Handle(Control: TControl; AInsertMode: TCharMapInsertMode = cmimDefault; AOnDragOver: TDragOverEvent = nil; AOnDragDrop: TDragDropEvent = nil);
var
  i: Integer;
begin
  if Control is TWinControl then
    with Control as TWinControl do
      for i := 0 to ControlCount - 1 do
        Self.Handle(Controls[i], AInsertMode, AOnDragOver, AOnDragDrop);

  i := FControls.IndexOfControl(Control);
  if i >= 0 then
  begin
    FControls[i].FInsertMode := AInsertMode;
    FControls[i].FOnDragOver := AOnDragOver;
    FControls[i].FOnDragDrop := AOnDragDrop;
  end
  else
  begin
    for i := 0 to FControlClasses.Count - 1 do
      if Control is TCharMapDropToolControlClass(FControlClasses[i]).Handles then
      begin
        FControls.Add(TCharMapDropToolControlClass(FControlClasses[i]).Create(Self, Control, AInsertMode, AOnDragOver, AOnDragDrop));
        Exit;
      end;

    if Assigned(AOnDragOver) and Assigned(AOnDragDrop) then
      FControls.Add(TCharMapDropToolControlDefault.Create(Self, Control, cmimCustom, AOnDragOver, AOnDragDrop));
  end;
end;

procedure TCharMapDropTool.InsertToControl(AControl: TControl;
  const AText: WideString; AInsertMode: TCharMapInsertMode);
var
  n: Integer;
  AObject: TCharacterDragObject;
begin

  // do a manual insert (when the user presses ENTER to insert a code
  n := GetCharMapDropTool.Controls.IndexOfControl(AControl);
  if n < 0 then Exit;
  AObject := TCharacterDragObject.Create;
  try
    AObject.Text[AInsertMode] := AText;
    FControls[n].Drop(AInsertMode, AObject, -1, -1);
  finally
    AObject.Free;
  end;
end;

procedure TCharMapDropTool.InsertToControl(AControl: TControl; AObject: TCharacterDragObject);
var
  n: Integer;
  im: TCharMapInsertMode;
begin
  // do a manual insert (when the user presses ENTER to insert a code
  n := GetCharMapDropTool.Controls.IndexOfControl(AControl);
  if n < 0 then Exit;
  if FControls[n].FInsertMode = cmimDefault
    then im := frmCharacterMapNew.InsertMode
    else im := FControls[n].FInsertMode;
  if im in [cmimDefault, cmimCustom] then Exit; { no drop }
  FControls[n].Drop(im, AObject, -1, -1);
end;

{ TCharMapDropToolControls }

function TCharMapDropToolControls.GetItem(Index: Integer): TCharMapDropToolControl;
begin
  Result := inherited GetItem(Index) as TCharMapDropToolControl;
end;

function TCharMapDropToolControls.IndexOfControl(AControl: TControl): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FControl = AControl then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TCharMapDropToolControls.SetItem(Index: Integer; const Value: TCharMapDropToolControl);
begin
  inherited SetItem(Index, Value);
end;

{ TCharMapDropToolControl }

constructor TCharMapDropToolControl.Create(ATool: TCharMapDropTool;
  AControl: TControl; AInsertMode: TCharMapInsertMode; AOnDragOver: TDragOverEvent;
  AOnDragDrop: TDragDropEvent);
begin
  inherited Create(nil);
  FTool := ATool;
  FControl := AControl;
  FInsertMode := AInsertMode;
  FOnDragOver := AOnDragOver;
  FOnDragDrop := AOnDragDrop;

  TControlExpose(FControl).OnDragOver := FTool.DragOver;
  TControlExpose(FControl).OnDragDrop := FTool.DragDrop;

  FControl.FreeNotification(Self);
end;

destructor TCharMapDropToolControl.Destroy;
begin
  if FControl <> nil then
  begin
    FControl.RemoveFreeNotification(Self);
    TControlExpose(FControl).OnDragOver := nil;
    TControlExpose(FControl).OnDragDrop := nil;
  end;
  inherited Destroy;
end;

procedure TCharMapDropToolControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FControl) and (Operation = opRemove) then
  begin
    FControl := nil;
    FTool.Controls.Remove(Self);
  end;
end;

class procedure TCharMapDropToolControl.Register;
begin
  GetCharMapDropTool.FControlClasses.Add(Self);
end;


function CharMapDropTool_GetCharContext(const s: WideString; col: Integer): Integer;
  { 0 = in code; 1 = in paren; 2 = in comment; 3 = whitespace before ', " = in quote }
var
  i: Integer;
  FInQuote: WideChar;
  FInParen: Boolean;
begin
  FInQuote := #0; FInParen := False;
  if col > Length(s) then col := Length(s);
  for i := 1 to col do
  begin
    if FInQuote <> #0 then
    begin
      if s[i] = FInQuote then
        FInQuote := #0;
    end
    else if FInParen then
    begin
      if s[i] = ')' then
        FInParen := False
    end
    else if (s[i] = '''') or (s[i] = '"') then
      FInQuote := s[i]
    else if s[i] = '(' then
      FInParen := True
    else if (s[i] = 'c') or (s[i] = 'C') then
    begin
      if (i = 1) or CharInSet(Char(s[i-1]), [#1..#32]) then
      begin
        // Start of comment
        Result := 2;
        Exit;
      end;
    end;
  end;

  if FInParen then Result := 1
  else if FInQuote <> #0 then Result := Ord(FInQuote)
  else if (col = 0) or CharInSet(Char(s[col]), [#1..#32]) then Result := 3 // Whitespace
  else Result := 0; // no whitespace
end;

function CharMapDropTool_Insert(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; const ALine: WideString; ASelCol: Integer): WideString;
var
  FCharContext: Integer;
begin
  Result := '';
  FCharContext := CharMapDropTool_GetCharContext(ALine, ASelCol);
  
  case AInsertType of
    cmimText:
      Result := AObject.Text[cmimCharacter];
    cmimCode, cmimName:
      if FCharContext > 31 then
        Result := WideChar(FCharContext) + ' ' + AObject.Text[AInsertType] + ' '
      else if (FCharContext <> 3) then
        Result := ' ' + AObject.Text[AInsertType] + ' '
      else
        Result := AObject.Text[AInsertType] + ' ';
    cmimCharacter:
      begin
        { Determine if cursor currently within a text block or a comment }
        if AObject.Text[AInsertType] = WideChar(FCharContext) then
        begin
          if AObject.Text[AInsertType] = '''' then Result := ''' "'''
          else Result := '" ''"';
        end
        else if FCharContext > 31 then
          Result := AObject.Text[AInsertType]
        else if FCharContext = 2 then
          Result := AObject.Text[AInsertType]
        else if AObject.Text[AInsertType] = '''' then
          if FCharContext = 3 then Result := '"''' else Result := ' "'''
        else
          if FCharContext = 3 then Result := ''''+AObject.Text[AInsertType] else Result := ' '''+AObject.Text[AInsertType];
      end;
  end;
end;

{ TCharMapDropToolControlDefault }

procedure TCharMapDropToolControlDefault.Drag(AObject: TCharacterDragObject; X,
  Y: Integer; var Accept: Boolean);
begin
  if Assigned(FOnDragOver)
    then FOnDragOver(Self, AObject, X, Y, dsDragMove, Accept)
    else Accept := False;
end;

procedure TCharMapDropToolControlDefault.Drop(AInsertType: TCharMapInsertMode;
  AObject: TCharacterDragObject; X, Y: Integer);
begin
  if Assigned(FOnDragDrop) then
  begin
    AObject.InsertType := AInsertType;
    FOnDragDrop(Self, AObject, X, Y);
  end;
end;

class function TCharMapDropToolControlDefault.Handles: TControlClass;
begin
  Result := TWinControl;
end;

initialization
finalization
  FreeAndNil(FCharMapDropTool);
end.
