unit IntegerList;

interface

uses
  Classes;

type
  TIntegerList = class(TList)
  private
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    function Add(const Value: Integer): Integer;
    function IndexOf(const Value: Integer): Integer;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

implementation

{ TIntegerList }

function TIntegerList.Add(const Value: Integer): Integer;
begin
  Result := inherited Add(Pointer(Value));
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  Result := Integer(inherited Get(Index));
end;

function TIntegerList.IndexOf(const Value: Integer): Integer;
begin
  Result := inherited IndexOf(Pointer(Value));
end;

procedure TIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, Pointer(Value));
end;

end.
 