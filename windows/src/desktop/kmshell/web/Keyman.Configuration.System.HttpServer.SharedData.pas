unit Keyman.Configuration.System.HttpServer.SharedData;

interface

//
// This unit is transitional but will probably live a while. It contains data
// shared between UI and http response threads. Required usage model: the
// owner dialog sets the data, and the http response threads read them. When
// the owner dialog is destroyed, the data will soon become out of date, but
// the lifetime of the objects contained will exceed that of the owner dialog,
// so that incomplete http requests can finish safely.
//
// Interfaces are used for automatic reference counting. When the interface
// reference count is zero, it will be removed from the list automatically but
// the list will not be compacted, so indexes will never be reused within a
// single process session.
//
// Ideally, this will be refactored to make it unnecessary, but for now this
// is the cleanest refactor pathway.
//

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs;

type
  THttpServerSharedData = class
  private
    FData: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Data: IUnknown): Integer;
    procedure Remove(Tag: Integer);
    function Get(Tag: Integer): IUnknown;
  end;

implementation

uses
  System.SysUtils;

{ THttpServerSharedData }

function THttpServerSharedData.Add(Data: IUnknown): Integer;
begin
  // We use a non-zero tag to avoid difficult to trace bugs where an
  // uninitialised page tag is referenced (which in a class will be zero).
  Result := FData.Add(Data) + 1;
end;

procedure THttpServerSharedData.Remove(Tag: Integer);
begin
  // Decrements reference count, ensures new attempts to get the data will fail
  // and allowing the data to be released when all existing references disappear
  if Tag > 0 then
    FData[Tag-1] := nil;
end;

constructor THttpServerSharedData.Create;
begin
  inherited Create;
  FData := TInterfaceList.Create;
end;

destructor THttpServerSharedData.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

function THttpServerSharedData.Get(Tag: Integer): IUnknown;
begin
  if Tag > 0
    then Result := FData[Tag-1]
    else Result := nil;
end;

end.
