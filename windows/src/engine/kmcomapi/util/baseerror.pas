unit baseerror;

interface

uses SysUtils;

type
  EKeymanBaseError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(AErrorCode: Integer; const Message: string);
    constructor CreateFmt(AErrorCode: Integer; const Message: string; const Args: array of const);
    property ErrorCode: Integer read FErrorCode;
  end;

implementation

{ EKeymanBaseError }

constructor EKeymanBaseError.Create(AErrorCode: Integer; const Message: string);
begin
  FErrorCode := AErrorCode;
  inherited Create(Message);
end;

constructor EKeymanBaseError.CreateFmt(AErrorCode: Integer; const Message: string; const Args: array of const);
begin
  FErrorCode := AErrorCode;
  inherited CreateFmt(Message, Args);
end;

end.
