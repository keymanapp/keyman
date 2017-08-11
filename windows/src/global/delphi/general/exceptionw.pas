unit exceptionw;

interface

uses SysUtils;

type
  ExceptionWide = class(Exception)
  private
    FMessage: WideString;
  public
    constructor Create(const msg: WideString);
    constructor CreateFmt(const msg: WideString; const Args: array of const);
    property Message: WideString read FMessage;
  end;

implementation

uses Unicode;

{ ExceptionWide }

constructor ExceptionWide.Create(const msg: WideString);
begin
  FMessage := msg;
  inherited Create(msg);
end;

constructor ExceptionWide.CreateFmt(const msg: WideString; const Args: array of const);
begin
  FMessage := WideFormat(msg, Args);
  inherited Create(FMessage);
end;

end.
 