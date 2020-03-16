unit Sentry.Client.Vcl;

interface

uses
  System.SysUtils,
  Sentry.Client;

type
  TSentryClientVcl = class(TSentryClient)
  private
    procedure HandleApplicationException(Sender: TObject; E: Exception);
  public
    constructor Create(AOptions: TSentryClientOptions; ACaptureExceptions: Boolean = True); override;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Forms;

{ TSentryClientVcl }

constructor TSentryClientVcl.Create(AOptions: TSentryClientOptions;
  ACaptureExceptions: Boolean);
begin
  inherited;
  if ACaptureExceptions then
  begin
    Application.OnException := HandleApplicationException;
  end;
end;

destructor TSentryClientVcl.Destroy;
begin
  inherited Destroy;
  if (TMethod(Application.OnException).Code = @TSentryClientVcl.HandleApplicationException)
      and (TMethod(Application.OnException).Data = Self) then
    Application.OnException := nil;
end;

procedure TSentryClientVcl.HandleApplicationException(Sender: TObject;
  E: Exception);
const
  Size = 1024;
var
  Buffer: array[0..Size-1] of Char;
begin
  if ExceptionErrorMessage(E, ExceptAddr, Buffer, Size) > 0 then
    ExceptionEvent(E.ClassName, Buffer);
end;

end.
