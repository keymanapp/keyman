unit keymanpackagecontentfont;

interface

uses
  Windows, SysUtils, keymanautoobject, ActiveX, ComObj, keymancontext, keymanapi_TLB;

type
  TKeymanPackageContentFont = class(TKeymanAutoObject, IKeymanPackageContentFont)
  private
    FFilename, FName: WideString;
  protected

    { IKeymanPackageContentFont }
    function Get_Filename: WideString; safecall;
    function Get_Name: WideString; safecall;
  public
    constructor Create(AContext: TKeymanContext; const AFilename, AName: WideString);
  end;

implementation

{ TKeymanPackageContentFont }

constructor TKeymanPackageContentFont.Create(AContext: TKeymanContext; const AFilename,
  AName: WideString);
begin
  inherited Create(AContext, IKeymanPackageContentFont);
  FFilename := AFilename;
  FName := AName;
end;

function TKeymanPackageContentFont.Get_Filename: WideString;
begin
  Result := FFilename;
end;

function TKeymanPackageContentFont.Get_Name: WideString;
begin
  Result := FName;
end;

end.
