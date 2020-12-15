unit Keyman.Configuration.System.HttpServer.App.TextEditorFonts;

interface

type
  ITextEditorFontsSharedData = interface
    ['{7442A323-C1E3-404B-BEEA-5B24A52BBB0E}']
    function AdditionalData: string;
  end;

  TTextEditorFontsSharedData = class(TInterfacedObject, ITextEditorFontsSharedData)
  private
    FAdditionalData: string;
  public
    constructor Create(const AAdditionalData: string);
    function AdditionalData: string;
  end;


implementation

{ TTextEditorFontsSharedData }

function TTextEditorFontsSharedData.AdditionalData: string;
begin
  Result := FAdditionalData;
end;

constructor TTextEditorFontsSharedData.Create(const AAdditionalData: string);
begin
  inherited Create;
  FAdditionalData := AAdditionalData;
end;

end.
