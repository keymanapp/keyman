unit Keyman.Configuration.System.HttpServer.App.TextEditorFonts;

interface

type
  ITextEditorFontsSharedData = interface
    ['{A746AE29-AF9C-49AD-9A83-CB1AA3626BFE}']
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
