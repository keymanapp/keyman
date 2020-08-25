unit keymankeyboardlanguage;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,

  keymanautoobject,
  keymanapi_TLB,
  internalinterfaces,
  keymancontext,
  msctf;

type
  TKeymanKeyboardLanguage = class(TKeymanAutoObject, IKeymanKeyboardLanguage)   // I4376
  private
    FContext: TKeymanContext;
    FOwner: IKeymanKeyboard;
    FBCP47Code: string;
    FLangID: Integer;
    FName: string;
    FOriginalBCP47Code: string;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
      override;

    function GetLangName: string; virtual;

    { IKeymanKeyboardLanguage }
    function Get_BCP47Code: WideString; safecall;
    function Get_LangID: Integer; safecall;
    function Get_OwnerKeyboard: IKeymanKeyboard; safecall;
    function Get_Name: WideString; safecall;

    property Context: TKeymanContext read FContext;
    property Owner: IKeymanKeyboard read FOwner;

  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboard; const AOriginalBCP47Code, ABCP47Code: string; ALangID: Integer; const AName: string);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,

  glossary,
  keymanerrorcodes,
  utiltsf,
  utilxml;

{ TKeymanKeyboardLanguage }

constructor TKeymanKeyboardLanguage.Create(AContext: TKeymanContext;
  AOwner: IKeymanKeyboard; const AOriginalBCP47Code, ABCP47Code: string; ALangID: Integer; const AName: string);
begin
  FContext := AContext;
  FOwner := AOwner;
  FOriginalBCP47Code := AOriginalBCP47Code;
  FBCP47Code := ABCP47Code;
  FLangID := ALangID;
  FName := AName;
  inherited Create(AContext, IKeymanKeyboardLanguage);
end;

function TKeymanKeyboardLanguage.Get_BCP47Code: WideString;
begin
  Result := FBCP47Code;
end;

function TKeymanKeyboardLanguage.Get_LangID: Integer;
begin
  Result := FLangID;
end;

function TKeymanKeyboardLanguage.Get_Name: WideString;
begin
  Result := GetLangName;
end;

function TKeymanKeyboardLanguage.Get_OwnerKeyboard: IKeymanKeyboard;
begin
  Result := FOwner;
end;

function TKeymanKeyboardLanguage.GetLangName: string;
var
  i: Integer;
  Language: IKeymanLanguage;
begin
  Result := FName;
  if Result <> '' then
    Exit;

  for i := 0 to (Context.Languages as IKeymanLanguages).Count - 1 do
  begin
    Language := (Context.Languages as IKeymanLanguages)[i];
    if Language.LangID = Get_LangID then
    begin
      Result := Language.LocaleName;
      Break;
    end;
  end;

  if Result = '' then
    {ignore result := } GetLanguageName(Get_LangID, Result);
end;

function TKeymanKeyboardLanguage.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
begin
  Result := XMLFormat([
    'langid', IntToHex(Get_LangID, 4),
    'bcp47code', FBCP47Code,
    'langname', GetLangName]);
end;

end.
