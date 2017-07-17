unit Windows8LanguageList;

interface

uses
  System.Generics.Collections,
  BCP47Tag;

type
  TWindows8Language = class
  private
    FLangID: Integer;
    FLocaleName: string;
    FBCP47Tag: string;
    FTag: TBCP47Tag;
  public
    constructor Create(const ABCP47Tag: string);
    destructor Destroy; override;
    property BCP47Tag: string read FBCP47Tag;
    property Tag: TBCP47Tag read FTag;
    property LocaleName: string read FLocaleName;
    property LangID: Integer read FLangID;
  end;

  TWindows8LanguageList = class(TObjectList<TWindows8Language>)
  private
    FIsSupported: Boolean;
  public
    constructor Create(ARefresh: Boolean = True);
    procedure Refresh;
    property IsSupported: Boolean read FIsSupported;
    function FindClosestByBCP47Tag(var BCP47Tag: string): TWindows8Language;
    function FindByBCP47Tag(BCP47Tag: string): TWindows8Language;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  RegistryKeys,
  utilsystem;

constructor TWindows8LanguageList.Create(ARefresh: Boolean = True);
begin
  inherited Create;
  if ARefresh then Refresh;
end;

function TWindows8LanguageList.FindByBCP47Tag(BCP47Tag: string): TWindows8Language;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(BCP47Tag, Items[i].BCP47Tag) then
      Exit(Items[i]);
  Result := nil;
end;

function TWindows8LanguageList.FindClosestByBCP47Tag(var BCP47Tag: string): TWindows8Language;
var
  tag: TBCP47Tag;
begin
  Result := nil;
  tag := TBCP47Tag.Create(BCP47Tag);
  try
    // Find tag by identical match
    Result := FindByBCP47Tag(tag.Tag);
    if Assigned(Result) then Exit;

    // Suppress the script
    tag.Script := '';
    Result := FindByBCP47Tag(tag.Tag);
    if Assigned(Result) then Exit;

    // Finally, suppress the region
    tag.Region := '';
    Result := FindByBCP47Tag(tag.Tag);
  finally
    if Assigned(Result) then
      BCP47Tag := tag.Tag;
    tag.Free;
  end;
end;

procedure TWindows8LanguageList.Refresh;
var
  i, j: Integer;
  Language: TWindows8Language;
  FValueNames: TStringList;
  FLanguageNames: TStringList;
begin
  Clear;
  with TRegistry.Create do
  try
    FIsSupported := OpenKeyReadOnly('Control Panel\International\User Profile') and ValueExists('Languages');
    if FIsSupported then
    begin
      FLanguageNames := TStringList.Create;
      FValueNames := TStringList.Create;
      try
        ReadMultiString('Languages', FLanguageNames);

        for i := 0 to FLanguageNames.Count - 1 do
        begin
          Language := TWindows8Language.Create(FLanguageNames[i]);
          Add(Language);
          if OpenKeyReadOnly('\Control Panel\International\User Profile\'+FLanguageNames[i]) then
          begin
            if ValueExists('TransientLangId') then
              Language.FLangId := ReadInteger('TransientLangId');
            if ValueExists('CachedLanguageName') then
              Language.FLocaleName := LoadIndirectString(ReadString('CachedLanguageName'));

            if Language.FLangId = 0 then
            begin
              FValueNames.Clear;
              GetValueNames(FValueNames);
              for j := 0 to FValueNames.Count - 1 do
                if (GetDataType(FValueNames[j]) = rdInteger) and
                  (Copy(FValueNames[j],5,1) = ':') then
                begin
                  Language.FLangID := StrToIntDef('$'+Copy(FValueNames[j],1,4),0);
                  Break;
                end;
            end;
          end;
        end;
      finally
        FLanguageNames.Free;
        FValueNames.Free;
      end;
    end;
  finally
    Free;
  end;
end;

{ TWindows8Language }

constructor TWindows8Language.Create(const ABCP47Tag: string);
begin
  FBCP47Tag := ABCP47Tag;
  FTag := TBCP47Tag.Create(ABCP47Tag);
end;

destructor TWindows8Language.Destroy;
begin
  FreeAndNil(FTag);
  inherited Destroy;
end;

{function TWindows8Language.BCP47TagMatches(const ATag: string): Boolean;
begin
  Result := SameText(Copy(FBCP47Tag, 1, Length(ATag)), ATag);
end;}

end.
