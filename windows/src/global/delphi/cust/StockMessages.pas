(*
  Name:             StockMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Add support for default file names
                    05 Dec 2006 - mcdurdin - Add SaveToPoFile
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit StockMessages;  // I3306

interface

uses Classes, SysUtils;

(*
type
  TStockMessageParameter = class(TCollectionItem)
  private
    FValue: string;
    FDescription: string;
  published
    property Value: string read FValue write FValue;
    property Description: string read FDescription write FDescription;
  end;

  TStockMessageParameters = class(TCollection)
  private
    function GetItem(Index: Integer): TStockMessageParameter;
    procedure SetItem(Index: Integer; const Value: TStockMessageParameter);
  public
    constructor Create; reintroduce;
    function Add: TStockMessageParameter;
    property Items[Index: Integer]: TStockMessageParameter read GetItem write SetItem; default;
  end;

  TStockMessageString = class(TCollectionItem)
  private
    FComment: string;
    FDefStr: string;
    FName: string;
    FParameters: TStockMessageParameters;
    procedure SetParameters(const Value: TStockMessageParameters);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property DefStr: string read FDefStr write FDefStr;
    property Comment: string read FComment write FComment;
    property Parameters: TStockMessageParameters read FParameters write SetParameters;
  end;

  TStockMessageSection = class;

  TStockMessageStrings = class(TCollection)
  private
    FOwner: TStockMessageSection;
    function GetItem(Index: Integer): TStockMessageString;
    procedure SetItem(Index: Integer; const Value: TStockMessageString);
  public
    constructor Create(AOwner: TStockMessageSection); reintroduce;
    function Add: TStockMessageString;
    property Items[Index: Integer]: TStockMessageString read GetItem write SetItem; default;
    property Section: TStockMessageSection read FOwner;
  end;

  TStockMessageSection = class(TCollectionItem)
  private
    FName: string;
    FStrings: TStockMessageStrings;
    procedure SetStrings(const Value: TStockMessageStrings);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Strings: TStockMessageStrings read FStrings write SetStrings;
  end;

  TStockMessageSections = class(TCollection)
  private
    function GetItem(Index: Integer): TStockMessageSection;
    procedure SetItem(Index: Integer; const Value: TStockMessageSection);
  public
    constructor Create; reintroduce;
    function Add: TStockMessageSection;
    property Items[Index: Integer]: TStockMessageSection read GetItem write SetItem; default;
  end;

  TStockMessages = class(TComponent)
  private
    FSections: TStockMessageSections;
    procedure SetSections(const Value: TStockMessageSections);
  public
    constructor Create(stream: TStream); reintroduce;
    destructor Destroy; override;
    procedure SaveToPas(FileName, DefaultsFileName: string);
    procedure Save(stream: TStream);
  published
    property Sections: TStockMessageSections read FSections write SetSections;
  end;
*)

implementation

uses
  KeymanVersion,
  StrUtils;

{ TStockMessageString }
(*
constructor TStockMessageString.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParameters := TStockMessageParameters.Create;
end;

destructor TStockMessageString.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TStockMessageString.SetParameters(const Value: TStockMessageParameters);
begin
  FParameters.Assign(Value);
end;

{ TStockMessageParameters }

function TStockMessageParameters.Add: TStockMessageParameter;
begin
  Result := inherited Add as TStockMessageParameter;
end;

constructor TStockMessageParameters.Create;
begin
  inherited Create(TStockMessageParameter);
end;

function TStockMessageParameters.GetItem(Index: Integer): TStockMessageParameter;
begin
  Result := inherited GetItem(Index) as TStockMessageParameter;
end;

procedure TStockMessageParameters.SetItem(Index: Integer;
  const Value: TStockMessageParameter);
begin
  inherited SetItem(Index, Value);
end;

{ TStockMessageSection }

constructor TStockMessageSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStrings := TStockMessageStrings.Create(Self);
end;

destructor TStockMessageSection.Destroy;
begin
  FreeAndNil(FStrings);
  inherited Destroy;
end;

procedure TStockMessageSection.SetStrings(const Value: TStockMessageStrings);
begin
  FStrings.Assign(Value);
end;

{ TStockMessages }

constructor TStockMessages.Create(stream: TStream);
begin
  inherited Create(nil);
  FSections := TStockMessageSections.Create;
  stream.ReadComponent(Self);
end;

destructor TStockMessages.Destroy;
begin
  FreeAndNil(FSections);
  inherited Destroy;
end;

procedure TStockMessages.Save(stream: TStream);
begin
  stream.WriteComponent(Self);
end;

procedure TStockMessages.SaveToPas(FileName, DefaultsFileName: string);
var
  f: TextFile;
  i, j: Integer;
  stype, sconst: string;
  sdefaults: string;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  writeln(f, '// Generated file - generated by MakeStockKCT at '+FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
  writeln(f, 'unit '+ChangeFileExt(ExtractFileName(FileName), '')+';'#13#10'interface'#13#10);
//  writeln(f, 'uses keymanapi_TLB;'#13#10);

  stype := 'type TMessageIdentifier = (SKNull, '#13#10;
  sconst := 'const MessageIdentifierNames: array[TMessageIdentifier] of string = (''SKNull'', '#13#10;
  sdefaults := 'const SMessageDefaults: array[TMessageIdentifier] of string = ('''', '#13#10;

  for i := 0 to FSections.Count - 1 do
  begin
    stype := stype + #13#10'  {'+FSections[i].Name+'}'#13#10#13#10;
    sconst := sconst + #13#10'  {'+FSections[i].Name+'}'#13#10#13#10;
    sdefaults := sdefaults + #13#10'  {'+FSections[i].Name+'}'#13#10#13#10;
    for j := 0 to FSections[i].Strings.Count - 1 do
      with FSections[i].Strings[j] do
      begin
        stype := stype + '  '+Name;
        sconst := sconst + '  '''+FName+'''';
        sdefaults := sdefaults + '  '+AnsiReplaceText(QuotedStr(Trim(FDefStr)), #13#10, '''+#13#10+''');
        if (j < FSections[i].Strings.Count-1) or (i < FSections.Count-1) then
        begin
          stype := stype + ',';
          sconst := sconst + ',';
          sdefaults := sdefaults + ',';
        end;

        stype := stype + #13#10;
        sconst := sconst + #13#10;
        sdefaults := sdefaults + #13#10;

      end;
  end;

  stype := stype + ');';
  sconst := sconst + ');';
  sdefaults := sdefaults + ');';
  
  writeln(f, stype);
  writeln(f);
  writeln(f, 'function MsgIdFromString(const msgid: WideString): TMessageIdentifier;');
  writeln(f, 'function StringFromMsgId(const msgid: TMessageIdentifier): WideString;');
  writeln(f);
  writeln(f, 'implementation');
  writeln(f);
  writeln(f, sconst);
  writeln(f);
  writeln(f, 'function StringFromMsgId(const msgid: TMessageIdentifier): WideString;');
  writeln(f, 'begin');
  writeln(f, '  Result := MessageIdentifierNames[msgid];');
  writeln(f, 'end;');
  writeln(f);
  writeln(f, 'function MsgIdFromString(const msgid: WideString): TMessageIdentifier;');
  writeln(f, 'var i: TMessageIdentifier;');
  writeln(f, 'begin');
  writeln(f, '  for i := Low(TMessageIdentifier) to High(TMessageIdentifier) do');
  writeln(f, '    if MessageIdentifierNames[i] = msgid then begin Result := i; Exit; end;');
  writeln(f, '  Result := SKNull;');
  writeln(f, 'end;');
  writeln(f);
  writeln(f, 'end.');

  CloseFile(f);

  AssignFile(f, DefaultsFileName);
  Rewrite(f);
  writeln(f, '// Generated file - generated by MakeStockKCT at '+FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
  writeln(f, 'unit '+ChangeFileExt(ExtractFileName(DefaultsFileName), '')+';'#13#10'interface'#13#10);
  writeln(f, 'function MsgDefault(const msgid: WideString): WideString;');
  writeln(f);
  writeln(f, 'implementation');
  writeln(f);
  writeln(f, 'uses MessageIdentifierConsts;');
  writeln(f);
  writeln(f, sdefaults);
  writeln(f);
  writeln(f, 'function MsgDefault(const msgid: WideString): WideString;');
  writeln(f, 'begin');
  writeln(f, '  Result := SMessageDefaults[MsgIdFromString(msgid)];');  // I3310
  writeln(f, 'end;');
  writeln(f);
  writeln(f, 'end.');

  CloseFile(f);
end;

procedure TStockMessages.SetSections(const Value: TStockMessageSections);
begin
  FSections.Assign(Value);
end;

{ TStockMessageStrings }

function TStockMessageStrings.Add: TStockMessageString;
begin
  Result := inherited Add as TStockMessageString;
end;

constructor TStockMessageStrings.Create(AOwner: TStockMessageSection);
begin
  FOwner := AOwner;
  inherited Create(TStockMessageString);
end;

function TStockMessageStrings.GetItem(Index: Integer): TStockMessageString;
begin
  Result := inherited GetItem(Index) as TStockMessageString;
end;

procedure TStockMessageStrings.SetItem(Index: Integer; const Value: TStockMessageString);
begin
  inherited SetItem(Index, Value);
end;

{ TStockMessageSections }

function TStockMessageSections.Add: TStockMessageSection;
begin
  Result := inherited Add as TStockMessageSection;
end;

constructor TStockMessageSections.Create;
begin
  inherited Create(TStockMessageSection);
end;

function TStockMessageSections.GetItem(Index: Integer): TStockMessageSection;
begin
  Result := inherited GetItem(Index) as TStockMessageSection;
end;

procedure TStockMessageSections.SetItem(Index: Integer; const Value: TStockMessageSection);
begin
  inherited SetItem(Index, Value);
end;
*)

end.


