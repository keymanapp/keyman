{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHtmlParser.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse@buypin.com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                Alexander Samusenko[sandx@chat.ru].

Last Modified: 2001-09-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHtmlParser;

interface

uses
  SysUtils, Classes,
  JclStrings,
  JvTypes, JvComponent;

type
  PTagInfo = ^TTagInfo;

  TTagInfo = record
    BeginPos: Integer;
    EndPos: Integer;
    BeginContext: Integer;
    EndContext: Integer;
    Key: Integer;
  end;

  TTagInfoList = class(TList)
  public
    procedure AddValue(const Value: TTagInfo);
    procedure Clear; override;
  end;

  TJvHtmlParser = class(TJvComponent)
  private
    FOnKeyFound: TJvKeyFoundEvent;
    FParser: TJvParserInfoList;
    FKeys: TStringList;
    FFile: TFileName;
    FTagList: TTagInfoList;
    procedure SetParser(Value: TJvParserInfoList);
    procedure SetFile(Value: TFileName);
    procedure SetTagList(const Value: TTagInfoList);
    function GetConditionsCount: Integer;
  protected
    procedure Loaded; override;
    property TagList: TTagInfoList read FTagList write SetTagList;
  public
    procedure AnalyseString(Str: string);
    procedure AnalyseFile;
    procedure AddCondition(Keyword, StartTag, EndTag: string;
      TextSelection: Integer = 0);
    procedure RemoveCondition(Index: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ConditionsCount: Integer read GetConditionsCount;
    procedure ClearConditions;
    procedure GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string); overload;
    procedure GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string; var TextSelection: Integer); overload;
  published
    property OnKeyFound: TJvKeyFoundEvent read FOnKeyFound write FOnKeyFound;
    property FileName: TFileName read Ffile write SetFile;
    property Parser: TJvParserInfoList read FParser write SetParser;
  end;

{Comparison function. Used internally  for  observance of the sequences tags}
// (rom) keep it local?
function CompareTags(Item1, Item2: Pointer): Integer;

implementation

function CompareTags(Item1, Item2: Pointer): Integer;
begin
  Result := (PTagInfo(Item1).BeginPos - PTagInfo(Item2).BeginPos);
end;

//=== TJvHtmlParser ==========================================================

constructor TJvHtmlParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParser := TStringList.Create;
  FKeys := TStringList.Create;
  FTagList := TTagInfoList.Create;
end;

destructor TJvHtmlParser.Destroy;
begin
  FParser.Free;
  FKeys.Free;
  FTagList.Free;
  inherited Destroy;
end;

procedure TJvHtmlParser.SetFile(Value: TFileName);
begin
  if FFile <> Value then
  begin
    FFile := Value;
    if not (csDesigning in ComponentState) then
      AnalyseFile;
  end;
end;

procedure TJvHtmlParser.SetParser(Value: TJvParserInfoList);
var
  I: Integer;
  ob: TJvParserInfo;
  cap: string;
begin
  FParser := Value;
  FKeys.Clear;
  I := 0;
  while I < FParser.Count do
  begin
    ob := TJvParserInfo.Create;
    try
      cap := FParser[I];
      Inc(I);
      ob.StartTag := FParser[I];
      Inc(I);
      ob.EndTag := FParser[I];
      Inc(I);
      ob.MustBe := StrToInt(FParser[I]);
      Inc(I);
      ob.TakeText := StrToInt(FParser[I]);
      Inc(I);
    finally
      FKeys.AddObject(cap, TObject(ob));
    end;
  end;
end;

// (rom) reimplemented with a TStringList

procedure TJvHtmlParser.AnalyseFile;
var
  List: TStringList;
begin
  begin
    List := TStringList.Create;
    try
      if FileExists(FileName) then
      begin
        List.LoadFromFile(FileName);
        AnalyseString(List.Text);
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TJvHtmlParser.AnalyseString(Str: string);
var
  st2: string;
  I, J, K, Index: Integer;
  TagInfo: TTagInfo;
begin
  if (FKeys.Count = 0) and (FParser.Count <> 0) then
    SetParser(FParser);
  if FKeys.Count > 0 then
  begin
    FTagList.Clear;
    for I := 0 to FKeys.Count - 1 do
    begin
      J := 1;
      while J <> 0 do
      begin
        J := StrSearch(TJvParserInfo(FKeys.Objects[I]).StartTag, Str, J);
        if J > 0 then
        begin
          K := StrSearch(TJvParserInfo(FKeys.Objects[I]).EndTag, Str, J);
          TagInfo.BeginPos := J;
          TagInfo.EndPos := K + length(TJvParserInfo(FKeys.Objects[I]).EndTag);
          TagInfo.Key := I;
          case TJvParserInfo(FKeys.Objects[I]).TakeText of
            0: //Between limits
              begin
                TagInfo.BeginContext := J +
                  Length(TJvParserInfo(FKeys.Objects[I]).StartTag);
                TagInfo.EndContext := K;
              end;
            1: //All before start tag
              begin
                TagInfo.BeginContext := 1;
                TagInfo.EndContext := J;
              end;
            2: //All after start tag
              begin
                TagInfo.BeginContext := J +
                  Length(TJvParserInfo(FKeys.Objects[I]).StartTag);
                TagInfo.EndContext := Length(Str);
              end;
            3: //The whole line if containing start tag
              begin
                TagInfo.BeginContext := J;
                TagInfo.EndContext := StrSearch('#1310', Str, J);
              end;
          end;
          FTagList.AddValue(TagInfo);
          J := J + Length(TJvParserInfo(FKeys.Objects[I]).StartTag);
        end;
      end;
    end;
  end;
  FTagList.Sort(CompareTags);
  with FTagList do
  begin
    for Index := 0 to (Count - 1) do
    begin
      st2 := Copy(Str, PTagInfo(Items[Index]).BeginContext,
        PTagInfo(Items[Index]).EndContext -
        PTagInfo(Items[Index]).BeginContext);
      if Assigned(FOnKeyFound) then
        FOnKeyFound(Self, FKeys[PTagInfo(Items[Index]).Key], st2, Str);
    end;
  end;
end;

procedure TJvHtmlParser.AddCondition(Keyword, StartTag, EndTag: string;
  TextSelection: Integer = 0);
var
  ob: TJvParserInfo;
begin
  ob := TJvParserInfo.Create;
  ob.StartTag := StartTag;
  ob.EndTag := EndTag;
  ob.TakeText := TextSelection;
  FKeys.AddObject(Keyword, TObject(ob));
end;

procedure TJvHtmlParser.RemoveCondition(Index: Integer);
begin
  FKeys.Delete(Index);
end;

procedure TJvHtmlParser.ClearConditions;
begin
  FParser.Clear;
  FKeys.Clear;
end;

procedure TJvHtmlParser.GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string);
begin
  Keyword := FKeys[Index];
  StartTag := TJvParserInfo(FKeys.Objects[Index]).StartTag;
  EndTag := TJvParserInfo(FKeys.Objects[Index]).EndTag;
end;

procedure TJvHtmlParser.GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string;
  var TextSelection: Integer);
begin
  GetCondition(Index, Keyword, StartTag, EndTag);
  TextSelection := TJvParserInfo(FKeys.Objects[Index]).TakeText;
end;

function TJvHtmlParser.GetConditionsCount: Integer;
begin
  Result := FKeys.Count;
end;

procedure TJvHtmlParser.SetTagList(const Value: TTagInfoList);
begin
  FTagList := Value;
end;

procedure TJvHtmlParser.Loaded;
begin
  inherited Loaded;
  SetParser(FParser);
end;

//=== TTagInfoList ===========================================================

procedure TTagInfoList.AddValue(const Value: TTagInfo);
var
  P: PTagInfo;
begin
  GetMem(P, SizeOf(TTagInfo));
  if P <> nil then
  begin
    // (rom) simplified
    System.Move(Value, P^, SizeOf(TTagInfo));
    Add(P);
  end;
end;

procedure TTagInfoList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I], SizeOf(TTagInfo));
  inherited Clear;
end;

end.

