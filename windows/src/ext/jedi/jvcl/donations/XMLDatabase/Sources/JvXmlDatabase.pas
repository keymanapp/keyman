{
  Known limitations:
    -You have to place the columns you test in the where clause in the select clause too
    -Where conditions *MUST* be enclosed between parenthesis as ... WHERE (Col = 5) AND (Col2 < Col3) ...
    -Update statements are limited to simple operations like ... SET Col1 = Col1 + 1, Col2 = 4 ...

}

unit JvXmlDatabase;

interface

uses
  SysUtils, Classes, Contnrs, Dialogs, DateUtils, Math, JvSimpleXml;

type
  TJvXmlDatabase = class;
  TJvXmlQuery = class;
  TJvXmlQueryParser = class;
  TJvXmlDatabaseException = class(Exception);

  TJvXmlTable = class
  public
    Xml: TJvSimpleXml;
    Locked: Boolean;
    Filename: string;
  end;

  TJvXmlQueryTable = class
  public
    Name: string;
    Alias: string;
    constructor Create(AValue: string);
  end;

  TJvXmlQueryColumn = class
  public
    Name: string;
    Table: string;
    constructor Create(AValue: string);
  end;

  TJvXmlOrderConvertion = (ocNone, ocDate, ocInteger, ocFloat);
  TJvXmlQueryOrder = class
  public
    Column: string;
    Ascending: Boolean;
    Convertion: TJvXmlOrderConvertion;
    constructor Create(AValue: string);
  end;

  TJvXmlSQLOperator = (opEquals, opGreater, opSmaller, opGreaterEquals,
    opSmallerEquals, opLike, opNot, opOr, opAnd, opXor, opLeftParenthesis,
    opRightParenthesis, opConstant, opColumn, opNull, opNone);
  TJvXmlQueryCondition = class
  public
    Condition: string;
    Operator: TJvXmlSQLOperator;
    constructor Create(AOperator: TJvXmlSQLOperator; ACondition: string = '');
  end;

  TJvXmlSetKind = (skConstant, skColumn);
  TJvXmlSetOperator = (soNone, soAdd, soMultiply, soDivide, soSubstract);
  TJvXmlQueryAssignement = class
  public
    Column: string;
    ValueKind, SecondKind: TJvXmlSetKind;
    Operator: TJvXmlSetOperator;
    Value, SecondValue: string;
    constructor Create(AValue: string);
    procedure UpdateElem(AElement: TJvSimpleXmlElem);
  end;

  TJvXmlInstruction = (xiSelect, xiUpdate, xiInsert, xiDelete);
  TJvXmlQueryParser = class
  private
    FQuery: string;
    FTables: TObjectList;
    FColumns: TObjectList;
    FConditions: TObjectList;
    FOrders: TObjectList;
    FInstruction: TJvXmlInstruction;
    FInstructionStr: string;
    FTablesStr: string;
    FWhereStr: string;
    FColumnsStr: string;
    FLimitStr: string;
    FLimitBegin: Integer;
    FLimitCount: Integer;
    FOrderStr: string;
    FSetStr: string;
    FOrderTable: TJvSimpleXmlElem;
    FUpdates: TObjectList;
    FValuesStr: string;
    FValues: TStringList;
    function GetColumn(const AIndex: Integer): TJvXmlQueryColumn;
    function GetTable(const AIndex: Integer): TJvXmlQueryTable;
    function GetColumnsCount: Integer;
    function GetTablesCount: Integer;
    function GetCondition(const AIndex: Integer): TJvXmlQueryCondition;
    function GetConditionsCount: Integer;
    function OrderCallBack(Elems: TJvSimpleXmlElems; Index1, Index2: Integer): Integer;
    function GetValue(const AIndex: Integer): string;
    function GetValuesCount: Integer;
  protected
    function ReadToken: string;
    function ReadColumns(AEndStatement: array of string; ACanTerminate: Boolean): string;
    function ReadTables(AEndStatement: array of string): string;
    function ReadWhere(AEndStatement: array of string): string;
    function ReadLimit(AEndStatement: array of string): string;
    function ReadOrderBy(AEndStatement: array of string): string;
    function ReadSet(AEndStatement: array of string): string;
    function ReadValues(AEndStatement: array of string): string;
    function ReadStatement(AEndStatement: array of string; ACanTerminate: Boolean; var AValue: string): string;
    procedure DoValidateInstruction;
    procedure DoValidateColumns;
    procedure DoValidateTables;
    procedure DoValidateWhere;
    procedure DoValidateOrderBy;
    procedure DoValidateSet;
    procedure DoValidateValues;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Parse(AQuery: string);
    function CheckConditions(AXmlElem: TJvSimpleXmlElem): Boolean;
    procedure LimitTable(var ATable: TJvSimpleXmlElem);
    procedure OrderTable(var ATable: TJvSimpleXmlElem);
    procedure UpdateRow(ARow: TJvSimpleXmlElem);
    property Instruction: TJvXmlInstruction read FInstruction write FInstruction;
    property Tables[const AIndex: Integer]: TJvXmlQueryTable read GetTable;
    property TablesCount: Integer read GetTablesCount;
    property Columns[const AIndex: Integer]: TJvXmlQueryColumn read GetColumn;
    property ColumnsCount: Integer read GetColumnsCount;
    property Condition[const AIndex: Integer]: TJvXmlQueryCondition read GetCondition;
    property ConditionsCount: Integer read GetConditionsCount;
    property Value[const AIndex: Integer]: string read GetValue;
    property ValuesCount: Integer read GetValuesCount;
  end;

  TJvXmlQuery = class
  private
    FParser: TJvXmlQueryParser;
    FDatabase: TJvXmlDatabase;
    FResults: TJvSimpleXmlElem;
    FTables: TList;
    FLastId: Integer;
  protected
    procedure Query(AQuery: string);
  public
    constructor Create(AOwner: TJvXmlDatabase);
    destructor Destroy;override;

    property Results: TJvSimpleXmlElem read FResults;
    property LastId: Integer read FLastId;
  end;

  TJvXmlDatabase = class(TComponent)
  private
    FTablesPath: string;
    FTables: TObjectList;
  protected
    function GetTable(const AName: string): TJvSimpleXml;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    procedure SaveTables;
    function Query(AQuery: string): TJvXmlQuery;
    property TablesPath:string read FTablesPath write FTablesPath;
  end;

implementation

var
  RS_UNKNOWNINST: string    = 'Unknown Instruction %s';
  RS_UNEXPECTEDEND: string  = 'Unexpected end of query';
  RS_UNEXPECTEDINST: string = 'Unexpected statement %s';

{ TJvXmlDatabase }

{**********************************************************************}
constructor TJvXmlDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FTables := TObjectList.Create;
end;
{**********************************************************************}
destructor TJvXmlDatabase.Destroy;
begin
  FreeAndNil(FTables);
  inherited;
end;
{**********************************************************************}
function TJvXmlDatabase.GetTable(const AName: string): TJvSimpleXml;
var
 i: Integer;
 st: string;
 lTable: TJvXmlTable;
begin
  st := TablesPath + AName;
  for i:=0 to FTables.Count-1 do
    if TJvXmlTable(FTables[i]).Filename = st then
    begin
      result := TJvXmlTable(FTables[i]).Xml;
      Exit;
    end;

  lTable := TJvXmlTable.Create;
  lTable.Xml := TJvSimpleXml.Create(nil);
  lTable.Xml.LoadFromFile(st);
  lTable.Locked := false;
  lTable.Filename := st;
  FTables.Add(lTable);
  result := lTable.Xml;
end;
{**********************************************************************}
function TJvXmlDatabase.Query(AQuery: string): TJvXmlQuery;
begin
  result := TJvXmlQuery.Create(self);
  result.Query(AQuery);
end;
{**********************************************************************}
procedure TJvXmlDatabase.SaveTables;
var
 i: Integer;
begin
  for i:=0 to FTables.Count-1 do
    TJvXmlTable(FTables[i]).Xml.SaveToFile(TJvXmlTable(FTables[i]).Filename);
end;
{**********************************************************************}

{ TJvXmlQuery }

{**********************************************************************}
constructor TJvXmlQuery.Create(AOwner: TJvXmlDatabase);
begin
  FDatabase := AOwner;
  FParser := TJvXmlQueryParser.Create;
  FResults := TJvSimpleXmlElemClassic.Create(nil);
  FTables := TList.Create;
end;
{**********************************************************************}
destructor TJvXmlQuery.Destroy;
begin
  FParser.Free;
  FResults.Free;
  FTables.Free;
  inherited;
end;
{**********************************************************************}
procedure TJvXmlQuery.Query(AQuery: string);
var
 i, j, lMax: Integer;
 lElem: TJvSimpleXmlElemClassic;
 lValue: string;

  function IsColumnSelected(ATable, AColumn: string): Boolean;
  var
   i: Integer;
  begin
    result := false;
    for i:=0 to FParser.ColumnsCount-1 do
      if (FParser.Columns[i].Name = '*') or ((FParser.Columns[i].Name = AColumn)
        and ((FParser.Columns[i].Table = '') or (FParser.Columns[i].Table = ATable))) then
      begin
        result := true;
        Break;
      end;
  end;

  procedure ConstructTable(AIndex: Integer; var AElem: TJvSimpleXmlElemClassic);
  var
   i,j: Integer;
   lElem: TJvSimpleXmlElemClassic;
  begin
    if AIndex >= FTables.Count then
    begin
      if FParser.CheckConditions(AElem) then
        FResults.Items.Add(AElem)
      else
        AElem.Free;
    end
    else
    begin
      with TJvSimpleXml(FTables[AIndex]) do
        for i:=0 to Root.Items.Count-1 do
        begin
          lElem := TJvSimpleXmlElemClassic.Create(nil);
          lElem.Assign(AElem);

          //Select columns to add
          for j:=0 to Root.Items[i].Properties.Count-1 do
            if IsColumnSelected(FParser.Tables[AIndex].Alias, Root.Items[i].Properties[j].Name) then
              lElem.Properties.Add(Root.Items[i].Properties[j].Name, Root.Items[i].Properties[j].Value);

          ConstructTable(AIndex + 1, lElem);
        end;
    end;
  end;

  procedure DeleteRows;
  var
   i,j: Integer;
  begin
    for i:=0 to FTables.Count-1 do
      for j:=TJvSimpleXml(FTables[i]).Root.Items.Count-1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXml(FTables[i]).Root.Items[j]) then
          TJvSimpleXml(FTables[i]).Root.Items.Delete(j);
  end;

  procedure UpdateRows;
  var
   i,j: Integer;
  begin
    for i:=0 to FTables.Count-1 do
      for j:=TJvSimpleXml(FTables[i]).Root.Items.Count-1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXml(FTables[i]).Root.Items[j]) then
          FParser.UpdateRow(TJvSimpleXml(FTables[i]).Root.Items[j]);
  end;

begin
  //Parse
  FParser.Parse(AQuery);

  //Get all tables
  for i:=0 to FParser.TablesCount-1 do
    FTables.Add(FDatabase.GetTable(FParser.Tables[i].Name));

  //Execute
  case FParser.Instruction of
    xiSelect:
      begin
        lElem := TJvSimpleXmlElemClassic.Create(nil);
        lElem.Name := 'Item';
        FResults.Name := 'Results';
        ConstructTable(0, lElem);
      end;

    xiDelete:
      begin
        DeleteRows;
        FDatabase.SaveTables;
      end;

    xiUpdate:
      begin
        UpdateRows;
        FDatabase.SaveTables;
      end;

    xiInsert:
      begin
        if FTables.Count = 1 then
          with TJvSimpleXml(FTables[0]).Root.Items.Add('item') do
            for i:=0 to FParser.ColumnsCount-1 do
              if i < FParser.ValuesCount then
              begin
                lValue := FParser.Value[i];
                if lValue = 'NULL' then
                begin
                  lMax := 0;
                  for j:=0 to TJvSimpleXml(FTables[0]).Root.Items.Count-1 do
                    lMax := Max(lMax, TJvSimpleXml(FTables[0]).Root.Items[j].Properties.IntValue(FParser.Columns[i].Name, 0));
                  inc(lMax);
                  lValue := IntToStr(lMax);
                  FLastId := lMax;
                end
                else if lValue = 'NOW' then
                  lValue := DateTimeToStr(Now)
                else if lValue = 'DATE' then
                  lValue := DateToStr(Now)
                else if lValue = 'TIME' then
                  lValue := TimeToStr(Now);
                Properties.Add(FParser.Columns[i].Name, lValue);
              end;
        FDatabase.SaveTables;
      end;
  end;

  FParser.OrderTable(FResults);
  FParser.LimitTable(FResults);
end;
{**********************************************************************}

{ TJvXmlQueryParser }

{**********************************************************************}
function TJvXmlQueryParser.CheckConditions(
  AXmlElem: TJvSimpleXmlElem): Boolean;
var
 i: Integer;

  function CheckCondition(var AIndex: Integer): Boolean;
  var
   lComp: TJvXmlSQLOperator;
   lValue, lValue2: string;
   lDate: TDateTime;
  begin
    result := true;
    while (AIndex < FConditions.Count) do
    begin
      with TJvXmlQueryCondition(FConditions[AIndex]) do
        case Operator of
          opLeftParenthesis:
            begin
              inc(AIndex);
              result := result and (CheckCondition(AIndex));
            end;
          opRightParenthesis:
            Exit;
          opNot:
            begin
              inc(AIndex);
              result := result and (not CheckCondition(AIndex));
            end;
          opColumn, opConstant:
            begin
              if Operator = opConstant then
                lValue := Condition
              else
              begin
                //ShowMessage(Condition); //Debug only
                if (Condition='daysbetweennow') then
                begin
                  inc(AIndex, 2); // (
                  lValue := AXmlElem.Properties.Value(TJvXmlQueryCondition(FConditions[AIndex]).Condition);
                  inc(AIndex); //)
                  lDate := StrToDateTimeDef(lValue, 0);
                  lValue := IntToStr(DaysBetween(Now, lDate));
                  if lDate<Now then
                    lValue := '-' + lValue;
                end
                else
                  lValue := AXmlElem.Properties.Value(Condition);
              end;
              inc(AIndex, 2);
              if not (AIndex < FConditions.Count) then
              begin
                result := false;
                Exit;
              end;
              lComp := TJvXmlQueryCondition(FConditions[AIndex - 1]).Operator;

              if TJvXmlQueryCondition(FConditions[AIndex]).Operator = opConstant then
                lValue2 := TJvXmlQueryCondition(FConditions[AIndex]).Condition
              else if TJvXmlQueryCondition(FConditions[AIndex]).Operator = opColumn then
              begin
                lValue2 := TJvXmlQueryCondition(FConditions[AIndex]).Condition;
                if AXmlElem.Properties.ItemNamed[lValue2]<>nil then
                  lValue2 := AXmlElem.Properties.Value(lValue2);
              end
              else if (TJvXmlQueryCondition(FConditions[AIndex]).Operator = opNull) and (lComp = opEquals) then
              begin
                result := result and (lValue = '');
                lComp := opNone;
              end
              else
              begin
                result := false;
                lComp := opNone;
              end;

              try
                case lComp of
                  opEquals:
                    result := result and (lValue = lValue2);
                  opGreater:
                    result := result and (StrToFloat(lValue) > StrToFloat(lValue2));
                  opSmaller:
                    result := result and (StrToFloat(lValue) < StrToFloat(lValue2));
                  opGreaterEquals:
                    result := result and (StrToFloat(lValue) >= StrToFloat(lValue2));
                  opSmallerEquals:
                    result := result and (StrToFloat(lValue) <= StrToFloat(lValue2));
                  opLike:
                    begin
                      //Not implemented yet
                    end;
                end;
              except
                result := false;
              end;
            end;
          opOr:
            begin
              inc(AIndex);
              result := result or CheckCondition(AIndex);
            end;
          opAnd:
            begin
              inc(AIndex);
              result := result and CheckCondition(AIndex);
            end;
          opXor:
            begin
              inc(AIndex);
              result := result xor CheckCondition(AIndex);
            end;
        end;
      inc(AIndex);
    end;
  end;

begin
  i := 0;
  result := CheckCondition(i);
end;
{**********************************************************************}
constructor TJvXmlQueryParser.Create;
begin
  FTables := TObjectList.Create;
  FColumns := TObjectList.Create;
  FConditions := TObjectList.Create;
  FOrders := TObjectList.Create;
  FUpdates := TObjectList.Create;
  FValues := TStringList.Create;
  FLimitBegin := 0;
  FLimitCount := MAXINT;
end;
{**********************************************************************}
destructor TJvXmlQueryParser.Destroy;
begin
  FTables.Free;
  FColumns.Free;
  FConditions.Free;
  FOrders.Free;
  FUpdates.Free;
  FValues.Free;
  inherited;
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateColumns;
var
 i: Integer;
 lColumn: TJvXmlQueryColumn;
begin
  i := pos(',', FColumnsStr);
  repeat
    if i<>0 then
    begin
      lColumn := TJvXmlQueryColumn.Create(Trim(Copy(FColumnsStr,1,i-1)));
      FColumns.Add(lColumn);
      FColumnsStr := Trim(Copy(FColumnsStr, i+1, MAXINT));
      i := pos(',', FColumnsStr);
    end
    else if FColumnsStr<>'' then
    begin
      lColumn := TJvXmlQueryColumn.Create(Trim(FColumnsStr));
      FColumns.Add(lColumn);
      FColumnsStr := '';
    end;
  until FColumnsStr = '';
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateInstruction;
begin
  FInstructionStr := UpperCase(FInstructionStr);

  if FInstructionStr = 'SELECT' then
    FInstruction := xiSelect
  else if FInstructionStr = 'UPDATE' then
    FInstruction := xiUpdate
  else if FInstructionStr = 'INSERT' then
    FInstruction := xiInsert
  else if FInstructionStr = 'DELETE' then
    FInstruction := xiDelete
  else
    raise TJvXmlDatabaseException.Create(Format(RS_UNKNOWNINST,[FInstructionStr]));
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateOrderBy;
var
 i: Integer;
 lOrder: TJvXmlQueryOrder;
begin
  FOrderStr := Trim(UpperCase(FOrderStr));
  i := pos(' ', FOrderStr);
  if i<>0 then
    FOrderStr := Trim(Copy(FOrderStr, i+1, MAXINT));

  i := pos(',', FOrderStr);
  repeat
    if i<>0 then
    begin
      lOrder := TJvXmlQueryOrder.Create(Trim(Copy(FOrderStr,1,i-1)));
      FOrders.Add(lOrder);
      FOrderStr := Trim(Copy(FOrderStr, i+1, MAXINT));
      i := pos(',', FOrderStr);
    end
    else if FOrderStr<>'' then
    begin
      lOrder := TJvXmlQueryOrder.Create(Trim(FOrderStr));
      FOrders.Add(lOrder);
      FOrderStr := '';
    end;
  until FOrderStr = '';
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateSet;
var
 i: Integer;
 lSet: TJvXmlQueryAssignement;
begin
  FSetStr := Trim(FSetStr);
  i := pos(',', FSetStr);
  repeat
    if i<>0 then
    begin
      lSet := TJvXmlQueryAssignement.Create(Trim(Copy(FSetStr,1,i-1)));
      FUpdates.Add(lSet);
      FSetStr := Trim(Copy(FSetStr, i+1, MAXINT));
      i := pos(',', FSetStr);
    end
    else if FSetStr<>'' then
    begin
      lSet := TJvXmlQueryAssignement.Create(Trim(FSetStr));
      FUpdates.Add(lSet);
      FSetStr := '';
    end;
  until FSetStr = '';
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateTables;
var
 i: Integer;
 lTable: TJvXmlQueryTable;
begin
  i := pos(',', FTablesStr);
  repeat
    if i<>0 then
    begin
      lTable := TJvXmlQueryTable.Create(Trim(Copy(FTablesStr,1,i-1)));
      FTables.Add(lTable);
      FTablesStr := Trim(Copy(FTablesStr, i+1, MAXINT));
      i := pos(',', FTablesStr);
    end
    else if FTablesStr<>'' then
    begin
      lTable := TJvXmlQueryTable.Create(Trim(FTablesStr));
      FTables.Add(lTable);
      FTablesStr := '';
    end;
  until FTablesStr = '';
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateValues;
var
 i: Integer;

  function ParseValue(AValue: string): string;
  begin
    result := Trim(AValue);

    //Escape quotes
    if (result<>'') and (result[1] in ['''','"']) then
      result := Copy(result, 2, Length(result) - 2);

    AValue := UpperCase(AValue);
    if AValue = 'NOW' then
      result := DateTimeToStr(Now);
  end;

begin
  i := pos(',', FValuesStr);
  repeat
    if i<>0 then
    begin
      FValues.Add(ParseValue(Trim(Copy(FValuesStr,1,i-1))));
      FValuesStr := Trim(Copy(FValuesStr, i+1, MAXINT));
      i := pos(',', FValuesStr);
    end
    else if FValuesStr<>'' then
    begin
      FValues.Add(ParseValue(Trim(FValuesStr)));
      FValuesStr := '';
    end;
  until FValuesStr = '';
end;
{**********************************************************************}
procedure TJvXmlQueryParser.DoValidateWhere;
var
 lToken: string;
 i: Integer;
 lChar: char;

  procedure AddToken(AToken: string);
  begin
    lToken := LowerCase(lToken);

    if (lToken = 'and') then
      FConditions.Add(TJvXmlQueryCondition.Create(opAnd))
    else if (lToken = 'or') then
      FConditions.Add(TJvXmlQueryCondition.Create(opOr))
    else if (lToken = 'like') then
      FConditions.Add(TJvXmlQueryCondition.Create(opLike))
    else if (lToken = 'xor') then
      FConditions.Add(TJvXmlQueryCondition.Create(opXor))
    else if (lToken = 'is') then
      FConditions.Add(TJvXmlQueryCondition.Create(opEquals))
    else if (lToken = 'null') then
      FConditions.Add(TJvXmlQueryCondition.Create(opNull))
    else
      FConditions.Add(TJvXmlQueryCondition.Create(opColumn,lToken));
  end;

begin
  FWhereStr := FWhereStr + ' ';
  i := 1;
  lToken := '';
  while i < Length(FWhereStr) do
  begin
    case FWhereStr[i] of
      '(':
        begin
          if lToken<>'' then
          begin
            AddToken(lToken);
            lToken := '';
          end;
          FConditions.Add(TJvXmlQueryCondition.Create(opLeftParenthesis));
        end;
      ')':
        begin
          if lToken<>'' then
          begin
            AddToken(lToken);
            lToken := '';
          end;
          FConditions.Add(TJvXmlQueryCondition.Create(opRightParenthesis));
        end;
      'a'..'z', 'A'..'Z', '0'..'9', '_':
        lToken := lToken + FWhereStr[i];
      ' ':
        if lToken <> '' then
        begin
          AddToken(lToken);
          lToken := '';
        end;
      '=':
        FConditions.Add(TJvXmlQueryCondition.Create(opEquals));
      '>':
        begin
          inc(i);
          if i<Length(FWhereStr) then
          begin
            if FWhereStr[i] = '=' then
              FConditions.Add(TJvXmlQueryCondition.Create(opGreaterEquals))
            else
            begin
              FConditions.Add(TJvXmlQueryCondition.Create(opGreater));
              dec(i);
            end;
          end;
        end;
      '<':
        begin
          inc(i);
          if i<Length(FWhereStr) then
          begin
            if FWhereStr[i] = '=' then
              FConditions.Add(TJvXmlQueryCondition.Create(opSmallerEquals))
            else
            begin
              FConditions.Add(TJvXmlQueryCondition.Create(opSmaller));
              dec(i);
            end;
          end;
        end;
      '''','"':
        begin
          lChar := FWhereStr[i];
          inc(i);
          lToken := '';
          while (i < Length(FWhereStr)) and (FWhereStr[i]<>lChar) do
          begin
            lToken := lToken + FWhereStr[i];
            inc(i);
          end;
          FConditions.Add(TJvXmlQueryCondition.Create(opConstant,lToken));
          lToken := '';
        end;
    end;
    inc(i);
  end;
end;
{**********************************************************************}
function TJvXmlQueryParser.GetColumn(
  const AIndex: Integer): TJvXmlQueryColumn;
begin
  result := TJvXmlQueryColumn(FColumns[AIndex]);
end;
{**********************************************************************}
function TJvXmlQueryParser.GetColumnsCount: Integer;
begin
  result := FColumns.Count;
end;
{**********************************************************************}
function TJvXmlQueryParser.GetCondition(
  const AIndex: Integer): TJvXmlQueryCondition;
begin
  result := TJvXmlQueryCondition(FConditions[AIndex]);
end;
{**********************************************************************}
function TJvXmlQueryParser.GetConditionsCount: Integer;
begin
  result := FConditions.Count;
end;
{**********************************************************************}
function TJvXmlQueryParser.GetTable(
  const AIndex: Integer): TJvXmlQueryTable;
begin
  result := TJvXmlQueryTable(FTables[AIndex]);
end;
{**********************************************************************}
function TJvXmlQueryParser.GetTablesCount: Integer;
begin
  result := FTables.Count;
end;
{**********************************************************************}
function TJvXmlQueryParser.GetValue(const AIndex: Integer): string;
begin
  result := FValues[AIndex];
end;
{**********************************************************************}
function TJvXmlQueryParser.GetValuesCount: Integer;
begin
  result := FValues.Count;
end;
{**********************************************************************}
procedure TJvXmlQueryParser.LimitTable(var ATable: TJvSimpleXmlElem);
begin
  while (FLimitBegin > 0) and (ATable.Items.Count > 0) do
  begin
    ATable.Items.Delete(0);
    dec(FLimitBegin);
  end;
  while (ATable.Items.Count > FLimitCount) do
    ATable.Items.Delete(ATable.Items.Count - 1);
end;
{**********************************************************************}
function TJvXmlQueryParser.OrderCallBack(Elems: TJvSimpleXmlElems; Index1,
  Index2: Integer): Integer;
var
 i: Integer;
 lStr1, lStr2: string;
 lFloat1, lFloat2: Double;
begin
  result := 0;

  for i:=0 to FOrders.Count-1 do
  begin
    lStr1 := FOrderTable.Items[Index1].Properties.Value(TJvXmlQueryOrder(FOrders[i]).Column);
    lStr2 := FOrderTable.Items[Index2].Properties.Value(TJvXmlQueryOrder(FOrders[i]).Column);
    if lStr1 <> lStr2 then
    begin
      //convert to date/int
      case TJvXmlQueryOrder(FOrders[i]).Convertion of
        ocNone:
          result := CompareStr(lStr1, lStr2);
        ocDate:
          result := CompareDateTime(StrToDateTimeDef(lStr1, 0), StrToDateTimeDef(lStr2, 0));
        ocInteger:
          result := StrToIntDef(lStr1, 0) - StrToIntDef(lStr2, 0);
        ocFloat:
          begin
            lFloat1 := StrToFloatDef(lStr1, 0);
            lFloat2 := StrToFloatDef(lStr2, 0);
            if lFloat1 > lFloat2 then
              result := 1
            else if lFloat1 < lFloat2 then
              result := -1;
          end;
      end;

      if not TJvXmlQueryOrder(FOrders[i]).Ascending then
        result := - result;
      Exit;
    end;
  end;
end;
{**********************************************************************}
procedure TJvXmlQueryParser.OrderTable(var ATable: TJvSimpleXmlElem);
begin
  FOrderTable := ATable;
  ATable.Items.CustomSort(OrderCallBack);
end;
{**********************************************************************}
procedure TJvXmlQueryParser.Parse(AQuery: string);
var
 st: string;
 lStatements: array of string;
 i,j: Integer;
begin
  FQuery := AQuery;

  FInstructionStr := ReadToken;
  DoValidateInstruction;

  case Instruction of
    xiSelect:
      begin
        st := ReadColumns(['FROM', 'WHERE', 'ORDER', 'LIMIT'], false);
        SetLength(lStatements, 4);
        lStatements[0] := 'FROM';
        lStatements[1] := 'WHERE';
        lStatements[2] := 'ORDER';
        lStatements[3] := 'LIMIT';
      end;

    xiDelete:
      begin
        ReadToken; //pass the FROM keyword
        st := 'FROM';
        SetLength(lStatements, 2);
        lStatements[0] := 'FROM';
        lStatements[1] := 'WHERE';
      end;

    xiUpdate:
      begin
        st := 'FROM';
        SetLength(lStatements, 3);
        lStatements[0] := 'FROM';
        lStatements[1] := 'SET';
        lStatements[2] := 'WHERE';
      end;

    xiInsert:
      begin
        st := 'FROM';
        SetLength(lStatements, 3);
        lStatements[0] := 'FROM';
        lStatements[1] := 'VALUES';
        lStatements[2] := 'COLUMNS';
        ReadToken; // Pass the into statement 

        //Modify query for lightness of parser
        //INSERT INTO file.xml(Col1, Col2) VALUES(val1, val2)
        // into
        //INSERT INTO file.xml COLUMNS col1, col2 VALUES val1, val2
        FQuery := StringReplace(FQuery, '()', '', [rfReplaceAll]);
        FQuery := StringReplace(FQuery, ')', ' ', [rfReplaceAll]);
        FQuery := StringReplace(FQuery, '(', ' COLUMNS ', []);
        FQuery := StringReplace(FQuery, '(', ' ', []);
      end;
  end;

  while (st<>'') do
  begin
    j := -1;
    for i:=0 to Length(lStatements)-1 do
      if lStatements[i] = st then
      begin
        lStatements[i] := ''; //Do not accept it anymore
        j := i;
        Break;
      end;
    if j=-1 then
      raise TJvXmlDatabaseException.Create(Format(RS_UNEXPECTEDINST,[st]));

    if st = 'FROM' then
      st := ReadTables(lStatements)
    else if st = 'WHERE' then
      st := ReadWhere(lStatements)
    else if st = 'LIMIT' then
      st := ReadLimit(lStatements)
    else if st = 'ORDER' then
      st := ReadOrderBy(lStatements)
    else if st = 'SET' then
      st := ReadSet(lStatements)
    else if st = 'VALUES' then
      st := ReadValues(lStatements)
    else if st = 'COLUMNS' then
      st := ReadColumns(lStatements, false);
  end;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadColumns(
  AEndStatement: array of string; ACanTerminate: Boolean): string;
begin
  result := ReadStatement(AEndStatement, ACanTerminate, FColumnsStr);
  DoValidateColumns;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadLimit(
  AEndStatement: array of string): string;
var
 i: Integer;
begin
  result := ReadStatement(AEndStatement, true, FLimitStr);
  i := pos(',', FLimitStr);
  if i = 0 then
    FLimitCount := StrToIntDef(FLimitStr, MAXINT)
  else
  begin
    FLimitCount := StrToIntDef(Trim(Copy(FLimitStr, i+1, MAXINT)), MAXINT);
    FLimitBegin := StrToIntDef(Trim(Copy(FLimitStr, 1, i-1)), 0);
  end;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadOrderBy(
  AEndStatement: array of string): string;
begin
  result := ReadStatement(AEndStatement, true, FOrderStr);
  DoValidateOrderBy;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadSet(AEndStatement: array of string): string;
begin
  result := ReadStatement(AEndStatement, true, FSetStr);
  DoValidateSet;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadStatement(AEndStatement: array of string;
  ACanTerminate: Boolean; var AValue: string): string;
var
 st: string;
 lFound: Boolean;
 i: Integer;
begin
  AValue := '';
  lFound := false;
  result := '';
  while not lFound do
    if (FQuery = '') and (ACanTerminate) then
      lFound := true
    else
    begin
      st := ReadToken;
      if st<>'' then
        for i:=0 to Length(AEndStatement)-1 do
          if UpperCase(st) = AEndStatement[i] then
          begin
            lFound := true;
            Break
          end;

      if not lFound then
        AValue := AValue + ' ' + st
      else
        result := st;
    end;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadTables(
  AEndStatement: array of string): string;
begin
  result := ReadStatement(AEndStatement, true, FTablesStr);
  DoValidateTables;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadToken: string;
var
 i: Integer;
begin
  if FQuery='' then
    raise TJvXmlDatabaseException.Create(RS_UNEXPECTEDEND);

  FQuery := TrimLeft(FQuery);
  i := 1;
  while (i<Length(FQuery)) and not(FQuery[i] in [' '{,'(',')'}]) do
    inc(i);
  if i>=Length(FQuery) then
  begin
    result := Trim(FQuery);
    FQuery := '';
  end
  else
  begin
    result := Copy(FQuery, 1, i-1);
    FQuery := Copy(FQuery, i+1, MAXINT);
  end;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadValues(
  AEndStatement: array of string): string;
begin
  result := ReadStatement(AEndStatement, true, FValuesStr);
  DoValidateValues;
end;
{**********************************************************************}
function TJvXmlQueryParser.ReadWhere(
  AEndStatement: array of string): string;
begin
  result := ReadStatement(AEndStatement, true, FWhereStr);
  DoValidateWhere;
end;
{**********************************************************************}
procedure TJvXmlQueryParser.UpdateRow(ARow: TJvSimpleXmlElem);
var
 i: Integer;
begin
  for i:=0 to FUpdates.Count-1 do
    TJvXmlQueryAssignement(FUpdates[i]).UpdateElem(ARow);
end;
{**********************************************************************}

{ TJvXmlQueryColumn }

{**********************************************************************}
constructor TJvXmlQueryColumn.Create(AValue: string);
var
 i: Integer;
begin
  i := Pos('.', AValue);
  if i<>0 then
  begin
    Name := Copy(AValue, i+1, MAXINT);
    Table := Copy(AValue, 1, i-1);
  end
  else
    Name := AValue;
end;
{**********************************************************************}

{ TJvXmlQueryTable }

{**********************************************************************}
constructor TJvXmlQueryTable.Create(AValue: string);
var
 i: Integer;
begin
  i := Pos(' ', AValue);
  if i<>0 then
  begin
    Name := Copy(AValue, 1, i-1);
    Alias := Trim(Copy(AValue, i+1, MAXINT));
  end
  else
    Name := AValue;
end;
{**********************************************************************}

{ TJvXmlQueryCondition }

{**********************************************************************}
constructor TJvXmlQueryCondition.Create(AOperator: TJvXmlSQLOperator;
  ACondition: string);
begin
  self.Operator := AOperator;
  self.Condition := ACondition;
end;
{**********************************************************************}

{ TJvXmlQueryOrder }

{**********************************************************************}
constructor TJvXmlQueryOrder.Create(AValue: string);
var
 i: Integer;
 st: string;
begin
  Column := Trim(AValue);
  Ascending := true;
  Convertion := ocNone;

  i := pos(' ', Column);
  if i<>0 then
  begin
    SetLength(Column, i-1);
    Ascending := pos('ASC', UpperCase(AValue)) <> 0;
  end;

  i := pos('(', Column);
  if i<>0 then
  begin
    st := UpperCase(Copy(Column, 1, i-1));
    Column := Copy(Column, i+1, MAXINT);
    SetLength(Column, Length(Column) - 1);

    if st = 'DATE' then
      Convertion := ocDate
    else if (st = 'INTEGER') or (st = 'INT') then
      Convertion := ocInteger
    else if (st = 'FLOAT') then
      Convertion := ocFloat;
  end;
end;
{**********************************************************************}


{ TJvXmlQueryAssignement }

{**********************************************************************}
constructor TJvXmlQueryAssignement.Create(AValue: string);
var
 i,j: Integer;
 lDelimiter: Char;
begin
  i := pos('=', AValue);
  if i=0 then
    raise Exception.Create('')
  else
  begin
    Column := Trim(Copy(AValue, 1, i-1));
    AValue := Trim(Copy(AValue, i+1, MAXINT));

    if AValue = '' then
      raise Exception.Create('');

    //Determine if column or constant
    if (AValue[1] = '"') or (AValue[1] = '''') then
    begin
      lDelimiter := AValue[1];
      ValueKind := skConstant;
      AValue := Copy(AValue, 2, MAXINT);
      i := 0;
      for j:=1 to Length(AValue) do
        if AValue[j] = lDelimiter then
          if (j=1) or (AValue[j-1] <> '\') then
          begin
            i := j;
            Break;
          end;
      if i<>0 then
      begin
        Value := Copy(AValue, 1, i-1);
        Value := StringReplace(Value, '\' + lDelimiter, lDelimiter, [rfReplaceAll]);
        AValue := Trim(Copy(AValue, i+1, MAXINT));
      end
      else
        raise Exception.Create('');        
    end
    else
    begin
      ValueKind := skColumn;
      i := pos(' ', AValue);
      if i=0 then
      begin
        Value := AValue;
        AValue := '';
      end
      else
      begin
        Value := Copy(AValue, 1, i-1);
        AValue := Trim(Copy(AValue, i+1, MAXINT));
      end;
    end;

    //Second kind and second value?
    if AValue = '' then
      Operator := soNone
    else
    begin
      case AValue[1] of
        '+': Operator := soAdd;
        '-': Operator := soSubstract;
        '*': Operator := soMultiply;
        '/': Operator := soDivide;
        else
          raise Exception.Create('');
      end;

      SecondValue := Trim(Copy(AValue, 2, MAXINT));
      if (SecondValue<>'') and (SecondValue[1] in ['''','"']) then
      begin
        SecondValue := Copy(SecondValue, 2, Length(SecondValue) - 2);
        SecondKind := skConstant;
      end
      else
        SecondKind := skColumn;
    end;
  end;
end;
{**********************************************************************}
procedure TJvXmlQueryAssignement.UpdateElem(AElement: TJvSimpleXmlElem);
var
 lValue, lValue2: string;

  function ParseValue(AValue: string): string;
  begin
    result := AValue;
    AValue := UpperCase(AValue);
    if AValue = 'NOW()' then
      result := DateTimeToStr(Now);
  end;

begin
  if ValueKind = skConstant then
    lValue := Value
  else
    lValue := AElement.Properties.Value(Value, ParseValue(Value));

  if Operator <> soNone then
  begin
    if SecondKind = skConstant then
      lValue2 := SecondValue
    else
      lValue2 := AElement.Properties.Value(SecondValue, ParseValue(SecondValue));
    case Operator of
      soAdd:
        lValue := FloatToStr(StrToFloatDef(lValue,0) + StrToFloatDef(lValue2,0));
      soMultiply:
        lValue := FloatToStr(StrToFloatDef(lValue,0) * StrToFloatDef(lValue2,0));
      soDivide:
        lValue := FloatToStr(StrToFloatDef(lValue,0) / StrToFloatDef(lValue2,0));
      soSubstract:
        lValue := FloatToStr(StrToFloatDef(lValue,0) - StrToFloatDef(lValue2,0));
    end;
  end;

  AElement.Properties.Delete(Column);
  AElement.Properties.Add(Column, lValue);
end;
{**********************************************************************}

end.


