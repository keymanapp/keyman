(*
  Name:             mrulist
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    5 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Delete a file from MRU
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - I1420 - Improve project performance
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    05 May 2015 - mcdurdin - I4697 - V9.0 - Remove VCL dependency for mrulist
*)
unit mrulist;  // I3306

interface

uses
  System.Classes;

type
  TMRUList = class
    FMRU: TStrings;
  private
    FMRUName: WideString;
    FOnChange: TNotifyEvent;
    function GetFile(Index: Integer): WideString;
    function GetFileCount: Integer;
    procedure Change;
  public
    constructor Create(AMRUName: WideString);
    destructor Destroy; override;
    procedure Delete(FileName: WideString);
    procedure Add(FileName: WideString);
    procedure Append(FileName: WideString);
    procedure Open(FileName: WideString);
    property FileCount: Integer read GetFileCount;
    property Files[Index: Integer]: WideString read GetFile;
    function EllipsisFile(Index: Integer): WideString;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  System.SysUtils,
  Winapi.ShlWapi,
  Winapi.Windows,

  ErrorControlledRegistry,
  RegistryKeys;

{ TMRUList }

procedure TMRUList.Add(FileName: WideString);
var
  n: Integer;
begin
  n := FMRU.IndexOf(FileName);

  if n = 0 then Exit;

  if n > 0
    then FMRU.Move(n, 0)
    else FMRU.Insert(0, FileName);
  while FMRU.Count > 9 do FMRU.Delete(9);
  Change;
end;

procedure TMRUList.Append(FileName: WideString);
var
  n: Integer;
begin
  n := FMRU.IndexOf(FileName);
  if n >= 0 then
  begin
    FMRU.Move(n, FMRU.Count-1);
    Change;
  end
  else if FMRU.Count < 9 then
  begin
    FMRU.Add(FileName);
    Change;
  end;
end;

procedure TMRUList.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TMRUList.Create(AMRUName: WideString);
var
  s: TStringList;
  i: Integer;
begin
  inherited Create;
  FMRUName := AMRUName;
  FMRU := TStringList.Create;
  if FMRUName <> '' then
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_IDEFiles) then
        if OpenKeyReadOnly(FMRUName) then
        begin
          s := TStringList.Create;
          try
            GetValueNames(s);
            s.Sort;
            for i := 0 to s.Count - 1 do
              FMRU.Add(ReadString(s[i]))
          finally
            s.Free;
          end;
        end;
    finally
      Free;
    end;
    while FMRU.Count > 9 do FMRU.Delete(9);
  end;
end;

procedure TMRUList.Delete(FileName: WideString);
var
  n: Integer;
begin
  n := FMRU.IndexOf(FileName);
  if n >= 0 then
  begin
    FMRU.Delete(n);
    Change;
  end;
end;

destructor TMRUList.Destroy;
var
  i: Integer;
begin
  if FMRUName <> '' then
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKey(SRegKey_IDEFiles, True) then
      begin
        if KeyExists(FMRUName) then DeleteKey(FMRUName);
        if OpenKey(FMRUName, True) then
          for i := 0 to FMRU.Count - 1 do
            WriteString('File'+IntToStr(i), FMRU[i]);
      end;
    finally
      Free;
    end;
  FMRU.Free;
  inherited Destroy;
end;

function TMRUList.EllipsisFile(Index: Integer): WideString;
begin
  Result := Files[Index];

  if not PathCompactPath(0, PWideChar(Result), GetSystemMetrics(SM_CXSCREEN) div 3) then   // I4697
    Result := ExtractFileName(Files[Index]);
end;

function TMRUList.GetFile(Index: Integer): WideString;
begin
  Result := FMRU[Index];
end;

function TMRUList.GetFileCount: Integer;
begin
  Result := FMRU.Count;
end;

procedure TMRUList.Open(FileName: WideString);
var
  n: Integer;
begin
  n := FMRU.IndexOf(FileName);
  if n >= 0 then
  begin
    FMRU.Move(n, 0);
    Change;
  end;
end;

end.
