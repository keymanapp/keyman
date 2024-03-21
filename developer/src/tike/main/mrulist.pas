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
  private
    FMRU: TStrings;
    FMRUName: string;
    FOnChange: TNotifyEvent;
    function GetFile(Index: Integer): string;
    function GetFileCount: Integer;
    procedure Change;
    procedure Save;
  public
    constructor Create(AMRUName: string);
    destructor Destroy; override;
    procedure Load;
    procedure Delete(FileName: string);
    procedure Add(FileName: string);
    procedure Append(FileName: string);
    procedure Open(FileName: string);
    procedure SaveListToXML(FileName: string);
    property FileCount: Integer read GetFileCount;
    property Files[Index: Integer]: string read GetFile;
    function EllipsisFile(Index: Integer): string;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function EllipsisFile(filename: string): string;

implementation

uses
  System.SysUtils,
  System.Win.ComObj,
  Winapi.ShlWapi,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  ErrorControlledRegistry,
  RegistryKeys;

const HRESULT_FROM_WIN32 = HRESULT($80070000);
const E_SHARING_VIOLATION: HRESULT = HRESULT(HRESULT_FROM_WIN32 or ERROR_SHARING_VIOLATION);

{ TMRUList }

procedure TMRUList.Change;
begin
  Save;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TMRUList.Create(AMRUName: string);
begin
  inherited Create;
  FMRUName := AMRUName;
  FMRU := TStringList.Create;
end;

procedure TMRUList.Load;
var
  s: TStringList;
  i: Integer;
  name: string;
  reg: TRegistryErrorControlled;
  FNewMRU: TStringList;
begin
  if FMRUName = '' then
    Exit;

  FNewMRU := TStringList.Create;
  try
    reg := TRegistryErrorControlled.Create;
    try
      if reg.OpenKey(SRegKey_IDEFiles_CU, True) and
        reg.OpenKey(FMRUName, True) then
      begin
        s := TStringList.Create;
        try
          reg.GetValueNames(s);
          s.Sort;
          for i := 0 to s.Count - 1 do
          begin
            name := reg.ReadString(s[i]);
            if FileExists(name) and (FNewMRU.IndexOf(name) < 0) then
              FNewMRU.Add(name);
          end;
        finally
          s.Free;
        end;
      end;
    finally
      reg.Free;
    end;

    if FNewMRU.Text = FMRU.Text then
    begin
      // No changes to MRU
      Exit;
    end;

    FMRU.Text := FNewMRU.Text;
  finally
    FNewMRU.Free;
  end;

  if FMRU.Count > 9 then
  begin
    while FMRU.Count > 9 do
      FMRU.Delete(9);
    Change;
  end
  else if Assigned(FOnChange) then
  begin
    // Don't use 'Change' here because we just loaded, and don't need to 'Save'
    // as well as notify
    FOnChange(Self);
  end;
end;

procedure TMRUList.Add(FileName: string);
var
  n: Integer;
begin
  if not FileExists(FileName) then
    Exit;

  n := FMRU.IndexOf(FileName);

  if n = 0 then Exit;

  if n > 0
    then FMRU.Move(n, 0)
    else FMRU.Insert(0, FileName);
  while FMRU.Count > 9 do FMRU.Delete(9);
  Change;
end;

procedure TMRUList.Append(FileName: string);
var
  n: Integer;
begin
  if not FileExists(FileName) then
    Exit;

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

procedure TMRUList.Delete(FileName: string);
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

procedure TMRUList.Open(FileName: string);
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

procedure TMRUList.Save;
var
  i: Integer;
begin
  if FMRUName = '' then Exit;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_IDEFiles_CU, True) then
    begin
      if KeyExists(FMRUName) then DeleteKey(FMRUName);
      if OpenKey(FMRUName, True) then
        for i := 0 to FMRU.Count - 1 do
          WriteString('File'+IntToStr(i), FMRU[i]);
    end;
  finally
    Free;
  end;
end;

destructor TMRUList.Destroy;
begin
  FMRU.Free;
  inherited Destroy;
end;

function TMRUList.EllipsisFile(Index: Integer): string;
begin
  Result := mrulist.EllipsisFile(Files[Index]);
end;

function EllipsisFile(filename: string): string;
var
  buffer: array[0..MAX_PATH] of char;
begin
  StrPCopy(buffer, filename);

  if PathCompactPath(0, buffer, GetSystemMetrics(SM_CXSCREEN) div 3) then   // I4697
    Result := buffer  // This removes the terminating nul
  else
    Result := ExtractFileName(filename);
end;

function TMRUList.GetFile(Index: Integer): string;
begin
  Result := FMRU[Index];
end;

function TMRUList.GetFileCount: Integer;
begin
  Result := FMRU.Count;
end;

procedure TMRUList.SaveListToXML(FileName: string);
var
  i: Integer;
  doc: IXMLDocument;
  node: IXMLNode;
begin
  // Save MRU to an XML file, used by global welcome page
  // This is a mostly a duplicate of part of ProjectSaver and
  // could be refactored, but there is not much gain in doing
  // so.

  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];
  doc.Encoding := 'utf-8';

  node := doc.CreateElement('MRU', '');
  doc.DocumentElement := node;

  for i := 0 to FileCount - 1 do
  begin
    with node.AddChild('File') do
    begin
      AddChild('ID').NodeValue := 'id_MRU'+IntToStr(i);
      AddChild('Filename').NodeValue := ExtractFileName(Files[i]);
      AddChild('FileType').NodeValue := ExtractFileExt(Files[i]);
      AddChild('FullPath').NodeValue := Files[i];
    end;
  end;

  try
    doc.SaveToFile(FileName);
  except
    on E:EOleException do
    begin
      if E.ErrorCode = E_SHARING_VIOLATION then
      begin
        // Another process is also writing the mru list; we'll let it
        // slide as we'll usually get another chance later
        Exit;
      end
      else
        // Another error, let's capture it
        raise;
    end;
  end;
end;

end.

