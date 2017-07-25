unit ThreadedPasswords;


interface
uses JwaWindows, JwsclEncryption, JwsclTypes, JwsclExceptions, Math, ComObj, Classes;

type
  PPassEntry = ^TPassEntry;
  TPassEntry = record
    SessionIndex : DWORD;
    EDomain,
    EUserName,
    EPassword : Pointer;
    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
    Data : Pointer;
    SizeData : Cardinal;  
  end;



  TPasswordList = class(TThreadList)
  protected
    procedure FreeIndex(Index : Integer);
  public
    constructor Create;
    destructor Destroy; override;

    class function CreatePassEntry(const Session : DWORD;
      const Domain, UserName, Password : WideString) : PPassEntry;
    class procedure FreePassEntry(var P : PPassEntry);

    function Add(const Session : DWORD; const Domain, UserName, Password : WideString) : Integer;

    procedure GetByIndex(Index : Integer;
          out Domain, UserName, Password : WideString);
    procedure GetBySession(const Session : DWORD;
          out Domain, UserName, Password : WideString);

    procedure SetByIndex(Index : Integer;
          const Session : DWORD; const Domain, UserName, Password : WideString);
    procedure SetBySession(const Session : DWORD;
          const Domain, UserName, Password : WideString);

    procedure SetDataByIndex(Index : Integer;
              const Data : Pointer; const Size : Cardinal);
    procedure SetDataBySession(Session : Integer;
              const Data : Pointer; const Size : Cardinal);

    function SessionToIndex(const Session : DWORD) : Integer;
    function IndexToSession(const Index : Integer) : DWORD;


    function IsIndexValid(const Index : Integer) : Boolean;
    function IsSessionValid(const Session : Integer) : Boolean;

    procedure DeleteByIndex(Index : Integer);
    procedure DeleteBySession(Session : Integer);

  end;


implementation

{ TPasswordList }

function TPasswordList.Add(const Session : DWORD; const Domain, UserName,
  Password: WideString): Integer;
begin
  if IsSessionValid(Session) then
    raise EJwsclDuplicateListEntryException.Create('');

  try
    result := LockList.Add(CreatePassEntry(Session, Domain, UserName, Password));
  finally
    UnlockList;
  end;
end;

constructor TPasswordList.Create;
begin
  inherited;
end;

class function TPasswordList.CreatePassEntry(const Session : DWORD;
  const Domain, UserName, Password: WideString): PPassEntry;
var P : PPassEntry;
    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
begin
  New(P);

  P^.SessionIndex := Session;

  SizeUser := (2+Length(UserName))*sizeof(WideChar);
  GetMem(P^.EUserName, SizeUser);
  SizeDomain := (2+Length(Domain))*sizeof(WideChar);
  GetMem(P^.EDomain, SizeDomain);
  SizePass := (2+Length(Password))*sizeof(WideChar);
  GetMem(P^.EPassword, SizePass);

  OleCheck(StringCbCopyW(P^.EUserName, SizeUser, PWideChar(UserName)));
  TJwEncryptMemory.EncryptMemory(P^.EUserName, SizeUser, [pmSameProcess],
      mtGetMem);

  OleCheck(StringCbCopyW(P^.EDomain, SizeDomain, PWideChar(Domain)));
  TJwEncryptMemory.EncryptMemory(P^.EDomain, SizeUser, [pmSameProcess],
      mtGetMem);

  OleCheck(StringCbCopyW(P^.EPassword, SizePass, PWideChar(Password)));
  TJwEncryptMemory.EncryptMemory(P^.EPassword, SizeUser, [pmSameProcess],
      mtGetMem);

  P^.SizeUser := SizeUser;
  P^.SizeDomain := SizeDomain;
  P^.SizePass := SizePass;

  result := P;
end;

procedure TPasswordList.DeleteByIndex(Index: Integer);
var P : PPassEntry;
    L : TList;
begin
 if not IsIndexValid(Index) then
   raise EJwsclInvalidIndex.Create('');

  L := LockList;
  try
    P := L.Items[Index];
    FreePassEntry(P);
    L.Items[Index] := nil;
    L.Delete(Index);
  finally 
    UnlockList;
  end;
end;

procedure TPasswordList.DeleteBySession(Session : Integer);
begin
  DeleteByIndex(SessionToIndex(Session));
end;

destructor TPasswordList.Destroy;
var L : TList;
    i : Integer;
begin
  L := LockList;
  try
    for i := L.Count-1 downto 0 do
    begin
      DeleteByIndex(i);
    end;
  finally
    UnlockList;
  end;
  Clear;

  inherited;
end;

procedure TPasswordList.FreeIndex(Index: Integer);
var L : TList;
    P: PPassEntry;
begin
  L := LockList;
  try
    P := L.Items[Index];
    FreePassEntry(P);
    L.Items[Index] := nil;
  finally
    UnlockList;
  end;
end;

class procedure TPasswordList.FreePassEntry(var P: PPassEntry);
begin
  if P <> nil then
  begin
    ZeroMemory(P^.EDomain, P^.SizeDomain);
    FreeMem(P^.EDomain);

    ZeroMemory(P^.EUserName, P^.SizeUser);
    FreeMem(P^.EUserName);

    ZeroMemory(P^.EPassword, P^.SizePass);
    FreeMem(P^.EPassword);
  end;
  P := nil;
end;

function TPasswordList.IndexToSession(const Index: Integer): DWORD;
var
  L : TList;
  i : Integer;
  P : PPassEntry;
begin
  result := INVALID_HANDLE_VALUE;

  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      P := L.Items[i];
      if P <> nil then
      begin
        result := P^.SessionIndex;
        exit;
      end;
    end;
  finally
    UnlockList;
  end;
end;

function TPasswordList.IsIndexValid(const Index : Integer) : Boolean;
var L : TList;
begin
  L := LockList;
  try
    result := (Index < L.Count) and (Index >= 0);
  finally
    UnlockList;
  end;
end;

function TPasswordList.IsSessionValid(const Session: Integer): Boolean;
var
  L : TList;
  i : Integer;
  P : PPassEntry;
begin
  result := false;

  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      P := L.Items[i];
      if P <> nil then
      begin
        if P^.SessionIndex = Session then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  finally
    UnlockList;
  end;
end;

function TPasswordList.SessionToIndex(const Session: DWORD): Integer;
var
  L : TList;
  i : Integer;
  P : PPassEntry;
begin
  result := INVALID_HANDLE_VALUE;

  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      P := L.Items[i];
      if (P <> nil) and (P^.SessionIndex = Session) then
      begin
        result := i;
        exit;
      end;
    end;
  finally
    UnlockList;
  end;
end;

procedure TPasswordList.SetByIndex(Index: Integer; const Session : DWORD; const Domain, UserName,
  Password: WideString);
var L : TList;
begin
  if not IsIndexValid(Index) then
    raise EJwsclInvalidIndex.Create('');

  FreeIndex(Index);
  try
    L.Items[Index] := CreatePassEntry(Session, Domain, UserName, Password);
  finally
    UnlockList;
  end;
end;

procedure TPasswordList.SetBySession(const Session: DWORD; const Domain,
  UserName, Password: WideString);
var
  L : TList;
  i : Integer;
  P : PPassEntry;
begin
  i := SessionToIndex(Session);
  if i <> INVALID_HANDLE_VALUE then
    SetByIndex(i, Session, Domain, UserName, Password)
  else
    Add(Session, Domain, UserName,Password);
end;

procedure TPasswordList.SetDataByIndex(Index: Integer; const Data: Pointer;
  const Size: Cardinal);
var P : PPassEntry;
    L : TList;

    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
    pData : Pointer;
begin
  if IsIndexValid(Index) then
    raise EJwsclInvalidIndex.Create('');

  L := LockList;
  try
    P := L.Items[Index];
    if P = nil then
      EJwsclInvalidPointerType.Create('');

    P^.SessionIndex := Size;
    GetMem(P^.Data, P^.SizeData);

    CopyMemory(P^.Data, Data, Size);
    TJwEncryptMemory.EncryptMemory(P^.Data, P^.SizeData, [pmSameProcess],
         mtGetMem);
  finally
    UnlockList;
  end;
end;

procedure TPasswordList.SetDataBySession(Session: Integer; const Data: Pointer;
  const Size: Cardinal);
begin
  SetDataByIndex(SessionToIndex(Session), Data, Size);
end;

procedure TPasswordList.GetByIndex(Index: Integer; out Domain, UserName,
  Password: WideString);
var P : PPassEntry;
    L : TList;
    Data : Pointer;

    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
begin
  L := LockList;
  try
    P := L.Items[Index];

    SizeUser := P^.SizeUser;
    SizeDomain := P^.SizeDomain;
    SizePass := P^.SizePass;

    if SizeUser > 0 then
    begin
      GetMem(Data, SizeUser);
      try
        CopyMemory(Data, P^.EUserName, SizeUser);
        TJwEncryptMemory.DecryptMemory(Data, SizeUser, [pmSameProcess], mtGetMem);
        SetLength(UserName, SizeUser div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@UserName[1]), SizeUser, Data));
      finally
        ZeroMemory(Data, SizeUser);
        FreeMem(Data);
      end;
    end;

    if SizeDomain > 0 then
    begin
      GetMem(Data, SizeDomain);
      try
        CopyMemory(Data, P^.EDomain, SizeDomain);
        TJwEncryptMemory.DecryptMemory(Data, SizeDomain, [pmSameProcess], mtGetMem);
        SetLength(Domain, SizeDomain div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@Domain[1]), SizeDomain, Data));
      finally
        ZeroMemory(Data, SizeDomain);
        FreeMem(Data);
      end;
    end;

    if SizePass > 0 then
    begin
      GetMem(Data, SizePass);
      try
        CopyMemory(Data, P^.EPassword, SizePass);
        TJwEncryptMemory.DecryptMemory(Data, SizePass, [pmSameProcess], mtGetMem);
        SetLength(Password, SizePass div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@Password[1]), SizePass, Data));
      finally
        ZeroMemory(Data, SizePass);
        FreeMem(Data);
      end;
    end;


  finally
    UnlockList;
  end;
end;

procedure TPasswordList.GetBySession(const Session: DWORD; out Domain, UserName,
  Password: WideString);
var
  L : TList;
  i : Integer;
  P : PPassEntry;
begin
  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      P := L.Items[i];
      if P <> nil then
      begin
        if P^.SessionIndex = Session then
        begin
          GetByIndex(I, Domain, UserName, Password);
          exit;
        end;
      end;
    end;
  finally
    UnlockList;
  end;
  raise EJwsclInvalidSession.Create('');
end;

end.
