unit mutation_test;

interface

uses
  Winapi.Windows,
  System.SysUtils;

procedure Run;

// Define this to see how the memory referenced by (C) is freed twice
{$DEFINE LOG_MEMORY_OPERATIONS}

type
  TMutatedMember = class
  private
    FMutatedMember: string;
    function DoMutation: string;
  public
    constructor Create;
    // Using register calling convention, we need 3 parameters. For
    // stdcall, only 2 parameters are required.
    procedure DoNothing(A, B, C: string);
  end;

implementation

{$IFDEF LOG_MEMORY_OPERATIONS}
procedure LogMemoryOperations; forward;
{$ENDIF}

constructor TMutatedMember.Create;
begin
  FMutatedMember := '0.0';
end;

function TMutatedMember.DoMutation: string;
begin
  FMutatedMember := '1.0';
  Result := '';
end;

procedure TMutatedMember.DoNothing(A, B, C: string);
begin
  // On exiting this function, C is freed a second time, which causes an
  // EInvalidPointer.

  // In this simple repro, if we were to reference C, then we wouldn't get an
  // exception. So to demonstrate the issue in a minimal way, we won't reference C.
  // In our more complex scenario, C is referenced in the function and still
  // causes the same double-free at function exit.

  // If we examine pstrrec(cardinal(C)-$c) in a debugger, we can see that refCnt
  // is 0 on entry (breakpoint on <begin>). Contrast to pstrrec(cardinal(B)-$c)
  // which shows a (correct) refCnt of 1.
end;

procedure Run;
var
  mm: TMutatedMember;
begin
{$IFDEF LOG_MEMORY_OPERATIONS}
  LogMemoryOperations;
{$ENDIF}

  mm := TMutatedMember.Create;
  try
    // As far as I can tell, this call pushes FMutatedMember onto the stack but
    // doesn't increase refCnt here. Then mm.DoMutation frees the string out
    // underneath it (replacing it with '1.0') and the passed parameter now refers
    // to invalid memory, resulting in a double-free of the string
    mm.DoNothing(mm.DoMutation, 'ignored', mm.FMutatedMember);
  finally
    mm.Free;
  end;
end;

{$IFDEF LOG_MEMORY_OPERATIONS}
var
  DefaultMM, LoggedAllocsAndFrees: TMemoryManagerEx;

function LoggedGetMem(Size: NativeInt): Pointer;
var
  b: array[0..64] of char;
begin
  Result := DefaultMM.GetMem(Size);
  Winapi.Windows.wsprintf(b, 'GetMem(%x) = %x', Size, Cardinal(Result));
  OutputDebugString(b);
end;

function LoggedFreeMem(P: Pointer): Integer;
var
  b: array[0..64] of char;
begin
  Result := DefaultMM.FreeMem(P);
  Winapi.Windows.wsprintf(b, 'FreeMem(%x) = %x', Cardinal(P), Result);
  OutputDebugString(b);
end;

function LoggedReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := DefaultMM.ReallocMem(P, Size);
end;

procedure LogMemoryOperations;
begin
  GetMemoryManager(DefaultMM);
  Move(DefaultMM, LoggedAllocsAndFrees, sizeof(TMemoryManagerEx));
  LoggedAllocsAndFrees.GetMem := @LoggedGetMem;
  LoggedAllocsAndFrees.FreeMem := @LoggedFreeMem;
  LoggedAllocsAndFrees.ReallocMem := @LoggedReallocMem;
  SetMemoryManager(LoggedAllocsAndFrees);
end;
{$ENDIF}

end.
