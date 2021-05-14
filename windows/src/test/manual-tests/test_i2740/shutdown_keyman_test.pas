(*
  Name:             shutdown_keyman_test
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Feb 2011

  Modified Date:    22 Feb 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Feb 2011 - mcdurdin - I2740 - Auto Update should close down Keyman before starting the update
*)
unit shutdown_keyman_test;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  psapi,
  tlhelp32;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  hwnd: THandle;
begin
  hwnd := FindWindow('TfrmKeyman7Main', nil);
  if hwnd <> 0 then PostMessage(hwnd, WM_CLOSE, 0, 0);


end;

function CloseThreadWindowProc(hwnd: THandle; lParam: LPARAM): BOOL; stdcall;
begin
  PostMessage(hwnd, WM_CLOSE, 0, 0);
  Result := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  KeymanApplicationNames: array[0..2] of string = ('kmshell.exe', 'keyman.exe', 'tsysinfo.exe');
var
  i: Integer;
  cbNeeded, cb: DWORD;
  processes: PDWORD;
  nProcesses: Cardinal;
  BaseName: array[0..260] of char;
  hModule, hProcess: THandle;
  n: Integer;
  FAppName: string;
  te: tagTHREADENTRY32;
  FProcessIDs: array of Cardinal;
  pid: PDWORD;
var
  hSnapshot: THandle;
begin
  cb := 1000 * sizeof(DWORD);
  processes := AllocMem(cb);
  repeat
    if not EnumProcesses(processes, cb, cbNeeded) then Exit;
    if cb = cbNeeded then
    begin
      cb := cb * 2;
      ReallocMem(processes, cb);
      cbNeeded := cb;
    end;
  until cb > cbNeeded;
  try
    SetLength(FProcessIDs, 0);
    pid := processes;
    nProcesses := cbNeeded div sizeof(DWORD);
    for i := 0 to nProcesses - 1 do
    begin
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, pid^);
      if hProcess <> 0 then
      try
        if EnumProcessModules(hProcess, @hModule, sizeof(hModule), cbNeeded) then
        begin
          if GetModuleBaseName(hProcess, hModule, baseName, Sizeof(BaseName) div SizeOf(BaseName[0])) > 0 then
          begin
            FAppName := ExtractFileName(BaseName);
            for n := 0 to High(KeymanApplicationNames) do
              if SameText(FAppName, KeymanApplicationNames[n]) then
              begin
                SetLength(FProcessIDs, Length(FProcessIDs)+1);
                FProcessIDs[High(FProcessIDs)] := pid^;
              end;
          end;
        end;
      finally
        CloseHandle(hProcess);
      end;
      Inc(pid);
    end;
  finally
    FreeMem(processes);
  end;

  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
  if hSnapshot <> INVALID_HANDLE_VALUE then
  try
    FillChar(te,sizeof(te),0);
    te.dwSize := SizeOf(te);
    if Thread32First(hSnapshot, te) then
    begin
      repeat
        for i := 0 to High(FProcessIDs) do
          if te.th32OwnerProcessID = FProcessIDs[i] then
          begin
            EnumThreadWindows(te.th32ThreadID, @CloseThreadWindowProc, 0);
          end;
      until not Thread32Next(hSnapshot, te);
    end;
  finally
    CloseHandle(hSnapshot);
  end;
end;

end.
