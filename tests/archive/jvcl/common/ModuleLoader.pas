{******************************************************************}
{                                                                  }
{       Project JEDI                                               }
{       OS independent Dynamic Loading Helpers                     }
{                                                                  }
{ The initial developer of the this code is                        }
{ Robert Marquardt <robert_marquardt@gmx.de)                       }
{                                                                  }
{ Copyright (C) 2000, 2001 Robert Marquardt.                       }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit ModuleLoader;

interface

{$WEAKPACKAGEUNIT ON}

// each OS gets its own IFDEFed complete code block to make reading easier

{$IFDEF WIN32}

uses
  Windows;

type
  // Handle to a loaded DLL
  TModuleHandle = HINST;

const
  // Value designating an unassigned TModuleHandle od a failed loading
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

implementation

// load the DLL file FileName
// the rules for FileName are those of LoadLibrary
// Returns: True = success, False = failure to load
// Assigns: the handle of the loaded DLL to Module
// Warning: if Module has any other value than INVALID_MODULEHANDLE_VALUE
// on entry the function will do nothing but returning success.

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibrary(PChar(FileName));
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// load the DLL file FileName
// LoadLibraryEx is used to get better control of the loading
// for the allowed values for flags see LoadLibraryEx documentation.

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// unload a DLL loaded with LoadModule or LoadModuleEx
// The procedure will not try to unload a handle with
// value INVALID_MODULEHANDLE_VALUE and assigns this value
// to Module after unload.

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the DLL Module
// nil is returned if the symbol is not available

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the DLL Module
// nil is returned if the symbol is not available.
// as an extra the boolean variable Accu is updated
// by anding in the success of the function.
// This is very handy for rendering a global result
// when accessing a long list of symbols.

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

// get the value of variables exported from a DLL Module
// Delphi cannot access variables in a DLL directly, so
// this function allows to copy the data from the DLL.
// Beware! You are accessing the DLL memory image directly.
// Be sure to access a variable not a function and be sure
// to read the correct amount of data.

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

// set the value of variables exported from a DLL Module
// Delphi cannot access variables in a DLL directly, so
// this function allows to copy the data to the DLL!
// BEWARE! You are accessing the DLL memory image directly.
// Be sure to access a variable not a function and be sure
// to write the correct amount of data.
// The changes are not persistent. They get lost when the
// DLL is unloaded.

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF}

{$IFDEF LINUX}

uses
  Types, Libc;

type
  // Handle to a loaded .so
  TModuleHandle = Pointer;

const
  // Value designating an unassigned TModuleHandle od a failed loading
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(nil);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

implementation

// load the .so file FileName
// the rules for FileName are those of dlopen()
// Returns: True = success, False = failure to load
// Assigns: the handle of the loaded .so to Module
// Warning: if Module has any other value than INVALID_MODULEHANDLE_VALUE
// on entry the function will do nothing but returning success.

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), RTLD_NOW);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// load the .so file FileName
// dlopen() with flags is used to get better control of the loading
// for the allowed values for flags see "man dlopen".

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// unload a .so loaded with LoadModule or LoadModuleEx
// The procedure will not try to unload a handle with
// value INVALID_MODULEHANDLE_VALUE and assigns this value
// to Module after unload.

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    dlclose(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the .so Module
// nil is returned if the symbol is not available

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the .so Module
// nil is returned if the symbol is not available.
// as an extra the boolean variable Accu is updated
// by anding in the success of the function.
// This is very handy for rendering a global result
// when accessing a long list of symbols.

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

// get the value of variables exported from a .so Module
// Delphi cannot access variables in a .so directly, so
// this function allows to copy the data from the .so.
// Beware! You are accessing the .so memory image directly.
// Be sure to access a variable not a function and be sure
// to read the correct amount of data.

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

// set the value of variables exported from a .so Module
// Delphi cannot access variables in a .so directly, so
// this function allows to copy the data to the .so!
// BEWARE! You are accessing the .so memory image directly.
// Be sure to access a variable not a function and be sure
// to write the correct amount of data.
// The changes are not persistent. They get lost when the
// .so is unloaded.

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF}

end.
