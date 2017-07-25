{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains stream classes using memory mapped files.

Author
Michael Morstein

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

Note

The Original Code is JwsclStreams.pas.

The Initial Developer of the Original Code is Michael Morstein.
Portions created by Michael Morstein are Copyright (C) Michael Morstein. All rights reserved.

Todo
Add Security Features for Mapped Files. (Security Descriptor)
}

unit JwsclStreams;
{$INCLUDE ..\includes\Jwscl.inc}

interface

uses JwaWindows, Classes,
     JwsclExceptions, JwsclResource, JwsclStrings;

const
  fmCreateTemporary    = 4;

  CREATE_TEMPORARY     = 6;
  FILE_SHARE_NONE      = 0;
  FILE_SHARE_READWRITE = FILE_SHARE_READ or FILE_SHARE_WRITE;

  FILE_WRITE_DATA      = 2;

type
  { Base class of every mapped stream. Inherits from Classes.TStream
    
    <b>Important</b>
    
    DO NOT CREATE OBJECT INSTANCES OF THIS CLASS! Use one of the other stream
    classes instead.

    Todo
    Add Security Features for Mapped Files. (Security Descriptor)
  }
  TJwCustomMappedStream = class(TStream)
  private
    FFileHandle: hFile;
    FMapHandle: THandle;
    FMemory: Pointer;
    FReadOnly: Boolean;
    FPosition, FSize: Int64;
  protected
    function CreateMapView(DataSize: Int64; Name: TJwPChar = nil): Int64;
    procedure CloseMapView;
    procedure EOSHandler(var Count: LongInt); virtual; abstract;
  public
    {<B>Read</B> Reads <i>Count</i> Bytes into the <i>Buffer</i>
       @param Buffer Specifies the buffer where the stream data is to be read.
       @param Count The number of bytes which will be written into the buffer.

       raises
         EJwsclNilPointer: is raised if the memory is a nil pointer.

       @return Returns the number of bytes which have currently been read from the buffer.
    }
    function Read(var Buffer; Count: Longint): Longint; override;

    {<B>Write</B> Writes <i>Count</i> Bytes from the <i>Buffer</i> into the stream
       @param Buffer The buffer which will be written into the stream
       @param Count The size of the <i>Buffer</i>

       raises
         EJwsclNilPointer: is raised if the memory is a nil pointer.

       @return Returns the number of bytes which have currenly been written to the stream.
    }
    function Write(const Buffer; Count: Longint): Longint; override;

    {<B>Seek</B> Sets the position of the data pointer of the stream
       @param Offset The new relative position of the stream. (Depending on the <i>Origin</i>-parameter)
       @param Origin Can be one of the following enumeration values

                *soFromBeginning: The new offset equals to the Offset-paramter
                *soCurrent      : The new offset is the sum of the current position and the
                                  value which has been specified in the Offset-parameter
                *soEnd          : The new offset is the sum of the stream size and the value
                                  which has been specified in the Offset parameter

       @return Returns the new position of the stream
    }
    {$IFDEF DELPHI6_UP}
      function Seek(const Offset: Int64; Origin:TSeekOrigin): Int64; override;
    {$ELSE}
      function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF}

    {The property <B>Memory</B> returns a pointer to the stream data}
    property Memory: Pointer read FMemory;

    {
     The property <B>Readonly</B> returns whether the user has full access or only readonly access to the stream
     The value of this property depends on the kind of the stream class
    }
    property Readonly: Boolean read FReadOnly;
  end;

  { This is a FileStream based on Memory Mapped Files}
  TJwFileStreamEx = class(TJwCustomMappedStream)
  private
    FFilename: TJwString;
  protected
{$IFDEF DELPHI6_UP}
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
{$ELSE}
    procedure SetSize(NewSize: Longint); override;
{$ENDIF}
    procedure EOSHandler(var Count: LongInt); override;
  public
    {<B>Create</B> Creates an instance of the TJwFileStreamEx class

      @param Filename The filename of the file which is to be opened/created
      @param Access Specifies the way the filestream opens the file.

             *fmCreate         : Creates a new file
             *fmCreateTemporary: Creates a new file which is deleted after the filestream is destroyed
             *fmOpenRead       : Opens an existing file with Readonly access!
             *fmOpenWrite      : Opens an existing file with WriteOnly access!
             *fmOpenReadWrite  : Opens an existing file with full access (read/write)

      @param ShareMode The sharing mode of the file. Can be:

             *FILE_SHARE_NONE     : Other processes cannot access the file until the stream is destroyed
             *FILE_SHARE_READ     : Other processes have read access to the file
             *FILE_SHARE_WRITE    : Other processes have write access to the file
             *FILE_SHARE_DELETE   : Other processes are allowed to delete the file
             *FILE_SHARE_READWRITE: Other processes have either read or write access to the file

      raises
        EJwsclWinCallFailedException: This exception is thrown if CreateFile fails
        EJwsclFileMappingException: This exception is thrown if an error has occured in TJwCustomMappedStream.CreateMapView
    }
    constructor Create(const Filename: TJwString; Access: Word; ShareMode: Word = FILE_SHARE_READWRITE); overload;

    {<B>Create</B> Creates an instance of the TJwFileStreamEx class

      @param FileHandle A handle to file which has been opened with CreateFile(Ex)
      @param DuplicateFileHandle Specifies whether the file handle is duplicated or not
                                 If this parameter is true you have to close the file handle by yourself

      raises
        EJwsclInvalidHandle: This exception is thrown if LoadLibray failed
        EJwsclNilPointer: This exception is thrown if GetProcAddress failed
        EJwsclFileMappingException: This exception is thrown if an error has occured in TJwCustomMappedStream.CreateMapView
    }
    constructor Create(FileHandle: hFile; DuplicateFileHandle: Boolean = false); overload;

    {<B>Destryoy</B> Destroys the instance FilestreamEx}
    destructor Destroy; override;

    {<B>Clear</B> Deletes all data in the stream and sets the size of the stream to zero}
    procedure Clear;
  end;

  { This is a stream class for simple IPC via Mapped-Memory-Streams}
  TJwIPCStream = class(TJwCustomMappedStream)
  private
    function OpenMapView(Name: TJwPChar): Int64;
  protected
    procedure EOSHandler(var Count: LongInt); override;
  public
    {<B>Create</B> Creates an instance of the TJwIPCStream class

      @param Name The name of the MMF which is to be created or which the user wants to connect to
      @param ReadonlyAccess Specifies whether the user wants readonly access to the MMF or not
      @param FileSize Default = -1. Specifies the size of the MMF. Must be a multiple of 4096
                      otherwise Create rounds to the next multiple of 4096.
                      If <i>FileSize</i> is less than zero, the MMF is not created but the stream connects
                      to the in <i>Name</i> specified MMF.
      raises
        EJwsclFileMappingException: This exception is thrown if an error has occured in TJwCustomMappedStream.CreateMapView
    }
    constructor Create(Name: TJwString; ReadonlyAccess: Boolean = false; FileSize: Int64 = -1);

    {<B>Destryoy</B> Destroys the instance FilestreamEx}
    destructor Destroy; override;
  end;

  { This is a stream class for general data handling}
  TJwVirtualStream = class(TJwCustomMappedStream)
  private
    function GetMemory: Pointer;
    procedure SetMemory(const Value: Pointer);
    function GetDataSize: Int64;
    procedure SetDataSize(const Value: Int64);
  protected
    procedure EOSHandler(var Count: LongInt); override;
  public
    {<B>Create</B> Creates an instance of the TJwVirtualStream class

      @param P Pointer to the data which the stream should handle
      @param DataSize Size of the data stored at <i>P</i>
      @param ReadonlyAccess Default = true. Specifies whether the user has readonly access or not
    }
    constructor Create(P: Pointer; DataSize: Int64; ReadonlyAccess: Boolean = true);

    {The property <B>Memory</B> returns or sets the data pointer of the stream}
    property Memory: Pointer read GetMemory write SetMemory;

    {The property <B>Size</B> returns or sets the Size of the stream data}
    property Size: Int64 read GetDataSize write SetDataSize;
  end;


implementation


{$IFDEF DELPHI5}
const
   soBeginning = soFromBeginning;
   soCurrent = soFromCurrent;
   soEnd = soFromEnd;
{$ENDIF}

{ TJwCustomMappedStream }

procedure TJwCustomMappedStream.CloseMapView;
begin
  UnmapViewOfFile(FMemory);
  CloseHandle(FMapHandle);
end;

function TJwCustomMappedStream.CreateMapView(DataSize: Int64; Name: TJwPChar): Int64;
var inf: TMemoryBasicInformation;
begin
  if DataSize = 0 then
    DataSize := 1;
  if not Readonly then
  {$IFDEF UNICODE}
    FMapHandle := CreateFileMappingW(FFileHandle,nil,PAGE_READWRITE,0,DataSize,Name)
  {$ELSE}
    FMapHandle := CreateFileMappingA(FFileHandle,nil,PAGE_READWRITE,0,DataSize,Name)
  {$ENDIF}
  else
  {$IFDEF UNICODE}
    FMapHandle := CreateFileMappingW(FFileHandle,nil,PAGE_READONLY,0,DataSize,Name);
  {$ELSE}
    FMapHandle := CreateFileMappingA(FFileHandle,nil,PAGE_READONLY,0,DataSize,Name);
  {$ENDIF}
  
  if FMapHandle = 0 then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'CreateMapView',Classname,RsUNStreams,0,true,'CreateFileMapping',['CreateFileMapping']);

  if not Readonly then
    FMemory := MapViewOfFile(FMapHandle,FILE_MAP_READ or FILE_MAP_WRITE,0,0,0)
  else
    FMemory := MapViewOfFile(FMapHandle,FILE_MAP_READ,0,0,0);

  if (Assigned(FMemory)) and (Name = nil) then
    Result := DataSize
  else
  if (Assigned(FMemory)) and (Assigned(Name)) then
  begin
    VirtualQuery(FMemory,inf,SizeOf(TMemoryBasicInformation));
    Result := inf.RegionSize;
  end
  else
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'CreateMapView',Classname,RsUNStreams,0,true,'MapViewOfFile',['MapViewOfFile']);
end;

function TJwCustomMappedStream.Read(var Buffer; Count: Integer): Longint;
begin
  if FMemory <> nil then
  begin
    if FPosition + Count > Size then
      Count := FSize-FPosition;

//64bit Warning: Converting a pointer to Cardinal may conflict with 64bit
    Move(Pointer(DWORD_PTR(FMemory)+FPosition)^,Buffer,Count);
    inc(FPosition,Count);
    Result := Count;
  end
  else
    raise EJwsclNilPointer.CreateFmtEx(RsNilPointer,'Read',Classname,RsUNStreams,0,false,[]);
end;

{$IFDEF DELPHI6_UP}
function TJwCustomMappedStream.Seek(const Offset: Int64; Origin:TSeekOrigin): Int64;
{$ELSE}
function TJwCustomMappedStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF}
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent  : FPosition := FPosition + Offset;
    soEnd      : FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TJwCustomMappedStream.Write(const Buffer; Count: Integer): Longint;
begin
  if (FMemory <> nil) and (not Readonly) then
  begin
    if (FPosition + Count > FSize) then
      EOSHandler(Count);

//64bit Warning: Converting a pointer to Cardinal may conflict with 64bit
    Move(Buffer,Pointer(DWORD_PTR(FMemory) + FPosition)^,Count);
    inc(FPosition,Count);
    Result := Count;
  end
  else
    raise EJwsclNilPointer.CreateFmtEx(RsNilPointer,'Write',Classname,RsUNStreams,0,false,[]);
end;

{ TJwFileStreamEx }

constructor TJwFileStreamEx.Create(FileHandle: hFile; DuplicateFileHandle: Boolean);

  function GetFileReadonly(f: hFile): Boolean;
  var inf: TObjectBasicInformation;
      size: Cardinal;
  begin
    begin
      size := SizeOf(TObjectBasicInformation);
      ZeroMemory(@inf, sizeof(inf));

      if NT_SUCCESS(NtQueryObject(f,ObjectBasicInformation,@inf,size,nil)) then
        Result := (inf.GrantedAccess and FILE_WRITE_DATA) <> FILE_WRITE_DATA
      else
        Result := true; //on error just use a readonly thinking
    end;
  end;

  function GetFileHandle(Duplicate: Boolean): hFile;
  var hTmp : THandle;
  begin
    if DuplicateFileHandle then
    begin
      hTmp := 0;
      if DuplicateHandle(GetCurrentProcess,FileHandle,GetCurrentProcess,@hTmp,0,false,DUPLICATE_SAME_ACCESS) then
        Result := hTmp
      else
        Result := INVALID_HANDLE_VALUE;
    end
    else
      Result := FileHandle;
  end;

begin
  FFileHandle := GetFileHandle(DuplicateFileHandle);
  FReadonly := GetFileReadonly(FFileHandle);

  if (FFileHandle <> INVALID_HANDLE_VALUE) and (FFileHandle <> 0) then
    FSize := CreateMapView(GetFileSize(FFileHandle,nil))
  else
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'Create',Classname,RsUNStreams,0,true,'CreateFile',['CreateFile']);
end;

constructor TJwFileStreamEx.Create(const Filename: TJwString; Access: Word; ShareMode: Word);
const AccessMode: Array[0..4] of Cardinal = (GENERIC_READ, GENERIC_WRITE,
                                             GENERIC_READ or GENERIC_WRITE,
                                             GENERIC_READ or GENERIC_WRITE,
                                             GENERIC_READ or GENERIC_WRITE);
      CreateFlag: Array[0..4] of Cardinal = (OPEN_EXISTING, OPEN_EXISTING,
                                             OPEN_EXISTING, CREATE_ALWAYS,
                                             CREATE_ALWAYS);
var FileFlag: Cardinal;
begin
  inherited Create;
  FFilename := Filename;
  FReadOnly := Access = 0;
  if Access = fmCreate then
    Access := 3;

  if Access <> 4 then
    FileFlag := FILE_ATTRIBUTE_NORMAL
  else
    FileFlag := FILE_FLAG_DELETE_ON_CLOSE;

  {$IFDEF UNICODE}
    FFileHandle := CreateFileW(TJwPChar(FFilename),AccessMode[Access],ShareMode,nil,
                               CreateFlag[Access],FileFlag,0);
  {$ELSE}
    FFileHandle := CreateFileA(TJwPChar(FFilename),AccessMode[Access],ShareMode,nil,
                               CreateFlag[Access],FileFlag,0);
  {$ENDIF}

  if (FFileHandle <> INVALID_HANDLE_VALUE) and (FFileHandle <> 0) then
    FSize := CreateMapView(GetFileSize(FFileHandle,nil))
  else
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'Create',Classname,RsUNStreams,0,true,'CreateFile',['CreateFile']);
end;

procedure TJwFileStreamEx.SetSize(NewSize: Integer);
begin
{$IFDEF DELPHI6_UP}
  SetSize(Int64(NewSize));
{$ELSE}
  CloseMapView;
  SetFilePointer(FFileHandle,NewSize,nil,FILE_BEGIN);
  SetEndOfFile(FFileHandle);
  FSize := CreateMapView(NewSize);
{$ENDIF}
end;

{$IFDEF DELPHI6_UP}
procedure TJwFileStreamEx.SetSize(const NewSize: Int64);
begin
  CloseMapView;
  SetFilePointer(FFileHandle,NewSize,nil,FILE_BEGIN);
  SetEndOfFile(FFileHandle);
  FSize := CreateMapView(NewSize);
end;
{$ENDIF}

destructor TJwFileStreamEx.Destroy;
begin
  CloseMapView;
  CloseHandle(FFileHandle);
  inherited Destroy;
end;

procedure TJwFileStreamEx.Clear;
begin
  SetSize(0);
  FPosition := 0;
end;

procedure TJwFileStreamEx.EOSHandler(var Count: LongInt);
var Overflow: LongInt;
begin
  Overflow := (FPosition+Count)-FSize;
  SetSize(FSize+Overflow);
end;

{ TJwIPCStream }

constructor TJwIPCStream.Create(Name: TJwString; ReadonlyAccess: Boolean; FileSize: Int64);
begin
  inherited Create;
  FReadOnly := ReadonlyAccess;
  FFileHandle := INVALID_HANDLE_VALUE;
  if FileSize >= 0 then
    FSize := CreateMapView(FileSize,TJwPChar(Name))
  else
    FSize := OpenMapView(TJwPChar(Name));
end;

destructor TJwIPCStream.Destroy;
begin
  CloseMapView;
  inherited;
end;

procedure TJwIPCStream.EOSHandler(var Count: Integer);
begin
  Count := FSize-FPosition;
end;

function TJwIPCStream.OpenMapView(Name: TJwPChar): Int64;
var inf: TMemoryBasicInformation;
begin
  if not ReadOnly then
  {$IFDEF UNICODE}     
    FMapHandle := OpenFileMappingW(FILE_MAP_READ or FILE_MAP_WRITE,false,Name)
  {$ELSE}
    FMapHandle := OpenFileMappingA(FILE_MAP_READ or FILE_MAP_WRITE,false,Name)
  {$ENDIF}
  else
  {$IFDEF UNICODE}
    FMapHandle := OpenFileMappingW(FILE_MAP_READ,false,Name);
  {$ELSE}
    FMapHandle := OpenFileMappingA(FILE_MAP_READ,false,Name);
  {$ENDIF}

  if FMapHandle = 0 then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'OpenMapView',Classname,RsUNStreams,0,true,'OpenFileMapping',['OpenFileMapping']);

  if not Readonly then
    FMemory := MapViewOfFile(FMapHandle,FILE_MAP_READ or FILE_MAP_WRITE,0,0,0)
  else
    FMemory := MapViewOfFile(FMapHandle,FILE_MAP_READ,0,0,0);

  if Assigned(FMemory) then
  begin
    VirtualQuery(FMemory,inf,SizeOf(TMemoryBasicInformation));
    Result := inf.RegionSize;
  end
  else
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'OpenMapView',Classname,RsUNStreams,0,true,'MapViewOfFile',['MapViewOfFile']);
end;

{ TJwVirtualStream }

constructor TJwVirtualStream.Create(P: Pointer; DataSize: Int64; ReadonlyAccess: Boolean);
begin
  inherited Create;
  FReadonly := ReadOnlyAccess;
  FMemory := P;
  FSize := DataSize;
end;

procedure TJwVirtualStream.EOSHandler(var Count: Integer);
begin
  raise EJwsclEndOfStream.CreateFmtEx(RsStreamsDataOutOfBounds,'EOSHandler',Classname,RsUNStreams,0,false,[]);
end;

function TJwVirtualStream.GetMemory: Pointer;
begin
  Result := FMemory;
end;

function TJwVirtualStream.GetDataSize: Int64;
begin
  Result := inherited Size;
end;

procedure TJwVirtualStream.SetMemory(const Value: Pointer);
begin
  FMemory := Value;
end;

procedure TJwVirtualStream.SetDataSize(const Value: Int64);
begin
  inherited Size := Value;
end;

end.
