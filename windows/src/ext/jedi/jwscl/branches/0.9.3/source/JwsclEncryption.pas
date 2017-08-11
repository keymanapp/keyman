{ Project JEDI Windows Security Code Library (JWSCL)
  
  Currently the Windows Vista Crypt API is not supported by this unit. The direct
  memory encryption is simulated.
  Author
    * Christian Wimmer
    * Philip Dittmann
  License
  The contents of this file are subject to the Mozilla Public License Version 1.1
  (the "License"); you may not use this file except in compliance with the
  \License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
  specific language governing rights and limitations under the License.
  
  Alternatively, the contents of this file may be used under the terms of the GNU
  Lesser General Public License (the "LGPL License"), in which case the provisions
  of the LGPL License are applicable instead of those above. If you wish to allow
  use of your version of this file only under the terms of the LGPL License and
  not to allow others to use your version of this file under the MPL, indicate
  your decision by deleting the provisions above and replace them with the notice
  and other provisions required by the LGPL License. If you do not delete the
  provisions above, a recipient may use your version of this file under either the
  MPL or the LGPL License.
  
  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  Note
  The Original Code is JwsclEncryption.pas.
  
  The Initial Developer of the Original Code is Christian Wimmer. Portions created
  by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.
  Portions created by Philip Dittmann are Copyright (C) Philip Dittmann. All
  rights reserved.                                                                 }
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclEncryption;

{$INCLUDE ..\includes\Jwscl.inc}

//do not move header comment from above unit declaration!

interface

uses
  SysUtils, Classes,
  jwaWindows, JwsclResource,
  JwsclTypes, JwsclExceptions, JwsclAcl,
  JwsclVersion, JwsclConstants,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}


type


  {<B>TJwEncryptionApi</B> defines methods to give direct access to Crypt API
  functions - with more simple call conventions}
  TJwEncryptionApi = class (TObject)
  public
    class function CryptProtectData(DataIn : TDataBlob; Description : TJwString;
            Entropy : PDataBlob; PromptInfo : TCryptProtectPromptStruct; Flags :
            TJwCryptProtectFlagSet): TDataBlob; virtual;

    class procedure CryptProtectMemory(Data: Pointer; Size : Cardinal; Flags :
            TJwProtectMemoryFlagSet); virtual;

    class function CryptUnProtectData(DataIn : TDataBlob;
            Entropy : PDataBlob; PromptInfo : TCryptProtectPromptStruct; Flags :
            TJwCryptProtectFlagSet): TDataBlob; virtual;

    class procedure CryptUnProtectMemory(Data: Pointer; Size : Cardinal; Flags :
            TJwProtectMemoryFlagSet); virtual;

   


  end;

  {<B>TJwEncryptMemory</B> provides functions to encrypt memory directly.}
  TJwEncryptMemory = class (TJwEncryptionApi)
  public
    {<B>EncryptMemory</B> encrypts and protects a block of memory.
     If the necessary memory is not big enough it automatically increases
     the memory.
     @param P Defines a pointer which must be encrypted. 
     @param Size Defines how much data of P must be encrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the encrypted data. 
     @param Flags Defines who can decrypt the new data.
        
          # pmSameProcess - defines that only the process which created
            the encrypted data can decrypt it
          # pmCrossProcess - defines that other processes on the same
            computer can decrypt the data 
          # pmSameLogon - defines only the same use can decrypt the data 
         
     @param MemoryType defines which type of memory manager created P.
       Currently only mtGetMem is supported. 
     raises
 EjwsclCryptUnsupportedException:  if MemoryType is not mtGetMem. 
      EjwsclCryptApiException: if an underlying API function failed. 
    }
    class procedure EncryptMemory(var P : Pointer;
      var Size : Cardinal; const Flags : TJwProtectMemoryFlagSet;
      const MemoryType : TJwMemoryType); virtual;

    {<B>DecryptMemory</B> decrypts a block of memory.
     The function also checks whether the given data was changed.
     The decrypted memor block will be resized to its original size.

     @param P Defines a pointer which must be decrypted. 
     @param Size Defines how much data of P must be decrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the encrypted data.
      The function changes the parameter value to the new size of the memory block.
       
     @param Flags Must be the same value of Flags specified in a previous call to EncryptMemory.  
     @param MemoryType defines which type of memory manager created P.
       Currently only mtGetMem is supported. 
     raises
 EjwsclCryptUnsupportedException:  if MemoryType is not mtGetMem. 
      EjwsclCryptApiException: if an underlying API function failed. 
    }
    class procedure DecryptMemory(var P : Pointer;
      var Size : Cardinal; const Flags : TJwProtectMemoryFlagSet;
      const MemoryType : TJwMemoryType); virtual;
  end;

  {<B>TJwEncryptData</B> provides access to encryption functions.}
  TJwEncryptData = class (TJwEncryptionApi)
  public
    {<B>EncryptPointerWithPrompt</B> encrypts and protects a memory block.
     To decrypt the data use DecryptPointerWithPrompt.
     @param P Defines a pointer which must be encrypted. 
     @param Size Defines how much data of P must be encrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the encrypted data. 
     @param Description Defines a string that describes the data. Can be empty.
     @param Entropy Defines additional data to be used to encrypt the data.
      The same Entropy must be specified to the decryption method DecryptPointerWithPrompt.
      Can be nil to use no entropy. 
     @param LocalMachineOnly Set to true so the data can only be decrypted on the
       same computer; otherwise false. 
     @param NoUi defines whether a dialog pops up where the user can change
       encryption security and set a password. To do so set the parameter to false.
       For none user interface apps or remote applications use true. 
     @param PromptFlags defines additional flags for UI dialog. This parameter
       applies only if NoUi is false. 
     @param Prompt defines a text to be displayed in the prompt dialog. This parameter
       applies only if NoUi is false. 
     @param Data [out]. This parameter receives the encrypted data.
       The data can be freed using FreeMem on its Data member .
     raises
 EjwsclCryptApiException:  if an underlying API function failed.

     }
    class procedure EncryptPointerWithPrompt(
      const P : Pointer;
      const Size : Cardinal;
      const Description : TJwString;
      const Entropy : PDataBlob;
      const LocalMachineOnly : Boolean;
      const NoUi : Boolean;
      const PromptFlags : TJwCryptProtectOnPromptFlagSet;
      const WindowParent : HWND;
      const Prompt : TJwString;
      out Data : TJwGetMemBlob);

    {<B>EncryptPointer</B> is like EncryptPointerWithPrompt but does not use a user interface.
     @param P Defines a pointer which must be encrypted. 
     @param Size Defines how much data of P must be encrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the encrypted data. 
     @param Entropy Defines additional data to be used to encrypt the data.
      The same Entropy must be specified to the decryption method DecryptPointerWithPrompt.
      Can be nil to use no entropy. 
     @param LocalMachineOnly Set to true so the data can only be decrypted on the
       same computer; otherwise false. 
      @param Data [out]. This parameter receives the encrypted data.
       The data can be freed using FreeMem on its Data member .
     raises
 EjwsclCryptApiException:  if an underlying API function failed.  }
    class procedure EncryptPointer(const P : Pointer; const Size : Cardinal;
      const Description : TJwString;
      const Entropy : PDataBlob;
      const LocalMachineOnly : Boolean;
      out Data : TJwGetMemBlob);

    {<B>DecryptPointerWithPrompt</B> decrypts a memory block that was created by EncryptPointerWithPrompt.
     The data is also checked for manipulation.

     @param P Defines a pointer which must be decrypted. 
     @param Size Defines how much data of P must be decrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the encrypted data.
      
     @param Entropy Defines additional data to be used to decrypt the data.
      The same Entropy must be specified to the encryption method EncryptPointerWithPrompt.
      Can be nil to use no entropy. 
     @param NoUi defines whether a dialog pops up where the user can change
       encryption security and set a password. To do so set the parameter to false.
       For none user interface apps or remote applications use true. 
     @param PromptFlags defines additional flags for UI dialog. This parameter
       applies only if NoUi is false. 
     @param Prompt defines a text to be displayed in the prompt dialog. This parameter
       applies only if NoUi is false. 
     @param Data [out]. This parameter receives the decrypted data.
       The data can be freed using FreeMem on its Data member .
     raises
 EjwsclCryptApiException:  if an underlying API function failed. 

     }
    class procedure DecryptPointerWithPrompt(const P : Pointer; const Size : Cardinal;
      const Entropy : PDataBlob;
      const NoUi : Boolean;
      const PromptFlags : TJwCryptProtectOnPromptFlagSet;
      const WindowParent : HWND;
      const Prompt : TJwString;
      out Data : TJwGetMemBlob);

     {<B>DecryptPointer</B> is like DecryptPointerWithPrompt but does not use a user interface.
      The data is also checked for manipulation.
     @param P Defines a pointer which must be decrypted. 
     @param Size Defines how much data of P must be decrypted. Do specify exactly
      the amount of allocated memory of P. Otherwise the memory which is beyond
      the given Size can become overwritten by the decrypted data. 
     @param Entropy Defines additional data to be used to decrypt the data.
      The same Entropy must be specified to the encryption method EncryptPointerWithPrompt.
      Can be nil to use no entropy. 
      @param Data [out]. This parameter receives the decrypted data.
       The data can be freed using FreeMem on its Data member .
     raises
 EjwsclCryptApiException:  if an underlying API function failed.  }
    class procedure DecryptPointer(const P : Pointer; const Size : Cardinal;
      const Entropy : PDataBlob;
      out Data : TJwGetMemBlob);
  end;

  {<B>TJwRandomDataGenerator</B> provides access to functions to create random data. }
  TJwRandomDataGenerator = class
  public
    {<B>GetStandardRandomData</B> generates a block of random data using pascal random function.
     The created data block must be freed by FreeRandomData.
     This function uses the pascal random function. So the randomize procedure
     must be called to let it work.

     @param Size defines how big the generated size of random data block will be.
       If the Size is 0 the size will be incremented to 512 bytes. 
     @return Returns a data block which contains random data. The block
      can be freed by FreeMem or using FreeRandomData 
     }
    class function GetStandardRandomData(const Size : Cardinal = 0) : TDataBlob; virtual;

    {<B>GetData</B> calls the standard random data generation function of this class.
     Default is GetStandardRandomData.
     }
    class function GetData(const Size : Cardinal = 0) : TDataBlob; virtual;


    {<B>FreeRandomData</B> frees a block of memory created by methods of @ClassName.
     The content of the given TDataBlock structure will be set to 0.}
    class procedure FreeRandomData(var Data : TDataBlob); overload; virtual;
    {<B>FreeRandomData</B> frees a block of memory created by methods of @ClassName.
     The parameter Data will be set to nil.}
    class procedure FreeRandomData(var Data : PDataBlob); overload; virtual;

    
  end;

{<B>JwEncryptString</B> encrypts and protects a string.
@param S defines a string that must be encrypted. 
@param Description Defines a string that describes the data. Can be empty. 
@param Prompt defines a whether a encryption prompt is displayed to confirm
  the decryption by the user.
@param LocalMachineOnly Set to true so the data can only be decrypted on the
 same computer; otherwise false. 
@param Entropy Defines additional data to be used to encrypt the data.
The same Entropy must be specified to the decryption method DecryptPointerWithPrompt.
Can be nil to use no entropy. 
@return Returns a new string that contains the encrypted data. 
raises
 EjwsclCryptApiException:  if an underlying API function failed. 
}
function JwEncryptString(const S : TJwString;
  const Description : TJwString = '';
  const Prompt : Boolean = false;
  const LocalMachineOnly : Boolean = false;
  const Entropy : PDataBlob = nil)
  : TJwString;

{<B>JwDecryptString</B> decrypts a string and checks for manipulation.
@param S defines a string that must be decrypted. 
@param Prompt defines a whether a encryption prompt is displayed to confirm
  the decryption by the user.
@param Entropy Defines additional data to be used to encrypt the data.
The same Entropy must be specified to the decryption method DecryptPointerWithPrompt.
Can be nil to use no entropy. 
@return Returns the decrypted string. 
raises
 EjwsclCryptApiException:  if an underlying API function failed. 
}
function JwDecryptString(const S : TJwString;
  const Prompt : Boolean = false;
  const Entropy : PDataBlob = nil)
  : TJwString;



{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses JwsclEnumerations;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

function JwEncryptString(const S : TJwString;
  const Description : TJwString = '';
  const Prompt : Boolean = false;
  const LocalMachineOnly : Boolean = false;
  const Entropy : PDataBlob = nil)
  : TJwString;
var Blob : TJwGetMemBlob;
begin
  if not Prompt then
    TJwEncryptData.EncryptPointer(@S[1], //const P : Pointer;
      Length(S)*sizeof(TJwChar),//const Size : Cardinal;
      Description,//const Description : TJwString;
      Entropy,//const Entropy : PDataBlob;
      LocalMachineOnly,//const LocalMachineOnly : Boolean;
      Blob//out Data : TJwGetMemBlob
    )
  else
    TJwEncryptData.EncryptPointerWithPrompt(
      @S[1],//const P : Pointer;
      Length(S)*sizeof(TJwChar),//const Size : Cardinal;
      Description,//const Description : TJwString;
      Entropy,//const Entropy : PDataBlob;
      LocalMachineOnly,//const LocalMachineOnly : Boolean;
      false,//const NoUi : Boolean;
      [cppf_PromptOnProtect,cppf_PromptOnUnprotect],//const PromptFlags : TJwCryptProtectOnPromptFlagSet;
      0,//const WindowParent : HWND;
      '',//const Prompt : TJwString;
      Blob//out Data : TJwGetMemBlob
      );
  try
    SetLength(result, Blob.Size);
    CopyMemory(@result[1], Blob.Data, Blob.Size);
  finally
    FreeMem(Blob.Data);
    Blob.Data := nil;
  end;
end;

function JwDecryptString(const S : TJwString;
  const Prompt : Boolean = false;
  const Entropy : PDataBlob = nil)
  : TJwString;
var Blob : TJwGetMemBlob;
begin
  if not Prompt then
    TJwEncryptData.DecryptPointer(@S[1], //const P : Pointer;
      Length(S),//const Size : Cardinal;
      Entropy,//const Entropy : PDataBlob;
      Blob//out Data : TJwGetMemBlob
    )
  else
    TJwEncryptData.DecryptPointerWithPrompt(
      @S[1],//const P : Pointer;
      Length(S),//const Size : Cardinal;
      Entropy,//const Entropy : PDataBlob;
      false,//const NoUi : Boolean;
      [],//const PromptFlags : TJwCryptProtectOnPromptFlagSet;
      0,//const WindowParent : HWND;
      '',//const Prompt : TJwString;
      Blob//out Data : TJwGetMemBlob
      );
  try
    SetLength(result, Blob.Size div sizeof(TJwChar));
    CopyMemory(@result[1], Blob.Data, Blob.Size);
  finally
    FreeMem(Blob.Data);
    Blob.Data := nil;
  end;
end;

{
******************************* TJwEncryptionApi *******************************
}


class function TJwEncryptionApi.CryptProtectData(DataIn : TDataBlob;
        Description : TJwString; Entropy : PDataBlob; PromptInfo :
        TCryptProtectPromptStruct; Flags : TJwCryptProtectFlagSet): TDataBlob;
begin
  if not JwaWindows.CryptProtectData(
    @DataIn,//__in          DATA_BLOB* pDataIn,
    PWideChar(WideString(Description)),//__in          LPCWSTR szDataDescr,
    Entropy,//__in          DATA_BLOB* pOptionalEntropy,
    nil,//__in          PVOID pvReserved,
    @PromptInfo,//__in_opt      CRYPTPROTECT_PROMPTSTRUCT* pPromptStruct,
    TJwEnumMap.ConvertProtectFlags(Flags),//__in          DWORD dwFlags,
    @result//__out         DATA_BLOB* pDataOut
    ) then

    raise EjwsclCryptApiException.CreateFmtWinCall(
      '',
      'CryptProtectData',                                //sSourceProc
      ClassName,                                //sSourceClass
      RsUNEncryption,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CryptProtectData',                   //sWinCall
      ['CryptProtectData']);                                  //const Args: array of const


end;

var _CryptProtectMemory: function(Memory: Pointer; Size: Cardinal; Flags: Cardinal): Cardinal; stdcall;
    CryptProtectMemoryNtStatus: Boolean; // Does the function above return a NT_STATUS?

class procedure TJwEncryptionApi.CryptProtectMemory(Data: Pointer; Size : Cardinal;
        Flags : TJwProtectMemoryFlagSet);
var Res: Integer; Exc: EJwsclCryptApiException;
begin
  //Try to use CryptProtectMemory - if it is not available, we have to use RtlEncryptMemory
  try
    GetProcedureAddress(Pointer(@_CryptProtectMemory), Crypt32, 'CryptProtectMemory');
  except
    GetProcedureAddress(Pointer(@_CryptProtectMemory), AdvApi32, 'SystemFunction040'); //RtlEncryptMemory is exported as SystemFunction040
    CryptProtectMemoryNtStatus:=True;
  end;
  if CryptProtectMemoryNtStatus then
  begin
    Res:=_CryptProtectMemory(Data, Size, TJwEnumMap.ConvertProtectMemoryFlags(Flags));
    if Res<>STATUS_SUCCESS then
    begin
      Exc:=EjwsclCryptApiException.CreateFmtEx(
        '',
        'CryptProtectMemory',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNEncryption,                          //sSourceFile
        0,                                           //iSourceLine
        RtlNtStatusToDosError(Res),                 //iLastError
        ['CryptProtectMemory']);                 //const args: array of const
      Exc.WinCallName:='RtlEncryptMemory';
      raise Exc;
    end;
  end
  else
  begin
    if _CryptProtectMemory(Data, Size, TJwEnumMap.ConvertProtectMemoryFlags(Flags))=0 then
      raise EjwsclCryptApiException.CreateFmtWinCall(
        '',
        'CryptProtectMemory',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNEncryption,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'CryptProtectMemory',                   //sWinCall
        ['CryptProtectMemory'])
  end;
end;



class function TJwEncryptionApi.CryptUnProtectData(DataIn: TDataBlob;
  Entropy: PDataBlob;
  PromptInfo: TCryptProtectPromptStruct;
  Flags: TJwCryptProtectFlagSet): TDataBlob;
begin
  if not JwaWindows.CryptUnProtectData(
    @DataIn,//__in          DATA_BLOB* pDataIn,
    nil,//__out_opt     LPWSTR* ppszDataDescr,
    Entropy,//__in          DATA_BLOB* pOptionalEntropy,
    nil,//__in          PVOID pvReserved,
    @PromptInfo,//__in_opt      CRYPTPROTECT_PROMPTSTRUCT* pPromptStruct,
    TJwEnumMap.ConvertProtectFlags(Flags),//__in          DWORD dwFlags,
    @result//__out         DATA_BLOB* pDataOut
    ) then

    raise EjwsclCryptApiException.CreateFmtWinCall(
      '',
      'CryptUnProtectData',                                //sSourceProc
      ClassName,                                //sSourceClass
      RsUNEncryption,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CryptUnProtectData',                   //sWinCall
      ['CryptUnProtectData']);                                  //const Args: array of const

end;

var _CryptUnProtectMemory: function(Memory: Pointer; Size: Cardinal; Flags: Cardinal): Cardinal; stdcall;
    CryptUnProtectMemoryNtStatus: Boolean; // Does the function above return a NT_STATUS?

class procedure TJwEncryptionApi.CryptUnProtectMemory(Data: Pointer; Size : Cardinal;
        Flags : TJwProtectMemoryFlagSet);
var Res: Integer; Exc: EJwsclCryptApiException;
begin
  //Try to use CryptUnProtectMemory - if it is not available, we have to use RtlDecryptMemory
  try
    GetProcedureAddress(Pointer(@_CryptUnProtectMemory), Crypt32, 'CryptUnProtectMemory');
  except
    GetProcedureAddress(Pointer(@_CryptUnProtectMemory), AdvApi32, 'SystemFunction041');
    CryptUnProtectMemoryNtStatus:=True;
  end;
  if CryptUnProtectMemoryNtStatus then
  begin
    Res:=_CryptUnProtectMemory(Data, Size, TJwEnumMap.ConvertProtectMemoryFlags(Flags));
    if Res<>STATUS_SUCCESS then
    begin
      Exc:=EjwsclCryptApiException.CreateFmtEx(
        '',
        'CryptUnProtectMemory',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNEncryption,                          //sSourceFile
        0,                                           //iSourceLine
        RtlNtStatusToDosError(Res),                 //iLastError
        ['CryptUnProtectMemory']);                 //const args: array of const
      Exc.WinCallName:='RtlDecryptMemory';
      raise Exc;
    end;
  end
  else
  begin
    if _CryptUnProtectMemory(Data, Size, TJwEnumMap.ConvertProtectMemoryFlags(Flags))=0 then
      raise EjwsclCryptApiException.CreateFmtWinCall(
        '',
        'CryptUnProtectMemory',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNEncryption,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'CryptUnProtectMemory',                   //sWinCall
        ['CryptUnProtectMemory'])
  end;
end;

{ TJwEncryptData }

class procedure TJwEncryptData.DecryptPointer(const P: Pointer;
  const Size: Cardinal;
  const Entropy: PDataBlob;
  out Data: TJwGetMemBlob);
begin
  DecryptPointerWithPrompt(P, Size,
    Entropy, true, [], 0, '', Data);
end;

class procedure TJwEncryptData.DecryptPointerWithPrompt(const P: Pointer;
  const Size: Cardinal;
  const Entropy: PDataBlob; const NoUi: Boolean;
  const PromptFlags: TJwCryptProtectOnPromptFlagSet;
  const WindowParent: HWND; const Prompt: TJwString;
  out Data: TJwGetMemBlob);
var DataIn : TDataBlob;
    PromptInfo : TCryptProtectPromptStruct;
    Flags : TJwCryptProtectFlagSet;
var Blob : TDataBlob;
begin
  DataIn.pbData := P;
  DataIn.cbData := Size;
  Flags := [];

  if NoUi then
    Include(Flags, cfUiFobidden);

  PromptInfo.cbSize := sizeof(TCryptProtectPromptStruct);
  PromptInfo.dwPromptFlags := TJwEnumMap.ConvertProtectPromptFlags(PromptFlags);
  PromptInfo.hwndApp := WindowParent;
  PromptInfo.szPrompt := PWideChar(WideString(Prompt));

  Blob := CryptUnProtectData(DataIn, Entropy, PromptInfo, Flags);

  Data.Size := Blob.cbData;
  Data.Data := nil;

  if Data.Size > 0 then
  begin
    GetMem(Data.Data, Data.Size);
    CopyMemory(Data.Data, Blob.pbData, Data.Size);
  end;
  if Blob.pbData <> nil then
    LocalFree(HLOCAL(Blob.pbData));

end;

class procedure TJwEncryptData.EncryptPointer(const P: Pointer;
  const Size: Cardinal; const Description: TJwString;
  const Entropy: PDataBlob; const LocalMachineOnly: Boolean;
  out Data: TJwGetMemBlob);
begin
  EncryptPointerWithPrompt(P, Size, Description,
    Entropy, LocalMachineOnly, true, [], 0, '', Data);
end;

class procedure TJwEncryptData.EncryptPointerWithPrompt(const P: Pointer;
  const Size: Cardinal; const Description: TJwString;
  const Entropy: PDataBlob; const LocalMachineOnly, NoUi: Boolean;
  const PromptFlags: TJwCryptProtectOnPromptFlagSet;
  const WindowParent: HWND; const Prompt: TJwString;
  out Data: TJwGetMemBlob);
var DataIn : TDataBlob;
    PromptInfo : TCryptProtectPromptStruct;
    Flags : TJwCryptProtectFlagSet;
var Blob : TDataBlob;
begin
  DataIn.pbData := P;
  DataIn.cbData := Size;
  Flags := [];
  if LocalMachineOnly then
    Include(Flags, cfLocalMachine);
  if NoUi then
    Include(Flags, cfUiFobidden);

  PromptInfo.cbSize := sizeof(TCryptProtectPromptStruct);
  PromptInfo.dwPromptFlags := TJwEnumMap.ConvertProtectPromptFlags(PromptFlags);
  PromptInfo.hwndApp := WindowParent;
  PromptInfo.szPrompt := PWideChar(WideString(Prompt));

  Blob := CryptProtectData(DataIn, Description,
    Entropy, PromptInfo, Flags);

  Data.Size := Blob.cbData;
  Data.Data := nil;

  if Data.Size > 0 then
  begin
    GetMem(Data.Data, Data.Size);
    CopyMemory(Data.Data, Blob.pbData, Data.Size);
  end;
  if Blob.pbData <> nil then
    LocalFree(HLOCAL(Blob.pbData));
end;

{ TJwRandomDataGenerator }

class procedure TJwRandomDataGenerator.FreeRandomData(var Data: TDataBlob);
begin
  if Data.pbData <> nil then
    FreeMem(Data.pbData);
  FillChar(Data,sizeof(Data), 0);
end;

class procedure TJwRandomDataGenerator.FreeRandomData(var Data: PDataBlob);
begin
  if Data <> nil then
    FreeRandomData(Data^);
  Data := nil;
end;

class function TJwRandomDataGenerator.GetData(const Size : Cardinal = 0) : TDataBlob; 
begin
  result := GetStandardRandomData(size);
end;

class function TJwRandomDataGenerator.GetStandardRandomData
 (const Size : Cardinal = 0): TDataBlob;
var i : Integer;
    p : PByte;
begin
  if Size = 0 then
  begin
    result.cbData := 512;
  end
  else
    result.cbData := Size;

  GetMem(result.pbData, result.cbData);
  p := PByte(result.pbData);

  for i := 0 to result.cbData-1 do
  begin
    p^ := random(255);
    Inc(p);
  end;

end;


{ TJwEncryptMemory }

class procedure TJwEncryptMemory.EncryptMemory(var P: Pointer;
  var Size: Cardinal; const Flags: TJwProtectMemoryFlagSet;
  const MemoryType: TJwMemoryType);
var Data: TJwGetMemBlob;
    EntropyPtr: PDataBlob;
    Entropy: TDataBlob;
    Id : Cardinal;
begin
  if (MemoryType = mtLocal) or
     (MemoryType = mtGlobal) then
     raise EjwsclCryptUnsupportedException.CreateFmtEx(
        RsCryptUnsupportedMemManager,// const MessageString: string;
        'EncryptMemory',ClassName,//sSourceProc, sSourceClass,
        RsUNEncryption,//sSourceFile: string;
        0,//iSourceLine:  Cardinal;
        false,//bShowLastError: boolean;
        []//const Args: array of const
        );

  EntropyPtr := nil;
  if pmSameProcess in Flags then
  begin
    Id := GetCurrentProcessId;
    Entropy.cbData := sizeof(Cardinal);
    Entropy.pbData := @Id;
    EntropyPtr := @Entropy;
  end;

  TJwEncryptData.EncryptPointer(
    P,//const P: Pointer;
    Size,//const Size: Cardinal;
    '',//const Description: TJwString;
    EntropyPtr,//const Entropy: PDataBlob;
    not (pmSameLogon in Flags),//const LocalMachineOnly: Boolean;
    Data//out Data: TJwGetMemBlob
    );
  Size := Data.Size;

  case MemoryType of
    mtGetMem : ReallocMem(P, Size);
{    mtLocal  :
     begin
       //P2 := LocalLock(HLOCAL(P));
       //FillChar(P2^, LocalSize(HLOCAL(P)), 0);
       //LocalUnlock(HLOCAL(P));
       P := Pointer(LocalReAlloc(HLOCAL(P), Size, LMEM_ZEROINIT));//LocalFlags(HLOCAL(P))));
       ID := GetLastError();
       if ID = 0 then;
     end;
    mtGlobal : GlobalReAlloc(HGLOBAL(P), Size, GlobalFlags(HGLOBAL(P)));}
  end;

  try
    case MemoryType of
      mtGetMem :
        begin
          CopyMemory(P, Data.Data, Size);
          FillChar(Data.Data^, Size, 0);
        end;
     { mtLocal  :
        begin
          ID := LocalSize(HLOCAL(P));
          if ID = 0 then;
          P2 := LocalLock(HLOCAL(P));
//          CopyMemory(P, Data.Data, Size);
//          FillChar(Data.Data^, Data.Size, 0);
          FillChar(P2^, 2, 0);
          LocalFlags(HLOCAL(P));
          LocalUnlock(HLOCAL(P));

          LocalFlags(HLOCAL(P));
        end;

      //mtGlobal : GlobalLock(P);      }
    end;
  finally
    FreeMem(Data.Data);
  end;
  LocalFlags(HLOCAL(P));
end;

class procedure TJwEncryptMemory.DecryptMemory(var P : Pointer;
      var Size : Cardinal; const Flags : TJwProtectMemoryFlagSet;
      const MemoryType : TJwMemoryType);
var Data: TJwGetMemBlob;
    EntropyPtr: PDataBlob;
    Entropy: TDataBlob;
    Id : Cardinal;
begin
  if (MemoryType = mtLocal) or
     (MemoryType = mtGlobal) then
     raise EjwsclCryptUnsupportedException.CreateFmtEx(
        RsCryptUnsupportedMemManager,// const MessageString: string;
        'DecryptMemory',ClassName,//sSourceProc, sSourceClass,
        RsUNEncryption,//sSourceFile: string;
        0,//iSourceLine:  Cardinal;
        false,//bShowLastError: boolean;
        []//const Args: array of const
        );

  EntropyPtr := nil;
  if pmSameProcess in Flags then
  begin
    Id := GetCurrentProcessId;
    Entropy.cbData := sizeof(Cardinal);
    Entropy.pbData := @Id;
    EntropyPtr := @Entropy;
  end;

  TJwEncryptData.DecryptPointer(
      P,//const P : Pointer;
      Size,//const Size : Cardinal;
      EntropyPtr,//const Entropy : PDataBlob;
      Data//out Data : TJwGetMemBlob
      );
  Size := Data.Size;

  case MemoryType of
    mtGetMem : ReallocMem(P, Size);
  {  mtLocal  : LocalReAlloc(HLOCAL(P), Size, LocalFlags(HLOCAL(P)));
    mtGlobal : GlobalReAlloc(HGLOBAL(P), Size, GlobalFlags(HGLOBAL(P)));
    }
  end;

  try
    case MemoryType of
      mtGetMem :
        begin
          CopyMemory(P, Data.Data, Size);
          FillChar(Data.Data^, Size, 0);
        end;
    {  mtLocal  :
        begin
          P2 := LocalLock(HLOCAL(P));
          CopyMemory(P2, Data.Data, Size);
          FillChar(Data.Data^, Size, 0);
          LocalUnlock(HLOCAL(P));
        end;

      //mtGlobal : GlobalLock(P);}
    end;


  finally
    FreeMem(Data.Data);
  end;
end;


procedure Test;
var x,x2 : TJwString;
    Blob : TDataBlob;
begin
  Blob := TJwRandomDataGenerator.GetStandardRandomData(0);
  x := JwEncryptString('123','',false,false,
      @Blob);
  if x = '' then;
  x2 := JwDecryptString(x,false,@Blob);
  if x2 = x then;
  TJwRandomDataGenerator.FreeRandomData(Blob);
end;

procedure Test2;
var Data : PAnsiChar;
    Size : Cardinal;
begin
  Size := 7;
  Data := PAnsiChar(LocalAlloc(LPTR,Size));

  TJwEncryptMemory.EncryptMemory(Pointer(Data),
    Size, [], mtLocal);

  TJwEncryptMemory.DecryptMemory(Pointer(Data),
    Size, [], mtLocal);
end;



{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}




initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
//  Test2;
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
