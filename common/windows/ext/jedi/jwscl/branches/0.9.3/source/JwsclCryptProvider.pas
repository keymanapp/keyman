{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides access to the parts of the Microsoft Cryptographic API (CAPI)
which depend on cryptographic service providers (CSPs).

Author
Philip Dittmann


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
The Initial Developer of the Original Code is Philip Dittmann.
Portions created by Philip Dittmann are Copyright (C) Philip Dittmann. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclCryptProvider;
//do not move header comment from above unit declaration!
{$INCLUDE ..\includes\Jwscl.inc}


interface


uses
  SysUtils, Classes,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclResource,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}

type
  {Provides access to cryptographic service providers }
  TJwCryptProvider = class
  protected
    //@exclude
    fCSPHandle: TJwCSPHandle;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
    //@exclude
    function GetName: TJwString;
    //@exclude
    function GetKeyContainerName: TJwString;
  public
    {<B>Create</B> retrieves a handle to the specified CSP using CryptAcquireContext.
     @param KeyContainerName The name of the key container to be used
            for subsequent operations. Can be '' to specify the default name.
            Must be '' if Flags contains ccfVerifyContext. 
     @param CSPName The name of the CSP. If you specify '', the default will be used. 
     @param The type of the CSP to acquire 
     @param Flags Special flags to use with the call to CryptAcquireContext 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails.}
    constructor Create(const KeyContainerName, CSPName: TJwString;
        CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet); overload;

    {<B>Create</B> duplicates an existing CSP and increments its reference count.
     @param OldCSP The CSP to duplicate 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails.}
    constructor Create(const OldCSP: TJwCryptProvider); overload;

    {<B>Destroy</B> releases the handle to the CSP.
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails.}
    destructor Destroy; override;

    {This function can be used to retrieve certain parameters
     of the CSP. It is mostly for internal use.
     See http://msdn2.microsoft.com/en-us/library/ms938096.aspx for
     more information.}
    function GetProvParam(dwParam: Cardinal; pbData: Pointer; var pdwDataLen: Cardinal; Flags: Cardinal): BOOL;

    {<B>DeleteKeyset</B> deletes the specified keyset using CryptAcquireContext with
     the flag CRYPT_DELETEKEYSET.
     @param KeysetName The name of the keyset to delete. 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails.}
    class procedure DeleteKeyset(const KeysetName: TJwString);

    {<B>GetDefaultProvider</B> obtains the name of the default CSP of the specified type.
     @param ProviderType The type of CSP for which the default
            is to retrieve 
     @param MachineDefault If true, the machine default provider is returned.
            Otherwise, the user default is returned. 
     @return <B>GetDefaultProvider</B> returns the name of the default provider of 
             the specified type.)
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails
             because there is no default provider or for other reasons. }
    class function GetDefaultProvider(ProviderType: TJwCSPType;
      MachineDefault: Boolean): TJwString; //static;
    {The directive static is necessary if you want to use
     the class property DefaultProvider.}

    {Sets the default CSP either for the current user or the machine
     @param NewDefault The name of the new default provider 
     @param ProviderType The type of CSP for which the specified provider
            should be the default 
     @param MachineDefault If true, the machine default provider is set.
            Otherwise, the user default is set.
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    class procedure SetDefaultProvider(ProviderType: TJwCSPType;
      MachineDefault: Boolean; const NewDefault: TJwString); //static;
    {The directive static is necessary if you want to use
     the class property DefaultProvider.}

    (*{This construct can be used since Delphi 7. You have to remove
     the comment on 'static' after GetDefaultProvider and
     SetDefaultProvider to compile!}
    class property DefaultProvider[ProviderType: TJwCSPType; MachineDefault: Boolean]: TJwString
      read GetDefaultProvider write SetDefaultProvider; *)

    {Deletes the default provider setting for the specified type
     @param ProviderType The CSP type for which the default setting should
            be deleted. 
     @param MachineDefault If true, the machine default setting is deleted.
            Otherwise, the user default setting is deleted. 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    class procedure DeleteDefaultProvider(ProviderType: TJwCSPType;
      MachineDefault: Boolean);

    {Enumerates all providers installed on the machine
     @return <B>EnumProviders</B> returns an array of TJwEnumProvider records, which
             contain the provider name and type. 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    class function EnumProviders: TJwEnumProviderArray;

    {Enumerates all provider types on the machine
     @return <B>EnumProviderTypes</B> returns an array of TJwEnumProvider records containing
             the type as a member of the TJwCSPType enumeration and the
             name as a human-readable string. 
      raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    class function EnumProviderTypes: TJwEnumProviderArray;

    {Enumerates all the algorithms supported by the CSP.
     @return <B>EnumAlgorithms</B> returns an array of TJwEnumAlgorithmsEntry records.
             See TJwEnumAlgorithmsEntry  for more information 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    function EnumAlgorithms: TJwEnumAlgorithms;

    {<B>GetRandomData</B> fills a buffer with random data. The data is usually
     far better than the one you retrieve by calling System.Random,
     although the exact implementation depends on the CSP.
     @param Random Pointer to the buffer to be filled with random
            data. The current contents of the buffer are used as a seed.
            Nevertheless, it is not necessary to supply a good seed in
            the buffer. 
     @param Length Size of the buffer to be filled 
     raises
 EJwsclCSPApiException:  will be raised if the underlying Windows call fails. }
    procedure GetRandomData(Random: Pointer; const Length: Cardinal);

    {<B>CSPHandle</B> is the handle to the CSP.}
    property CSPHandle: TJwCSPHandle read fCSPHandle;

    {The name of the CSP as specified in the CSPName parameter in the call to
     create 
     EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    property Name: TJwString read GetName;

    {The name of the key container as specified in the KeyContainerName parameter
     in the call to create 
     EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    property KeyContainerName: TJwString read GetName;
  end;

  TJwCryptKey = class;

  {<B>TJwHash</B> is a class to compute hashes. 
   Both keyless and keyed algorithms are supported.
   Hashes can also be used to sign data.
   }
  TJwHash = class
  protected
    //@exclude
    fHashHandle: TJwHashHandle;
    //@exclude
    fProvider:   TJwCryptProvider;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
    //<B>GetHashParam</B> calls the WinAPI function CryptGetHashParam. It
    //is protected since the functionality is encapsulated by
    //GetHashLength , RetrieveHash  and Algorithm .
    function GetHashParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
    //@exclude
    function GetAlgorithm: TJwHashAlgorithm;
  public
    {Creates a new hash object
     @param Alg Specifies the algorithm to use 
     @param CSP The provider the hash uses. If it is nil,
            the default CSP of type ctRsaFull is used. Be aware that you
            will not be able to call the GetSignatureLength  and
            Sign  functions in this case since the CSP is created
            with the flag ccfVerifyContext meaning that there are no
            lasting key pairs in the CSP.
            The CSP can be freed during lifetime of the hash
            since the hash increments the reference count of the CSP. 
     @param The key used for data hashing if a keyed algorithm is specified.
            Otherwise this must be nil. 
     raises
 EJwsclHashException:  will be raised if there is a key specified
             for a non-keyed algorithm or if no key is specified for a keyed
             algorithm.
      EJwsclHashApiException: will be raised if the underlying Windows call fails.}
    constructor Create(Alg: TJwHashAlgorithm; const CSP: TJwCryptProvider = nil; const Key: TJwCryptKey = nil);

    {Destroys the hash object and releases the CSP
     raises
 EJwsclHashApiException:  will be raised if the underlying Windows call fails.}
    destructor Destroy; override;

    {Adds data to the hash object
     @param Data Pointer to the data to be added
     @param Size Specifies the size of the data
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails due to a
             previous call to RetrieveHash or
             Sign or for other reasons.}
    procedure HashData(Data: Pointer; Size: Cardinal);

    {Adds data to the hash object
     @param Stream Stream containing the data to be hashed
     @param Size defines how much data is hashed from the current stream position.
       If set to zero (0) the method sets the stream position to beginning
       and hashes the whole stream.
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails due to a
             previous call to RetrieveHash or
             Sign or for other reasons.}
    procedure HashStream(Stream: TStream; const Size : Int64 = 0);

    {<B>GetHashLength</B> returns the size of the hash value in bytes.
     This value is constant for each algorithm.
     @return <B>GetHashLength</B> returns the length of the hash value. 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails.}
    function GetHashLength: Cardinal;

    {<B>RetrieveHash</B> computes the hash of the data previously added to
     the hash using HashData . After a successful call
     to this function you cannot add more data to the hash.
     Any additional calls to HashData will fail.
     @param Hash Specifies the storage in which to put the
            computed hash value 
     @param Len Specifies the size of Hash. The procedure will
            raise an exception if Len is not big enough. 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails because the specified
             buffer is to small or for other reasons.}
    procedure RetrieveHash(Hash: Pointer; var Len: Cardinal); overload;

//    procedure RetrieveHashArray(out Hash); overload;

    {<B>RetrieveHash</B> computes the hash of the data previously added to
     the hash using HashData . After a successful call
     to this function you cannot add more data to the hash.
     Any additional calls to HashData will fail.
     The buffer returned by this function must be freed using
     FreeBuffer .
     @param Len Returns the size of the hash 
     @return Pointer to a buffer containing the hash 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails.}
    function RetrieveHash(out Len: Cardinal): Pointer; overload;

    {This function returns the length of the signature for subsequent
     calls to the Sign  routine.
     @return <B>GetSignatureLength</B> returns the length of the signature. 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails.}
    function GetSignatureLength: Cardinal;

    {<B>Sign</B> computes a signature of the data in the hash.
     The used CSP must have permanent key pairs and thus
     have not been created with the ccfVerifyContext flag.
     @param Signature The buffer the signature is stored in 
     @param Len The length of the buffer specified in Signature.
            The actual size of the signature is returned here. 
     @param Key A CSP usually has two key pairs. This parameter specifies
            which should be used. 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails because the specified
             buffer is to small or for other reasons. }
    procedure Sign(Signature: Pointer; var Len: Cardinal; Key: TJwKeyPairType = kptSignature); overload;

    {<B>Sign</B> computes a signature of the data in the hash.
     The used CSP must have permanent key pairs and thus
     have not been created with the ccfVerifyContext flag.
     The buffer returned by this function must be freed using
     FreeBuffer .
     @param Len The length of the returned buffer 
     @param Key A CSP usually has two key pairs. This parameter specifies
            which should be used. 
     @return The buffer containing the signature. It should be freed
              using FreeBuffer  when it is no longer needed. 
     raises
 EJwsclHashApiException:  will be raised if the
             underlying Windows call fails. }
    function Sign(out Len: Cardinal; Key: TJwKeyPairType = kptSignature): Pointer; overload;

    {<B>VerifySignature</B> verifies that the given signature is correct for the
     data previously added to the hash using HashData .
     @param Signature The signature which should be verified 
     @param Len The length of the signature 
     @param Key The public key that corresponds to the private key with
          with which the signature was created 
     raises
 EJwsclHashApiException:  will be raised if the underlying
             Windows call fails due to an incorrect signature or for other
             reasons. In case of an incorrect signature, the property
             LastError of the exception object will have the value NTE_BAD_SIGNATURE. }
    procedure VerifySignature(Signature: Pointer; Len: Cardinal; Key: TJwCryptKey);

    {<B>FreeBuffer</B> frees buffer returned by previous calls to
     RetrieveHash  and Sign .
     @Param Buffer The buffer to be freed }
    class procedure FreeBuffer(Buffer: Pointer);

    {The algorithm specified in the call to create 
     EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    property Algorithm: TJwHashAlgorithm read GetAlgorithm;

    {<B>HashHandle</B> is the handle to the hash object.}
    property HashHandle: TJwHashHandle read fHashHandle;
  end;

  {<B>TJwCryptKey</B> encapsulates CAPI keys. 
   Keys for symmetric and asymmetric algorithms are supported.
   They can be used for hash computing. Keys can be created
   by retrieving the user keys of a CSP, randomly generating
   a new key, deriving from base data and importing from
   a data blob.}
  TJwCryptKey = class
  protected
    //@exclude
    fHandle: TJwKeyHandle;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
    //@exclude
    function GetDWordKeyParam(const Param: Integer): Cardinal;
  public
    {Duplicates an existing key.
    @param OldKey The key to duplicate 
    raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    constructor Create(OldKey: TJwCryptKey);

    {Gets one of the two standard key pairs of the specified CSP.
     @param CSP The cryptographic service provider 
     @param Key The key pair you want to retrieve 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    constructor GetUserKey(CSP: TJwCryptProvider; Key: TJwKeyPairType);

    {Imports a key from a data blob
     @param CSP The cryptographic service provider 
     @param Data The data blob 
     @param DataLen The size of the data blob. 
     @param PubKey The key with which the data blob shall be decrypted.
            This parameter must be nil if no encryption key has been specified
            upon key export. 
     @param Flags Special flags to use with the WinAPI funtion call 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    constructor Import(CSP: TJwCryptProvider; Data: Pointer; DataLen: Cardinal;
      PubKey: TJwCryptKey; Flags: TJwKeyFlagSet);

    {Randomly generates a key
    @param CSP The cryptographic service provider
    @param Alg The algorithm for which the key is to be used
    @param Flags Flags to use with the WinAPI call
    @param Length The length of the key
    raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    constructor Generate(CSP: TJwCryptProvider; Alg: TJwEncryptionAlgorithm;
      Flags: TJwKeyFlagSet; Length: Word);

    {Randomly generates a public/private key pair and stores
    it in the key container of the specified CSP. The new key
    replaces the old key (if existing) in the container. Thus,
    this constructor is generally used in the following way:
    <code lang="Delphi">
    try
      //try to get the key pair
      Key := TJwCryptKey.GetUserKey(CSP, KeyType)
    except
      on E: EJwsclKeyApiException do
        if E.LastError = NTE_NO_KEY then
          //No key pair existed: We create one and put in the
          //container.
          Key := TJwCryptKey.GenerateContainerKey(CSP, KeyType, [], 0)
        else
          raise;
    end;
    </code>
    @param CSP The cryptographic service provider
    @param KeyPair They key pair to replace in the key container
                   of the CSP.
    @param Flags Flags to use with the WinAPI call
    @param Length The length of the key
    raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}

    constructor GenerateContainerKey(CSP: TJwCryptProvider;
      KeyPair: TJwKeyPairType; Flags: TJwKeyFlagSet; Length: Word);

    {Derives a key from a specified seed
    @param CSP The cryptographic service provider 
    @param Alg The algorithm for which the key is to be used 
    @param Flags Flags to use with the WinAPI call 
    @param Length The length of the key 
    @param BaseData The base data from which the key shall be derived.
                    The data must have been added to the hash object using
                    TJwHash.HashData  
    raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    constructor Derive(CSP: TJwCryptProvider; Alg: TJwEncryptionAlgorithm;
      Flags: TJwKeyFlagSet; Length: Word; BaseData: TJwHash);

    {Destroys the key
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    destructor Destroy; override;

    {This function can be used to retrieve certain parameters
     of the key. It is mostly for internal use.
     See http://msdn2.microsoft.com/en-us/library/aa379949.aspx for
     more information.}
    function GetKeyParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;

    {<B>GetExportKeyLength</B> can be used to determine the length of the export blob for
     subsequent calls to ExportKey .
     @param ExpKey The key with which the data blob is encrypted. It depends on BlobType
            and the CSP which keys, and whether nil keys, are allowed. 
     @param BlobType The type of blob to which the key shall be exported 
     @param Flags Export flags 
     @return The length of the export blob 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    function GetExportKeyLength(ExpKey: TJwCryptKey; BlobType: TJwKeyExportKind; Flags: TJwKeyFlagSet): Cardinal;

    {Exports the key to a data blob
     @param ExpKey The key with which the data blob is encrypted. It depends on BlobType
            and the CSP which keys, and wether nil keys, are allowed. 
     @param BlobType Specifies how the key should be exported. 
     @param Flags Export flags 
     @param Blob The blob in which the export data is stored 
     @param BlobLen Specify the size of Blob here. it will be set to
            the actual length of the exported key upon return.
            An exception will be raised if BlobLen is to small. 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails becaus the specified
             buffer is to small or for other reasons.}
    procedure ExportKey(ExpKey: TJwCryptKey; BlobType: TJwKeyExportKind;
      Flags: TJwKeyFlagSet; Blob: Pointer; var BlobLen: Cardinal); overload;

    {Exports the key to a data blob which is returned. It must be freed using
     FreeBuffer .
     @param ExpKey The key with which the data blob is encrypted. It depends on BlobType
            and the CSP which keys, and whether nil keys, are allowed. 
     @param BlobType Specifies how the key should be exported. 
     @param Flags Export flags 
     @param BlobLen This parameter will be set to the size of the returned
            data blob. 
     @return A pointer to the allocated data blob with the key data is
             returned. You have to free the blob using FreeBuffer 
             when it is no longer needed. 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    function  ExportKey(ExpKey: TJwCryptKey; BlobType: TJwKeyExportKind;
      Flags: TJwKeyFlagSet; out BlobLen: Cardinal): Pointer; overload;

    {This function encrypts a data block. It trashes the original
     data.
     @param Hash Specify an instance of TJwHash to encrypt and
            hash data simultaneously, e.g. for signing it.
            This parameter can be nil. 
     @param FinalBlock Use this parameter to indicate that this
            is the last block in a stream of data to encrypt. 
     @param Flags Special encryption flags 
     @param Data Pointer to the data to be encrypted. The ciphertext is also
            returned in this buffer. 
     @param DataLen The length of the plaintext. The length of the
            ciphertext is returned here. 
     @param BufSize Specify the size of the Data buffer here. 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    procedure Encrypt(const Hash: TJwHash; FinalBlock: Boolean; Flags: TJwKeyFlagSet;
      Data: Pointer; var DataLen: Cardinal; BufSize: Cardinal); overload;

    {This function decrypts a data block. It trashes the encrypted
     data.
     @param Hash Specify an instance of TJwHash to decrypt and
            hash data simultaneously, e.g. for verifying a signature.
            This parameter can be nil. 
     @param FinalBlock Use this parameter to indicate that this
            is the last block in a stream of data to decrypt. 
     @param Flags Special decryption flags 
     @param Data Pointer to the data to be decrypted. The plaintext is also
            returned in this buffer. 
     @param DataLen The length of the ciphertext. The length of the
            plaintext is returned here as well. 
     raises
 EJwsclKeyApiException:  will be raised if the
             underlying Windows call fails.}
    procedure Decrypt(const Hash: TJwHash; FinalBlock: Boolean; Flags: TJwKeyFlagSet;
      Data: Pointer; var DataLen: Cardinal); overload;

    {<B>FreeBuffer</B> frees buffer returned by previous calls to
     ExportKey .
     @Param Buffer The buffer to be freed }
    class procedure FreeBuffer(Buffer: Pointer);

    {The handle to the key}
    property Handle: TJwKeyHandle read fHandle;

    {The length of the key, as specified upon key creation}
    property KeyLen: Cardinal index KP_KEYLEN  read GetDWordKeyParam;

    {The block size of the key. This is zero for stream ciphers.
     If the key is a public/private key pair, the encryption granularity
     is returned.}
    property BlockLen: Cardinal index KP_BLOCKLEN  read GetDWordKeyParam;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses JwsclEnumerations;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

{ TJwCryptProvider }

constructor TJwCryptProvider.Create(const KeyContainerName, CSPName: TJwString;
  CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet);
begin
  inherited Create;
  if not {$IFDEF UNICODE}CryptAcquireContextW{$ELSE}CryptAcquireContextA{$ENDIF UNICODE}(fCSPHandle, TJwPChar(KeyContainerName),
    TJwPChar(CSPName), TJwEnumMap.ConvertCSPType(CSPType),
    TJwEnumMap.ConvertCSPCreationFlags(Flags)) then
    RaiseApiError('Create', 'CryptAcquireContext');
end;

constructor TJwCryptProvider.Create(const OldCSP: TJwCryptProvider);
begin
  inherited Create;
  fCSPHandle := OldCSP.CSPHandle;
  if not CryptContextAddRef(OldCSP.CSPHandle, nil, 0) then
    RaiseApiError('Create', 'CryptCOntextAddRef');
end;

destructor TJwCryptProvider.Destroy;
begin
  if fCSPHandle <> 0 then
    if not CryptReleaseContext(fCSPHandle, 0) then
      RaiseApiError('Destroy', 'CryptReleaseContext');
  inherited;
end;

class procedure TJwCryptProvider.DeleteKeyset(const KeysetName: TJwString);
var DummyHandle: Cardinal;
begin
  if not {$IFDEF UNICODE}CryptAcquireContextW{$ELSE}CryptAcquireContextA{$ENDIF UNICODE}(DummyHandle,
    TJwPChar(KeysetName), nil, PROV_RSA_FULL, CRYPT_DELETEKEYSET) then
    RaiseApiError('DeleteKeyset', 'CryptAcquireContext');
end;

function TJwCryptProvider.GetProvParam(dwParam: Cardinal; pbData: Pointer;
  var pdwDataLen: Cardinal; Flags: Cardinal): BOOL;
begin
  Result := CryptGetProvParam(fCSPHandle, dwParam, pbData, pdwDataLen, Flags);
end;

class procedure TJwCryptProvider.RaiseApiError(const Procname, WinCallname: TJwString);
begin
  raise EJwsclCSPApiException.CreateFmtWinCall(
    '',
    Procname,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallname,
    [Procname]);
end;

class function TJwCryptProvider.GetDefaultProvider(ProviderType: TJwCSPType;
  MachineDefault: Boolean): TJwString;
var Len: Cardinal; Flag: Cardinal;
begin
  Len := 0;
  if MachineDefault then
    Flag := CRYPT_MACHINE_DEFAULT
  else
    Flag := CRYPT_USER_DEFAULT;
  if not CryptGetDefaultProvider(TJwEnumMap.ConvertCSPType(ProviderType), nil, Flag, nil, Len) then
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  SetLength(Result, Len);
  if not {$IFDEF UNICODE}CryptGetDefaultProviderW{$ELSE}CryptGetDefaultProviderA{$ENDIF UNICODE}(TJwEnumMap.ConvertCSPType(ProviderType), nil, Flag, TJwPChar(Result), Len) then
  begin
    Result := '';
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  end;
end;

class procedure TJwCryptProvider.SetDefaultProvider(ProviderType: TJwCSPType;
  MachineDefault: Boolean; const NewDefault: TJwString);
var Flags: Cardinal;
begin
  if MachineDefault then
    Flags := CRYPT_MACHINE_DEFAULT
  else
    Flags := CRYPT_USER_DEFAULT;
  if not {$IFDEF UNICODE}CryptSetProviderExW{$ELSE}CryptSetProviderExA{$ENDIF}(TJwPChar(NewDefault), TJwEnumMap.ConvertCSPType(ProviderType), nil, Flags) then
    RaiseApiError('SetDefaultProvider', 'CryptSetProviderEx');
end;

class procedure TJwCryptProvider.DeleteDefaultProvider(ProviderType: TJwCSPType;
  MachineDefault: Boolean);
var Flags: Cardinal;
begin
  if MachineDefault then
    Flags := CRYPT_DELETE_DEFAULT or CRYPT_MACHINE_DEFAULT
  else
    Flags := CRYPT_DELETE_DEFAULT or CRYPT_USER_DEFAULT;
  if not CryptSetProviderEx(nil, TJwEnumMap.ConvertCSPType(ProviderType), nil, Flags) then
    RaiseApiError('SetDefaultProvider', 'CryptSetProviderEx');
end;

class function TJwCryptProvider.EnumProviders: TJwEnumProviderArray;
var i: integer; Err: Cardinal; Len: Cardinal; ProvType: Cardinal;
begin
  i := 0;
  Len := 0;
  Err := ERROR_SUCCESS;
  repeat
    if not CryptEnumProviders(i, nil, 0, ProvType, nil, Len) then
    begin
      Err := GetLastError;
      if Err <> ERROR_NO_MORE_ITEMS then
      begin
        SetLastError(Err);
        RaiseApiError('EnumProviders', 'CryptEnumProviders');
      end;
    end
    else
    begin
      SetLength(Result, i+1);
      SetLength(Result[i].Name, Len);
      if not {$IFDEF UNICODE}CryptEnumProvidersW{$ELSE}CryptEnumProvidersA{$ENDIF}(i, nil, 0, ProvType, TJwPChar(Result[i].Name), Len) then
        RaiseApiError('EnumProviders', 'CryptEnumProviders');
      Result[i].ProviderType := TJwEnumMap.ConvertCSPType(ProvType);
      inc(i);
    end;
  until Err = ERROR_NO_MORE_ITEMS;
end;

class function TJwCryptProvider.EnumProviderTypes: TJwEnumProviderArray;
var i: integer; Err: Cardinal; Len: Cardinal; ProvType: Cardinal;
begin
  i := 0;
  Len := 0;
  Err := ERROR_SUCCESS;
  repeat
    if not CryptEnumProviderTypes(i, nil, 0, ProvType, nil, Len) then
    begin
      Err := GetLastError;
      if Err <> ERROR_NO_MORE_ITEMS then
      begin
        SetLastError(Err);
        RaiseApiError('EnumProviderTypes', 'CryptEnumProviderTypes');
      end;
    end
    else
    begin
      SetLength(Result, i+1);
      SetLength(Result[i].Name, Len);
      if not {$IFDEF UNICODE}CryptEnumProvidersW{$ELSE}CryptEnumProvidersA{$ENDIF}(i, nil, 0, ProvType, TJwPChar(Result[i].Name), Len) then
        RaiseApiError('EnumProviderTypes', 'CryptEnumProviderTypes');
      Result[i].ProviderType := TJwEnumMap.ConvertCSPType(ProvType);
      inc(i);
    end;
  until Err = ERROR_NO_MORE_ITEMS;
end;

function TJwCryptProvider.GetName: TJwString;
var Len: Cardinal;
begin
  Len := 0;
  if not GetProvParam(PP_NAME, nil, Len, 0) then
    RaiseApiError('GetName', 'CryptGetProvParam');
  SetLength(Result, Len);
  if not GetProvParam(PP_NAME, Pointer(Result), Len, 0) then
    RaiseApiError('GetName', 'CryptGetProvParam');
end;

function TJwCryptProvider.GetKeyContainerName: TJwString;
var Len: Cardinal;
begin
  Len := 0;
  if not GetProvParam(PP_CONTAINER, nil, Len, 0) then
    RaiseApiError('GetKeyContainerName', 'CryptGetProvParam');
  SetLength(Result, Len);
  if not GetProvParam(PP_CONTAINER, Pointer(Result), Len, 0) then
    RaiseApiError('GetKeyContainerName', 'CryptGetProvParam');
end;

function TJwCryptProvider.EnumAlgorithms: TJwEnumAlgorithms;
var EnumAlgs: PROV_ENUMALGS_EX; Size: Cardinal; Err: Cardinal;
begin
  Size := SizeOf(EnumAlgs);
  GetProvParam(PP_ENUMALGS_EX, @EnumAlgs, Size, CRYPT_FIRST);
  while true do
  begin
    SetLength(Result, Length(Result)+1);
    with Result[High(Result)] do
    begin
      AlgId := EnumAlgs.aiAlgid;
      HashAlgType := TJwEnumMap.ConvertHashAlgorithm(EnumAlgs.aiAlgid);
      HashAlgorithm := HashAlgType <> haUnknown;
      if not HashAlgorithm then
        EncrAlgType := TJwEnumMap.ConvertEncryptionAlgorithm(EnumAlgs.aiAlgid);
      DefaultKeyLen := EnumAlgs.dwDefaultLen;
      MinKeyLen := EnumAlgs.dwMinLen;
      MaxKeyLen := EnumAlgs.dwMaxLen;
      ProtocolNum := EnumAlgs.dwProtocols;
      SetString(ShortName, EnumAlgs.szName,     EnumAlgs.dwNameLen);
      SetString(LongName,  EnumAlgs.szLongName, EnumAlgs.dwLongNameLen);
    end;
    if not GetProvParam(PP_ENUMALGS_EX, @EnumAlgs, Size, 0) then
    begin
      Err := GetLastError;
      if Err <> ERROR_NO_MORE_ITEMS then
      begin
        SetLastError(Err);
        RaiseApiError('EnumAlgorithms', 'CryptGetProvParam');
      end
      else
        break;
    end;
  end;
end;

procedure TJwCryptProvider.GetRandomData(Random: Pointer; const Length: Cardinal);
begin
  if not CryptGenRandom(fCSPHandle, Length, Random) then
    RaiseApiError('GetRandomData', 'CryptGenRandom');
end;

{ TJwHash }

constructor TJwHash.Create(Alg: TJwHashAlgorithm; const CSP: TJwCryptProvider = nil;
  const Key: TJwCryptKey = nil);
var KeyHandle: TJwKeyHandle;
begin
  inherited Create;
  if Assigned(Key) and (Alg in KeylessHashAlgorithms)  then
    raise EJwsclHashException.CreateFmtEx(
      RsNonKeyedHash,
      'Create',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if not Assigned(Key) and not (Alg in KeylessHashAlgorithms) then
    raise EJwsclHashException.CreateFmtEx(
      RsKeyedHashNeedsKey,
      'Create',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);

  if CSP = nil then
    fProvider := TJwCryptProvider.Create('', '', ctRsaFull, [ccfVerifyContext])
  else
    fProvider := TJwCryptProvider.Create(CSP);
  if Assigned(Key) then
    KeyHandle := Key.Handle
  else
    KeyHandle := 0;
  if not CryptCreateHash(fProvider.CSPHandle, TJwEnumMap.ConvertHashAlgorithm(Alg), KeyHandle, 0, fHashHandle) then
    RaiseApiError('Create', 'CryptCreateHash');
end;

destructor TJwHash.Destroy;
begin
  if fHashHandle <> 0 then
    if not CryptDestroyHash(fHashHandle) then
      RaiseApiError('Destroy', 'CryptDestroyHash');
  fProvider.Free;
  inherited;
end;

class procedure TJwHash.FreeBuffer(Buffer: Pointer);
begin
  {Type of memory manager.
  Be aware that we use GetMem in JwLoadHashFromRegistry to create
  a TJwFileHashData record. So change it also there if memory manager is changed.
  }
  FreeMem(Buffer);
end;

class procedure TJwHash.RaiseApiError(const Procname, WinCallname: TJwString);
begin
  raise EJwsclHashApiException.CreateFmtWinCall(
    '',
    ProcName,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallName,
    [ProcName]);
end;

function TJwHash.GetHashParam(dwParam: Cardinal; pbData: PByte;
  var pdwDataLen: Cardinal): BOOL;
begin
  Result := CryptGetHashParam(fHashHandle, dwParam, pbData, pdwDataLen, 0);
end;

function TJwHash.GetAlgorithm: TJwHashAlgorithm;
var Alg: ALG_ID; Size: Cardinal;
begin
  Size := SizeOf(Alg);
  if not GetHashParam(HP_ALGID, @Alg, Size) then
    RaiseApiError('GetAlgorithm', 'CryptGetHashParam');;
  Result := TJwEnumMap.ConvertHashAlgorithm(Alg);
end;

procedure TJwHash.HashData(Data: Pointer; Size: Cardinal);
begin
  if not CryptHashData(fHashHandle, Data, Size, 0) then
    RaiseApiError('HashData', 'CryptHashData');
end;

procedure TJwHash.HashStream(Stream: TStream; const Size : Int64 = 0);
var MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.CopyFrom(Stream, Size);
    HashData(MemStream.Memory, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;

function TJwHash.GetHashLength: Cardinal;
var Size: Cardinal;
begin
  Size := SizeOf(Result);
  if not GetHashParam(HP_HASHSIZE, @Result, Size) then
    RaiseApiError('GetHashLength', 'CryptGetHashParam');
end;

procedure TJwHash.RetrieveHash(Hash: Pointer; var Len: Cardinal);
begin
  if not GetHashParam(HP_HASHVAL, Hash, Len) then
    RaiseApiError('RetrieveHash', 'CryptGetHashParam');
end;

function TJwHash.RetrieveHash(out Len: Cardinal): Pointer;
begin
  Len := GetHashLength;
  GetMem(Result, Len);
  try
    RetrieveHash(Result, Len);
    ReallocMem(Result, Len);
  except
    FreeMem(Result);
    raise;
  end;
end;

function TJwHash.GetSignatureLength: Cardinal;
begin
  Result := 0;
  //the key type should not matter for the signature length,
  //so just take kptSignature here
  Sign(nil, Result, kptSignature);
end;

procedure TJwHash.Sign(Signature: Pointer; var Len: Cardinal;
  Key: TJwKeyPairType = kptSignature);
begin
  if not CryptSignHash(fHashHandle, TJwEnumMap.ConvertKeyPairType(Key), nil, 0, Signature, Len) then
    RaiseApiError('Sign', 'CryptSignHash');
end;

function TJwHash.Sign(out Len: Cardinal;
  Key: TJwKeyPairType = kptSignature): Pointer;
begin
  Len := GetSignatureLength;
  GetMem(Result, Len);
  try
    Sign(Result, Len, Key);
    ReallocMem(Result, Len);
  except
    FreeMem(Result);
    raise;
  end;
end;

procedure TJwHash.VerifySignature(Signature: Pointer; Len: Cardinal; Key: TJwCryptKey);
begin
  if not CryptVerifySignature(fHashHandle, Signature, Len, Key.Handle, nil, 0) then
    RaiseApiError('VerifySignature', 'CryptVerifySignature');
end;

{ TJwCryptKey }
constructor TJwCryptKey.Create(OldKey: TJwCryptKey);
begin
  inherited Create;
  if not CryptDuplicateKey(OldKey.Handle, nil, 0, fHandle) then
    RaiseApiError('Create', 'CryptDuplicateKey');
end;

constructor TJwCryptKey.GetUserKey(CSP: TJwCryptProvider;
  Key: TJwKeyPairType);
begin
  inherited Create;
  if not CryptGetUserKey(CSP.CSPHandle, TJwEnumMap.ConvertKeyPairType(Key), fHandle) then
    RaiseApiError('GetUserKey', 'CryptGetUserKey');
end;

constructor TJwCryptKey.Import(CSP: TJwCryptProvider; Data: Pointer;
  DataLen: Cardinal; PubKey: TJwCryptKey; Flags: TJwKeyFlagSet);
var Key: Cardinal;
begin
  inherited Create;
  if not (Flags<=ImportKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Import',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if Assigned(PubKey) then
    Key := PubKey.Handle
  else
    Key := 0;
  if not CryptImportKey(CSP.CSPHandle, Data, DataLen, Key, TJwEnumMap.ConvertKeyFlagSet(Flags), fHandle) then
    RaiseApiError('Import', 'CryptImportKey');
end;

constructor TJwCryptKey.Generate(CSP: TJwCryptProvider;
  Alg: TJwEncryptionAlgorithm; Flags: TJwKeyFlagSet; Length: Word);
begin
  if not (Flags<=GenerateKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Generate',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);

  inherited Create;
  if not CryptGenKey(CSP.CSPHandle, TJwEnumMap.ConvertEncryptionAlgorithm(Alg), TJwEnumMap.ConvertKeyFlagSet(Flags) or (Length shl 16), fHandle) then
    RaiseApiError('Generate', 'CryptGenKey');
end;

constructor TJwCryptKey.GenerateContainerKey(CSP: TJwCryptProvider; KeyPair: TJwKeyPairType; Flags: TJwKeyFlagSet; Length: Word);
begin
  if not (Flags<=GenerateKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Generate',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  inherited Create;
  if not CryptGenKey(CSP.CSPHandle, TJwEnumMap.ConvertKeyPairType(KeyPair), TJwEnumMap.ConvertKeyFlagSet(Flags) or (Length shl 16), fHandle) then
    RaiseApiError('Generate', 'CryptGenKey');
end;

constructor TJwCryptKey.Derive(CSP: TJwCryptProvider; Alg: TJwEncryptionAlgorithm;
  Flags: TJwKeyFlagSet; Length: Word; BaseData: TJwHash);
begin
  if not (Flags<=DeriveKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Derive',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  inherited Create;
  if not CryptDeriveKey(CSP.CSPHandle, TJwEnumMap.ConvertEncryptionAlgorithm(Alg), BaseData.HashHandle, TJwEnumMap.ConvertKeyFlagSet(Flags) or(Length shl 16), fHandle) then
    RaiseApiError('Derive', 'CryptDeriveKey');
end;

destructor TJwCryptKey.Destroy;
begin
  if fHandle <> 0 then
    if not CryptDestroyKey(fHandle) then
      RaiseApiError('Destroy', 'CryptDestroyKey');
  inherited Destroy;
end;

function TJwCryptKey.GetExportKeyLength(ExpKey: TJwCryptKey;
  BlobType: TJwKeyExportKind; Flags: TJwKeyFlagSet): Cardinal;
//var Key: Cardinal;
begin
  Result := 0;
  ExportKey(ExpKey, BlobType, Flags, nil, Result);
end;

procedure TJwCryptKey.ExportKey(ExpKey: TJwCryptKey; BlobType: TJwKeyExportKind;
  Flags: TJwKeyFlagSet; Blob: Pointer; var BlobLen: Cardinal);
var Key: Cardinal;
begin
  if not (Flags <= ExportKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'ExportKey',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if Assigned(ExpKey) then
    Key := ExpKey.Handle
  else
    Key := 0;
  if not CryptExportKey(fHandle, Key, TJwEnumMap.ConvertKeyExportKind(BlobType), TJwEnumMap.ConvertKeyFlagSet(Flags), Blob, BlobLen) then
    RaiseApiError('GetExportKeyLength', 'CryptExportKey');
end;

function TJwCryptKey.ExportKey(ExpKey: TJwCryptKey; BlobType: TJwKeyExportKind;
  Flags: TJwKeyFlagSet; out BlobLen: Cardinal): Pointer;
begin
  BlobLen := GetExportKeyLength(ExpKey, BlobType, Flags);
  GetMem(Result, BlobLen);
  try
    ExportKey(ExpKey, BlobType, Flags, Result, BlobLen);
    ReallocMem(Result, BlobLen);
  except
    FreeMem(Result);
    raise;
  end;
end;

class procedure TJwCryptKey.RaiseApiError(const Procname: TJwString; const WinCallname: TJwString);
begin
  raise EJwsclKeyApiException.CreateFmtWinCall(
    '',
    ProcName,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallName,
    [ProcName]);
end;

class procedure TJwCryptKey.FreeBuffer(Buffer: Pointer);
begin
  FreeMem(Buffer);
end;

procedure TJwCryptKey.Encrypt(const Hash: TJwHash; FinalBlock: Boolean;
  Flags: TJwKeyFlagSet; Data: Pointer; var DataLen: Cardinal; BufSize: Cardinal);
var HashHandle: Cardinal;
begin
  if not (Flags <= EncryptKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Encrypt',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if Assigned(Hash) then
    HashHandle := Hash.HashHandle
  else
    HashHandle := 0;
  if not CryptEncrypt(fHandle, HashHandle, FinalBlock, TJwEnumMap.ConvertKeyFlagSet(Flags),
           Data, DataLen, BufSize) then
    RaiseApiError('Encrypt', 'CryptEncrypt');
end;

procedure TJwCryptKey.Decrypt(const Hash: TJwHash; FinalBlock: Boolean;
  Flags: TJwKeyFlagSet; Data: Pointer; var DataLen: Cardinal);
var HashHandle: Cardinal;
begin
  if not (Flags <= DecryptKeyFlags) then
    raise EJwsclInvalidFlagsException.CreateFmtEx(
      RsInvalidFlags,
      'Decrypt',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if Assigned(Hash) then
    HashHandle := Hash.HashHandle
  else
    HashHandle := 0;
  if not CryptDecrypt(fHandle, HashHandle, FinalBlock,
           TJwEnumMap.ConvertKeyFlagSet(Flags), Data, DataLen) then
    RaiseApiError('Decrypt', 'CryptDecrypt');
end;

function TJwCryptKey.GetKeyParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
begin
  Result := CryptGetKeyParam(fHandle, dwParam, pbData, pdwDataLen, 0);
end;

function TJwCryptKey.GetDWordKeyParam(const Param: integer): Cardinal;
var Size: Cardinal;
begin
  Size := 4;
  if not GetKeyParam(Param, @Result, Size) then
    RaiseApiError('GetDWordKeyParam', 'CryptGetKeyParam');
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
