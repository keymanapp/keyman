{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides access to authenticode certificates

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
The Original Code is JwsclCertificates.pas.

The Initial Developer of the Original Code is Philip Dittmann.
Portions created by Philip Dittmann are Copyright (C) Philip Dittmann. All rights reserved.

See Jwscl.inc for Vista related stuff!

}

unit JwsclCertificates;

{$INCLUDE ..\includes\Jwscl.inc}



interface
uses SysUtils, Contnrs, Classes,
  Windows, Dialogs,
  jwaWindows, JwsclResource, JwsclUtils, JwaVista,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl,
  JwsclDescriptor, JwsclEnumerations,
  JwsclVersion, JwsclConstants,
  JwsclStrings;

//yet to be declared:
//EJwsclCertApiException = class(EJwsclSecurityException) in JwsclExceptions.pas
//RsUNCertificates = 'JwsclCertificates.pas' in JwsclResource.pas
type
  {This class encapsulates authenticode certificates.}
  TJwCertificate = class
  private
    fCertContext: PCertContext;
  protected
    function GetSimpleName(NameFlag: Integer): TJwString;
    function GetDateTime(Index: Integer): TDateTime;

    class procedure RaiseApiError(const Procname, WinCallname: TJwString; ExceptClass: EJwsclExceptionClass = nil);
  public
    {<B>CreateFromPEFile</B> retrieves a certificate from a signed file
     in the portable executable format, i.e. files with the extensions
     exe, dll, ocx, sys and more.
     Note that the signature is not checked for validity. You can
     use the JwIsFileTrusted function to do this.
     @Param FileName FileName is the path to the file of which the signature
       is to be retrieved.
     @Param Index A PE file can be signed by several entities. Index is
       the zero-based number of the certificate to be retrieved.
     raises EJwsclInvalidIndex: If Index exceeds the number of certificates
       found in the file - 1
     raises EJwsclWinCallFailed: If the call to CreateFile fails
       due to an invalid path, insufficient rights or similar
     raises EJwsclCertApiException: If a call to a certificate
       api function fails
     }
    constructor CreateFromPEFile(FileName: TJwString; Index: Cardinal);

    {<B>CreateFromFile</B> retrieves a certificate from a signed file
     of any format supported by WinVerifyTrust, including PE files.
     Unlike CreateFromPEFile this constructor does raise an exception
     if the signature is not valid.
     @Param FileName FileName is the path to the file of which the signature
       is to be retrieved.
     @Param idxSigner Files can be signed by several entities. idxSigner
       is the zero-based number of the certificate to be retrieved.
     raises EJwsclCertApiException: If a call to a certificate api
       function, most notably WinVerifyTrust, fails
    }
    constructor CreateFromFile(FileName: TJwString; idxSigner: Cardinal);



    {<B>Destroy</B> destroys the instance and frees the certificate context.
    raises EJwsclCertApiException: If the underlying Windows call fails
    }
    destructor Destroy; override;

    {<B>GetPECertCount</B> returns the number of certificates in
     signed file in the portable executable format.
     @Param FileName FileName is the path to the file
     @Return Returns the number of certificates in the specified
       file

     raises EJwsclWinCallFeiled: If the call to CreateFile fails
       due to an invalid path, insufficient rights or similar
     raises EJwsclCertApiException: If the call to
       ImageEnumerateCertificates fails
    }
    class function GetPECertCount(FileName: TJwString): Cardinal;

    {<B>SimpleIssuerName</B> is the name of the issuer of the certificate in
    a simple format as specified in the certificate.
    }
    property SimpleIssuerName: TJwString index CERT_NAME_ISSUER_FLAG read GetSimpleName;

    {<B>SimpleSubjectName</B> is the name of the subject of the certificate in
    a simple format as specified in the certificate.
    }
    property SimpleSubjectName: TJwString index 0 read GetSimpleName;

    {<B>NotBefore</B> is the beginning of the timespan in which the
    certificate is valid.
    raises EJwsclWinCallFailed: The getter might theoretically
    raise this exception if the call to FileTimeToSystemTime fails,
    but normally this should not happen.}
    property NotBefore: TDateTime index 0 read GetDateTime;

    {<B>NotBefore</B> is the end of the timespan in which the
    certificate is valid.
    raises EJwsclWinCallFailed: The getter might theoretically
    raise this exception if the call to FileTimeToSystemTime fails,
    but normally this should not happen.}
    property NotAfter:  TDateTime index 1 read GetDateTime;

    {<B>CertContext</B> is the internal pointer to the TCertContext structure.
    Usually it is not necessary to access it directly.}
    property CertContext: PCertContext read fCertContext;
  end;

//<B>JwIsFileTrusted</B> checks whether the given file has a valid
//certificate.
//@param FileName The file to be checked
//@return Returns true if the file is trusted
function JwIsFileTrusted(FileName: TJwString): Boolean;

//<B>JwCheckFileSignature</B> gives a detailed trust status for
//a given file.
//@param FileName The file to be checked
//@param Res The trust result returned by WinVerifyTrust. This
//  value is provider-specific. Only a value of zero should be
//  considered a successful return.
procedure JwCheckFileSignature(FileName: TJwString; out Res: Integer);

implementation

procedure JwCheckFileSignature(FileName: TJwString; out Res: Integer);
var WintrustData: TWintrustData; FileInfo: TWintrustFileInfo;
begin
  with WintrustData do
  begin
    cbStruct := SizeOf(WintrustData);
    pPolicyCallbackData := nil;
    pSIPClientData := nil;
    dwUIChoice := WTD_UI_NONE;
    fdwRevocationChecks := WTD_REVOKE_WHOLECHAIN;
    dwUnionChoice := WTD_CHOICE_FILE;
    InfoUnion.pFile := @FileInfo;
    dwStateAction := WTD_STATEACTION_IGNORE;
    hWVTStateData := 0;
    pwszUrlReference := nil;
    dwProvFlags := WTD_SAFER_FLAG;
    dwUIContext := 0;
  end;
  with FileInfo do
  begin
    cbStruct := SizeOf(FileInfo);
    pcwszFilePath := PWideChar(WideString(FileName));
    hFile := 0;
    pgKnownSubject := nil;
  end;

  Res := WinVerifyTrust(0, WINTRUST_ACTION_GENERIC_VERIFY_V2, WintrustData);
end;


function JwIsFileTrusted(FileName: TJwString): Boolean;
var Res: Integer;
begin
  JwCheckFileSignature(FileName, Res);
  Result := Res = 0;
end;



constructor TJwCertificate.CreateFromPEFile(FileName: TJwString; Index: Cardinal);
var FileHandle: THandle; Cert: PWinCertificate; Count: Cardinal;
    VMSParam: CRYPT_VERIFY_MESSAGE_PARA;
begin
  FileHandle := {$IFDEF UNICODE}CreateFileW{$ELSE}CreateFileA{$ENDIF}(TJwPChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then
    RaiseApiError('CreateFromPEFile', 'CreateFile', EJwsclWinCallFailedException);
  try
    if not ImageEnumerateCertificates(FileHandle, CERT_SECTION_TYPE_ANY, Count, nil, 0) then
      RaiseApiError('CreateFromPEFile', 'ImageEnumerateCertificates');
    if Index >= Count then
      raise EJwsclInvalidIndex.Create('');
    GetMem(Cert, SizeOf(TWinCertificate));
    try
      if not ImageGetCertificateHeader(FileHandle, Index, Cert) then
        RaiseApiError('CreateFromPEFile', 'ImageGetCertificateHeader');
      Count := Cert.dwLength + SizeOf(TWinCertificate) -1;
      ReallocMem(Cert, Count);
      if not ImageGetCertificateData(FileHandle, Index, Cert, Count) then
        RaiseApiError('CreateFromPEFile', 'ImageGetCertificateHeader');

      ZeroMemory(@VMSParam, SizeOf(VMSParam));
      VMSParam.cbSize := SizeOf(VMSParam);
      VMSParam.dwMsgAndCertEncodingType := X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;
      if not CryptVerifyMessageSignature(@VMSParam, 0, @Cert^.bCertificate[0], Cert.dwLength, nil, nil, @fCertContext) then
        RaiseApiError('CreateFromPEFile', 'CryptVerifyMessageSignature');
    finally
      FreeMem(Cert);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;

constructor TJwCertificate.CreateFromFile(FileName: TJwString; idxSigner: Cardinal);
var WintrustData: TWintrustData; FileInfo: TWintrustFileInfo;
    ProvData: PCRYPT_PROVIDER_DATA; Sgnr: PCRYPT_PROVIDER_SGNR;
    CertData: PCRYPT_PROVIDER_CERT;
    TrustRes: Integer;
begin
  with WintrustData do
  begin
    cbStruct := SizeOf(WintrustData);
    pPolicyCallbackData := nil;
    pSIPClientData := nil;
    dwUIChoice := WTD_UI_NONE;
    fdwRevocationChecks := WTD_REVOKE_WHOLECHAIN;
    dwUnionChoice := WTD_CHOICE_FILE;
    InfoUnion.pFile := @FileInfo;
    dwStateAction := WTD_STATEACTION_VERIFY;
    hWVTStateData := 0;
    pwszUrlReference := nil;
    dwProvFlags := WTD_SAFER_FLAG;
    dwUIContext := 0;
  end;
  with FileInfo do
  begin
    cbStruct := SizeOf(FileInfo);
    pcwszFilePath := PWideChar(WideString(FileName));
    hFile := 0;
    pgKnownSubject := nil;
  end;

  TrustRes := WinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2, WintrustData);
  try
    if TrustRes <> 0 then //Do this check inside the try clause!
      RaiseApiError('CreateFromFile', 'WinVerifyTrust');
    ProvData := WTHelperProvDataFromStateData(WintrustData.hWVTStateData);
    if ProvData = nil then
      RaiseApiError('CreateFromFile', 'WTHelperProvDataFromStateData');
    Sgnr := WTHelperGetProvSignerFromChain(ProvData, idxSigner, False, 0);
    if Sgnr = nil then
      RaiseApiError('CreateFromFile', 'WTHelperGetProvSignerFromChain');
    CertData := WTHelperGetProvCertFromChain(Sgnr, 0);
    if CertData = nil then
      RaiseApiError('CreateFromFile', 'WTHelperGetProvCertFromChain');

    //duplicate the context: this one will be freed in the finally clause
    fCertContext := CertDuplicateCertificateContext(CertData^.pCert);
  finally
    //release state data
    WintrustData.dwStateAction := WTD_STATEACTION_CLOSE;
    WinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2, WintrustData);
  end;
end;


destructor TJwCertificate.Destroy;
begin
  if fCertContext <> nil then
    if not CertFreeCertificateContext(fCertContext) then
      RaiseApiError('Destroy', 'CertFreeCertificateContext');
end;

class function TJwCertificate.GetPECertCount(FileName: string): Cardinal;
var FileHandle: Cardinal;
begin
  FileHandle := {$IFDEF UNICODE}CreateFileW{$ELSE}CreateFileA{$ENDIF}(TJwPChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then
    RaiseApiError('CreateFromPEFile', 'CreateFile', EJwsclWinCallFailedException);
  try
    if not ImageEnumerateCertificates(FileHandle, CERT_SECTION_TYPE_ANY, Result, nil, 0) then
      RaiseApiError('CreateFromPEFile', 'ImageEnumerateCertificates');
  finally
    CloseHandle(FileHandle);
  end;
end;

class procedure TJwCertificate.RaiseApiError(const Procname: TJwString; const WinCallname: TJwString; ExceptClass: EJwsclExceptionClass = nil);
begin
  if ExceptClass = nil then
    ExceptClass := EJwsclCertApiException;

  raise ExceptClass.CreateFmtWinCall(
    '',
    Procname,
    ClassName,
    RsUNCertificates,
    0,
    True,
    WinCallname,
    [Procname]);
end;

function TJwCertificate.GetSimpleName(NameFlag: Integer): TJwString;
begin
  SetLength(Result, {$IFDEF UNICODE}CertGetNameStringW{$ELSE}CertGetNameStringA{$ENDIF}(fCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, NameFlag, nil, nil, 0) - 1);
  {$IFDEF UNICODE}CertGetNameStringW{$ELSE}CertGetNameStringA{$ENDIF}(fCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, NameFlag, nil, TJwPChar(Result), Length(Result) + 1)
end;

function TJwCertificate.GetDateTime(Index: Integer): TDateTime;
var FileTime: TFileTime; SystemTime: TSystemTime;
begin
  with fCertContext^.pCertInfo^ do
    if Index = 0 then
      FileTime := NotBefore
    else
      FileTime := NotAfter;
  if not FileTimeToSystemTime(FileTime, SystemTime) then
    RaiseApiError('GetDateTime', 'FileTimeToSystemTime', EJwsclWinCallFailedException);
  Result := SystemTimeToDateTime(SystemTime)
end;

end.
