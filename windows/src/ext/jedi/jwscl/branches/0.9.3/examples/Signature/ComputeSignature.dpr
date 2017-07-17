{This program computes the signature if a given string. This
signature and the public key necessary for verification are then
stored in two files. These files are used by the example
VerifySignature.}
program ComputeSignature;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JwaWindows,
  JwsclCryptProvider,
  JwsclExceptions,
  JwsclTypes;

//The files are put into the current directory. If you do not have
//the necessary rights to write, you have to change the paths.
const
  SignatureFile = 'Signature.sig';
  PublicKeyFile = 'PublicKey.pbk';

var Provider: TJwCryptProvider; Key: TJwCryptKey; Hash: TJwHash;
    Data: string; Len: Cardinal; Blob: Pointer; FS: TFileStream;
begin
  //If the default key container has not yet been created, specify
  //[ccfNewKeyset] as the last parameter
  TJwCryptProvider.DeleteKeyset('');
  Provider := TJwCryptProvider.Create('', '', ctRsaFull, [ccfNewKeyset]);
  try
    try
      //try to get the user key
      Key := TJwCryptKey.GetUserKey(Provider, kptSignature);
    except
      on E: EJwsclKeyApiException do
        if E.LastError = Cardinal(NTE_NO_KEY) then
          //No signature key pair was stored in the key container.
          //We create one and store it.
          Key := TJwCryptKey.GenerateContainerKey(Provider, kptSignature, [], 0)
        else
          raise;
    end;
    try
      Hash := TJwHash.Create(haSHA, Provider);
      try
        Writeln('Enter a string! Its signature will be stored in '+SignatureFile+'.');
        Readln(Data);
        Hash.HashData(Pointer(Data), Length(Data));
        Blob := Hash.Sign(Len, kptSignature);
        try
          FS := TFileStream.Create(SignatureFile, fmCreate);
          try
            FS.WriteBuffer(Blob^, Len);
          finally
            FS.Free;
          end;
        finally
          Hash.FreeBuffer(Blob);
        end;
        Writeln('Signature successfully stored');
      finally
        Hash.Free;
      end;


      Blob := Key.ExportKey(nil, kekPublic, [], Len);
      try
        FS := TFileStream.Create(PublicKeyFile, fmCreate);
        try
          FS.WriteBuffer(Blob^, Len);
        finally
          FS.Free;
        end;
        Writeln('Public key for signature decryption stored in '+PublicKeyFile+'.');
      finally
        Key.FreeBuffer(Blob);
      end;
    finally
      Key.Free;
    end;
  finally
    Provider.Free;
    Readln;
  end;
end.
