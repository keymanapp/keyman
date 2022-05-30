{This demonstration verifies the signature of a given string. The
signature and the public decryption key are taken from two files
created by ComputeSignature.}
program VerifySignature;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JwsclCryptProvider,
  JwsclTypes,
  JwsclExceptions,
  JwaWindows;

//If ComputeSignature is not in the current directory or
//you changed these pathes in ComputeSignature.dpr, you have to
//these
const
  SignatureFile = 'Signature.sig';
  PublicKeyFile = 'PublicKey.pbk';

var Provider: TJwCryptProvider; Key: TJwCryptKey; Hash: TJwHash;
    Data: string; MS: TMemoryStream;
begin
  Provider := TJwCryptProvider.Create('', '', ctRsaFull, [ccfVerifyContext]);
  try
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(PublicKeyFile);
      Key := TJwCryptKey.Import(Provider, MS.Memory, MS.Size, nil, []);
    finally
      MS.Free;
    end;
    try
      Writeln('Public key for signature decryption imported');
      Hash := TJwHash.Create(haSHA, Provider);
      try
        Writeln('Enter the string that was used in ComputeSignature!');
        Readln(Data);
        Hash.HashData(Pointer(Data), Length(Data));
        MS := TMemoryStream.Create;
        try
          MS.LoadFromFile(SignatureFile);
          try
            Hash.VerifySignature(MS.Memory, MS.Size, Key);
            Writeln('The signature is valid');
          except
            on E: EJwsclHashApiException do
            begin
              if E.LastError = Cardinal(NTE_BAD_SIGNATURE) then
                Writeln('The signature is invalid!')
              else
                Writeln(E.Message);
            end;
          end;
        finally
          MS.Free;
        end;
      finally
        Hash.Free;
      end;
    finally
      Key.Free;
    end;
  finally
    Provider.Free;
    Readln;
  end;
end.
