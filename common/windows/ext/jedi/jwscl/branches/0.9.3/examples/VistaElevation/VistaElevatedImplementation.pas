unit VistaElevatedImplementation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj,

    JwsclElevation, VistaElevationDLL_TLB, StdVcl,
     JwsclTypes,   JwsclExceptions, JwsclSid,     JwsclAcl,
     JwsclVersion, JwsclConstants,  JwsclProcess, JwsclUtils,
     JwsclToken,
     JwsclDescriptor, JwsclKnownSid, JwsclMapping, JwsclResource,
     JwsclStrings;

type
  TElevationDemoObject = class(TTypedComObject, IElevationDemoObject)
  protected
    {IElevationDemoObject-Methoden hier deklarieren}
    procedure DoSomething(const Str : WideString); stdcall;
    function DoTest: HResult; stdcall;
  end;


resourcestring
  ElevationDescription = 'JEDI Windows Security Code Library - Vista Elevation Demo';

implementation

uses SysUtils,ComServ, Dialogs;

{ TElevationDemoObject }



procedure TElevationDemoObject.DoSomething(const Str : WideString);
var  Token : TJwSecurityToken;
begin
  try //always use a try except on the outer begin/end to check for all exceptions.
    Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);

    try
      if Token.RunElevation <> 0 then
        MessageDlg('Process started with elevation: '+Str,mtInformation,[mbOK],0)
      else
        MessageDlg('Process started withOUT elevation: '+Str,mtWarning,[mbOK],0);
    finally
      Token.Free;
    end;
  except
    On E : Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  end;
end;


function TElevationDemoObject.DoTest: HResult;
begin
  MessageBoxA(0,'TElevationDemoObject.DoTest: Das ist eine Ausgabe des DemoObjects','123',MB_OK);
  result := 0;
end;



initialization
  TJwElevationClassFactory.Create(
    @ElevationDescription, true,
    ComServer, TElevationDemoObject,
    CLASS_ElevationDemoObject,
    ciMultiInstance);
end.
