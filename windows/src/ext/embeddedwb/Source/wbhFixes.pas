unit wbhFixes;

interface

uses
  Windows, ActiveX;

type
{$EXTERNALSYM IServiceProvider}
  IServiceProvider = interface(IUnknown)
    ['{6d5140c1-7436-11ce-8034-00aa006009fa}']
    function QueryService(const rsid, IID: TGuid; out Obj): HResult; stdcall;
  end;

  PServiceProvider = ^IServiceProvider;
{$EXTERNALSYM PServiceProvider}

type
  IDispatch = interface(IUnknown)
    ['{00020400-0000-0000-C000-000000000046}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      stdcall;
  end;

  IDispatchEx = interface(IDispatch)
    ['{A6EF9860-C720-11D0-9337-00A0C90DCAA9}']
    function GetDispID(const bstrName: TBSTR; const grfdex: DWORD;
      out id: TDispID): HResult; stdcall;
    function InvokeEx(const id: TDispID; const lcid: LCID; const wflags:
      WORD; const pdp: PDispParams; out varRes: OleVariant; out pei:
      TExcepInfo; const pspCaller: PServiceProvider): HResult; stdcall;
    function DeleteMemberByName(const bstr: TBSTR;
      const grfdex: DWORD): HResult; stdcall;
    function DeleteMemberByDispID(const id: TDispID): HResult; stdcall;
    function GetMemberProperties(const id: TDispID; const grfdexFetch:
      DWORD; out grfdex: DWORD): HResult; stdcall;
    function GetMemberName(const id: TDispID; out bstrName: TBSTR):
      HResult; stdcall;
    function GetNextDispID(const grfdex: DWORD; const id: TDispID;
      out nid: TDispID): HResult; stdcall;
    function GetNameSpaceParent(out unk: IUnknown): HResult; stdcall;
  end;

implementation

end.
