//***********************************************************
//               IEDownloadManager  (Oct 15, 2001)          *
//                                                          *
//                         ver. 1.0                         *
//                                                          *
//                       For Delphi 5 & 6                   *
//                                                          *
//                            by                            *
//                     Per Lindsø Larsen                    *
//                   per.lindsoe@larsen.mail.dk             *
//                                                          *
//                                                          *
//                                                          *
//        Updated versions:                                 *
//                                                          *
//               http://www.euromind.com/iedelphi           *
//***********************************************************

unit _IEDownloadManager;

interface

uses
  EwbAcc, Urlmon, Windows, ActiveX, Classes, ComObj,
  Registry, DownloadForm_U;


type
TIEDownloadManagerFactory = class(TComObjectFactory)
private
  procedure AddKeys;
  procedure RemoveKeys;
public
  procedure UpdateRegistry(Register: Boolean); override;
end;


TIEDownloadManager = class(TComObject, IDownloadManager)
private
  DownloadForm: TDownloadForm;
  function Download(
    pmk: IMoniker; // Identifies the object to be downloaded
    pbc: IBindCtx; // Stores information used by the moniker to bind
    dwBindVerb: DWORD; // The action to be performed during the bind
    grfBINDF: DWORD; // Determines the use of URL encoding during the bind
    pBindInfo: PBindInfo; // Used to implement IBindStatusCallback::GetBindInfo
    pszHeaders: PWidechar; // Additional headers to use with IHttpNegotiate
    pszRedir: PWidechar; // The URL that the moniker is redirected to
    uiCP: UINT // The code page of the object's display name
    ): HRESULT; stdcall;
end;


const
  Class_IEDownloadManager: TGUID = '{FEC76531-D69B-448D-840F-AD7865DD9F7B}';

implementation

uses ComServ;

{ TIEDownloadManager }


procedure TIEDownloadManagerFactory.UpdateRegistry(Register: Boolean);
begin
  inherited UpdateRegistry(Register);
  if Register then AddKeys else RemoveKeys;
end;

procedure TIEDownloadManagerFactory.AddKeys;
var
  reg: TRegistry;
  S: string;
begin
  inherited;
  S := GUIDToString(Class_IEDownloadManager);
  reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\Microsoft\Internet Explorer', True) then
    begin
      Reg.WriteString('DownloadUI', S);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TIEDownloadManagerFactory.RemoveKeys;
var
  reg: TRegistry;
begin
  inherited;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software\Microsoft\Internet Explorer', False);
    reg.DeleteValue('DownloadUI');
    reg.Closekey;
  finally
    Reg.Free;
  end;
end;




{ TIEDownloadManager }

function TIEDownloadManager.Download(pmk: IMoniker; pbc: IBindCtx;
  dwBindVerb: DWORD; grfBINDF: DWORD; pBindInfo: PBindInfo; pszHeaders,
  pszRedir: PWidechar; uiCP: UINT): HRESULT;
var
  Url: PWidechar;
begin
  if DownloadForm = nil then Downloadform := TDownloadform.Create(nil);
  Downloadform.show;
  pmk.GetDisplayName(pbc, nil, Url);
  Downloadform.Download(Url);
  Result := S_OK;
end;

initialization
  TIEDownloadManagerFactory.Create(ComServer, TIEDownloadManager, Class_IEDownloadManager,
    'IEDownloadManager', 'Download Manager for IE 5.5 and later', ciMultiinstance, tmApartment);

end.

