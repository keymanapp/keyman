{******************************************************************************}
{                                                                              }
{ URL Monikers Interface Unit for Object Pascal                     		   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2005 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The initial developer of the original translation is Rudy Velthuis		   }
{                                                                              }
{ Portions created by Rudy Velthuis are Copyright (C) 2005-2008                }
{ All Rights Reserved.                                      				   }
{                                                                              }
{ Adapted for JEDI API Library by Christian Wimmer                             }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ The original code is: urlmon.h, released 2005.                			   }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaUrlMon;

{$I ..\Includes\JediAPILib.inc}
interface

uses
  ActiveX,
{$IFNDEF DELPHI5}msxml,{$ENDIF}
  JwaWinBase, JwaWinUser, JwaWinType, JwaActiveX;

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}


{$HPPEMIT '//---------------------------------------------------------------------------'}
{$HPPEMIT '// if compilation errors occur while attempting to access structs, unions, or enums'}
{$HPPEMIT '// define NO_WIN32_LEAN_AND_MEAN so that the appropriate windows headers are included.'}
{$HPPEMIT '//---------------------------------------------------------------------------'}
{$HPPEMIT '#if defined(NO_WIN32_LEAN_AND_MEAN)'}
{$HPPEMIT '#include "rpc.h"'}
{$HPPEMIT '#include "rpcndr.h"'}
{$HPPEMIT '#include "urlmon.h"'}
{$HPPEMIT '  #ifndef COM_NO_WINDOWS_H'}
{$HPPEMIT '  #include "windows.h" '}
{$HPPEMIT '  #include "ole2.h"'}
{$HPPEMIT '  #endif'}
{$HPPEMIT '#endif'}


{$HPPEMIT 'interface DECLSPEC_UUID("79eac9c9-baf9-11ce-8c82-00aa004ba90b") IPersistMoniker;'}
{$HPPEMIT 'interface DECLSPEC_UUID("a5ca5f7f-1847-4d87-9c5b-918509f7511d") IMonikerProp;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9cd-baf9-11ce-8c82-00aa004ba90b") IBindProtocol;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9c0-baf9-11ce-8c82-00aa004ba90b") IBinding;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9c1-baf9-11ce-8c82-00aa004ba90b") IBindStatusCallback;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d0-baf9-11ce-8c82-00aa004ba90b") IAuthenticate;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d2-baf9-11ce-8c82-00aa004ba90b") IHttpNegotiate;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4F9F9FCB-E0F4-48eb-B7AB-FA2EA9365CB4") IHttpNegotiate2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F134C4B7-B1F8-4e75-B886-74B90943BECB") IWinInetFileStream;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d5-bafa-11ce-8c82-00aa004ba90b") IWindowForBindingUI;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d1-baf9-11ce-8c82-00aa004ba90b") ICodeInstall;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d6-bafa-11ce-8c82-00aa004ba90b") IWinInetInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d7-bafa-11ce-8c82-00aa004ba90b") IHttpSecurity;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9d8-bafa-11ce-8c82-00aa004ba90b") IWinInetHttpInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("DD1EC3B3-8391-4fdb-A9E6-347C3CAAA7DD") IWinInetCacheHints;'}
{$HPPEMIT 'interface DECLSPEC_UUID("fc4801a1-2ba9-11cf-a229-00aa003d7352") IBindHost;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e0-baf9-11ce-8c82-00aa004ba90b") IInternet;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e1-baf9-11ce-8c82-00aa004ba90b") IInternetBindInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e3-baf9-11ce-8c82-00aa004ba90b") IInternetProtocolRoot;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e4-baf9-11ce-8c82-00aa004ba90b") IInternetProtocol;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e5-baf9-11ce-8c82-00aa004ba90b") IInternetProtocolSink;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9f0-baf9-11ce-8c82-00aa004ba90b") IInternetProtocolSinkStackable;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e7-baf9-11ce-8c82-00aa004ba90b") IInternetSession;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9e8-baf9-11ce-8c82-00aa004ba90b") IInternetThreadSwitch;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9eb-baf9-11ce-8c82-00aa004ba90b") IInternetPriority;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9ec-baf9-11ce-8c82-00aa004ba90b") IInternetProtocolInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9ed-baf9-11ce-8c82-00aa004ba90b") IInternetSecurityMgrSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9ee-baf9-11ce-8c82-00aa004ba90b") IInternetSecurityManager;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F164EDF1-CC7C-4f0d-9A94-34222625C393") IInternetSecurityManagerEx;'}
{$HPPEMIT 'interface DECLSPEC_UUID("cd45f185-1b21-48e2-967b-ead743a8914e") IZoneIdentifier;'}
{$HPPEMIT 'interface DECLSPEC_UUID("3af280b6-cb3f-11d0-891e-00c04fb6bfc4") IInternetHostSecurityManager;'}
{$HPPEMIT 'interface DECLSPEC_UUID("79eac9ef-baf9-11ce-8c82-00aa004ba90b") IInternetZoneManager;'}
{$HPPEMIT 'interface DECLSPEC_UUID("A4C23339-8E06-431e-9BF4-7E711C085648") IInternetZoneManagerEx;'}
{$HPPEMIT 'interface DECLSPEC_UUID("B15B8DC1-C7E1-11d0-8680-00AA00BDCB71") ISoftDistExt;'}
{$HPPEMIT 'interface DECLSPEC_UUID("711C7600-6B48-11d1-B403-00AA00B92AF1") ICatalogFileInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("69d14c80-c18e-11d0-a9ce-006097942311") IDataFilter;'}
{$HPPEMIT 'interface DECLSPEC_UUID("70bdde00-c18e-11d0-a9ce-006097942311") IEncodingFilterFactory;'}
{$HPPEMIT 'interface DECLSPEC_UUID("53c84785-8425-4dc5-971b-e58d9c19f9b6") IWrappedProtocol;'}

{$HPPEMIT 'typedef System::DelphiInterface<IPersistMoniker> _di_IPersistMoniker;'}
{$HPPEMIT 'typedef System::DelphiInterface<IMonikerProp> _di_IMonikerProp;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBindProtocol> _di_IBindProtocol;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBinding> _di_IBinding;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBindStatusCallback> _di_IBindStatusCallback;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAuthenticate> _di_IAuthenticate;'}
{$HPPEMIT 'typedef System::DelphiInterface<IHttpNegotiate> _di_IHttpNegotiate;'}
{$HPPEMIT 'typedef System::DelphiInterface<IHttpNegotiate2> _di_IHttpNegotiate2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWinInetFileStream> _di_IWinInetFileStream;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWindowForBindingUI> _di_IWindowForBindingUI;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICodeInstall> _di_ICodeInstall;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWinInetInfo> _di_IWinInetInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IHttpSecurity> _di_IHttpSecurity;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWinInetHttpInfo> _di_IWinInetHttpInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWinInetCacheHints> _di_IWinInetCacheHints;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBindHost> _di_IBindHost;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternet> _di_IInternet;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetBindInfo> _di_IInternetBindInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetProtocolRoot> _di_IInternetProtocolRoot;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetProtocol> _di_IInternetProtocol;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetProtocolSink> _di_IInternetProtocolSink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetProtocolSinkStackable> _di_IInternetProtocolSinkStackable;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetSession> _di_IInternetSession;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetThreadSwitch> _di_IInternetThreadSwitch;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetPriority> _di_IInternetPriority;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetProtocolInfo> _di_IInternetProtocolInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetSecurityMgrSite> _di_IInternetSecurityMgrSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetSecurityManager> _di_IInternetSecurityManager;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetSecurityManagerEx> _di_IInternetSecurityManagerEx;'}
{$HPPEMIT 'typedef System::DelphiInterface<IZoneIdentifier> _di_IZoneIdentifier;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetHostSecurityManager> _di_IInternetHostSecurityManager;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetZoneManager> _di_IInternetZoneManager;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInternetZoneManagerEx> _di_IInternetZoneManagerEx;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISoftDistExt> _di_ISoftDistExt;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICatalogFileInfo> _di_ICatalogFileInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDataFilter> _di_IDataFilter;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEncodingFilterFactory> _di_IEncodingFilterFactory;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWrappedProtocol> _di_IWrappedProtocol;'}

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  {$EXTERNALSYM IID_IPersistMoniker}
  IID_IPersistMoniker: TGUID = (
    D1:$79EAC9C9; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IMonikerProp}
  IID_IMonikerProp: TGUID = (
    D1:$A5CA5F7F; D2:$1847; D3:$4D87; D4:($9C,$5B,$91,$85,$09,$F7,$51,$1D));
  {$EXTERNALSYM IID_IBindProtocol}
  IID_IBindProtocol: TGUID = (
    D1:$79EAC9CD; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IBinding}
  IID_IBinding: TGUID = (
    D1:$79EAC9C0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IBindStatusCallback}
  IID_IBindStatusCallback: TGUID = (
    D1:$79EAC9C1; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IAuthenticate}
  IID_IAuthenticate: TGUID = (
    D1:$79EAC9D0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IHttpNegotiate}
  IID_IHttpNegotiate: TGUID = (
    D1:$79EAC9D2; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IHttpNegotiate2}
  IID_IHttpNegotiate2: TGUID = (
    D1:$4F9F9FCB; D2:$E0F4; D3:$48EB; D4:($B7,$AB,$FA,$2E,$A9,$36,$5C,$B4));
  {$EXTERNALSYM IID_IWinInetFileStream}
  IID_IWinInetFileStream: TGUID = (
    D1:$F134C4B7; D2:$B1F8; D3:$4E75; D4:($B8,$86,$74,$B9,$09,$43,$BE,$CB));
  {$EXTERNALSYM IID_IWindowForBindingUI}
  IID_IWindowForBindingUI: TGUID = (
    D1:$79EAC9D5; D2:$BAFA; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_ICodeInstall}
  IID_ICodeInstall: TGUID = (
    D1:$79EAC9D1; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IWinInetInfo}
  IID_IWinInetInfo: TGUID = (
    D1:$79EAC9D6; D2:$BAFA; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IHttpSecurity}
  IID_IHttpSecurity: TGUID = (
    D1:$79EAC9D7; D2:$BAFA; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IWinInetHttpInfo}
  IID_IWinInetHttpInfo: TGUID = (
    D1:$79EAC9D8; D2:$BAFA; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IWinInetCacheHints}
  IID_IWinInetCacheHints: TGUID = (
    D1:$DD1EC3B3; D2:$8391; D3:$4FDB; D4:($A9,$E6,$34,$7C,$3C,$AA,$A7,$DD));
  {$EXTERNALSYM IID_IBindHost}
  IID_IBindHost: TGUID = (
    D1:$FC4801A1; D2:$2BA9; D3:$11CF; D4:($A2,$29,$00,$AA,$00,$3D,$73,$52));
  {$EXTERNALSYM SID_IBindHost}
  SID_IBindHost: TGUID = (
    D1:$FC4801A1; D2:$2BA9; D3:$11CF; D4:($A2,$29,$00,$AA,$00,$3D,$73,$52));
  {$EXTERNALSYM SID_SBindHost}
  SID_SBindHost: TGUID = (
    D1:$FC4801A1; D2:$2BA9; D3:$11CF; D4:($A2,$29,$00,$AA,$00,$3D,$73,$52));
  {$EXTERNALSYM SID_BindHost}
  SID_BindHost: TGUID = (
    D1:$FC4801A1; D2:$2BA9; D3:$11CF; D4:($A2,$29,$00,$AA,$00,$3D,$73,$52));
  {$EXTERNALSYM IID_IInternet}
  IID_IInternet: TGUID = (
    D1:$79EAC9E0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetBindInfo}
  IID_IInternetBindInfo: TGUID = (
    D1:$79EAC9E1; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetProtocolRoot}
  IID_IInternetProtocolRoot: TGUID = (
    D1:$79EAC9E3; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetProtocol}
  IID_IInternetProtocol: TGUID = (
    D1:$79EAC9E4; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetProtocolSink}
  IID_IInternetProtocolSink: TGUID = (
    D1:$79EAC9E5; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetProtocolSinkStackable}
  IID_IInternetProtocolSinkStackable: TGUID = (
    D1:$79EAC9F0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetSession}
  IID_IInternetSession: TGUID = (
    D1:$79EAC9E7; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetThreadSwitch}
  IID_IInternetThreadSwitch: TGUID = (
    D1:$79EAC9E8; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetPriority}
  IID_IInternetPriority: TGUID = (
    D1:$79EAC9EB; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetProtocolInfo}
  IID_IInternetProtocolInfo: TGUID = (
    D1:$79EAC9EC; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetSecurityMgrSite}
  IID_IInternetSecurityMgrSite: TGUID = (
    D1:$79EAC9ED; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetSecurityManager}
  IID_IInternetSecurityManager: TGUID = (
    D1:$79EAC9EE; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM SID_SInternetSecurityManager}
  SID_SInternetSecurityManager: TGUID = (
    D1:$79EAC9EE; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetSecurityManagerEx}
  IID_IInternetSecurityManagerEx: TGUID = (
    D1:$F164EDF1; D2:$CC7C; D3:$4F0D; D4:($9A,$94,$34,$22,$26,$25,$C3,$93));
  {$EXTERNALSYM SID_SInternetSecurityManagerEx}
  SID_SInternetSecurityManagerEx: TGUID = (
    D1:$F164EDF1; D2:$CC7C; D3:$4F0D; D4:($9A,$94,$34,$22,$26,$25,$C3,$93));
  {$EXTERNALSYM IID_IZoneIdentifier}
  IID_IZoneIdentifier: TGUID = (
    D1:$CD45F185; D2:$1B21; D3:$48E2; D4:($96,$7B,$EA,$D7,$43,$A8,$91,$4E));
  {$EXTERNALSYM IID_IInternetHostSecurityManager}
  IID_IInternetHostSecurityManager: TGUID = (
    D1:$3AF280B6; D2:$CB3F; D3:$11D0; D4:($89,$1E,$00,$C0,$4F,$B6,$BF,$C4));
  {$EXTERNALSYM SID_SInternetHostSecurityManager}
  SID_SInternetHostSecurityManager: TGUID = (
    D1:$3AF280B6; D2:$CB3F; D3:$11D0; D4:($89,$1E,$00,$C0,$4F,$B6,$BF,$C4));
  {$EXTERNALSYM IID_IInternetZoneManager}
  IID_IInternetZoneManager: TGUID = (
    D1:$79EAC9EF; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IInternetZoneManagerEx}
  IID_IInternetZoneManagerEx: TGUID = (
    D1:$A4C23339; D2:$8E06; D3:$431E; D4:($9B,$F4,$7E,$71,$1C,$08,$56,$48));
  {$EXTERNALSYM IID_ISoftDistExt}
  IID_ISoftDistExt: TGUID = (
    D1:$B15B8DC1; D2:$C7E1; D3:$11D0; D4:($86,$80,$00,$AA,$00,$BD,$CB,$71));
  {$EXTERNALSYM IID_ICatalogFileInfo}
  IID_ICatalogFileInfo: TGUID = (
    D1:$711C7600; D2:$6B48; D3:$11D1; D4:($B4,$03,$00,$AA,$00,$B9,$2A,$F1));
  {$EXTERNALSYM IID_IDataFilter}
  IID_IDataFilter: TGUID = (
    D1:$69D14C80; D2:$C18E; D3:$11D0; D4:($A9,$CE,$00,$60,$97,$94,$23,$11));
  {$EXTERNALSYM IID_IEncodingFilterFactory}
  IID_IEncodingFilterFactory: TGUID = (
    D1:$70BDDE00; D2:$C18E; D3:$11D0; D4:($A9,$CE,$00,$60,$97,$94,$23,$11));
  {$EXTERNALSYM IID_IWrappedProtocol}
  IID_IWrappedProtocol: TGUID = (
    D1:$53C84785; D2:$8425; D3:$4DC5; D4:($97,$1B,$E5,$8D,$9C,$19,$F9,$B6));

  {$EXTERNALSYM IID_IOInet}
  IID_IOInet: TGUID = (
    D1:$79EAC9E0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetBindInfo}
  IID_IOInetBindInfo: TGUID = (
    D1:$79EAC9E1; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetProtocolRoot}
  IID_IOInetProtocolRoot: TGUID = (
    D1:$79EAC9E3; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetProtocol}
  IID_IOInetProtocol: TGUID = (
    D1:$79EAC9E4; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetProtocolSink}
  IID_IOInetProtocolSink: TGUID = (
    D1:$79EAC9E5; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetProtocolInfo}
  IID_IOInetProtocolInfo: TGUID = (
    D1:$79EAC9EC; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetSession}
  IID_IOInetSession: TGUID = (
    D1:$79EAC9E7; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetPriority}
  IID_IOInetPriority: TGUID = (
    D1:$79EAC9EB; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetThreadSwitch}
  IID_IOInetThreadSwitch: TGUID = (
    D1:$79EAC9E8; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));
  {$EXTERNALSYM IID_IOInetProtocolSinkStackable}
  IID_IOInetProtocolSinkStackable: TGUID = (
    D1:$79EAC9F0; D2:$BAF9; D3:$11CE; D4:($8C,$82,$00,$AA,$00,$4B,$A9,$0B));


//=--------------------------------------------------------------------------=
// UrlMon.h
//=--------------------------------------------------------------------------=
// (C) Copyright 1995-1998 Microsoft Corporation.  All Rights Reserved.
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//=--------------------------------------------------------------------------=

const
  {$EXTERNALSYM SZ_URLCONTEXT}
  SZ_URLCONTEXT            = 'URL Context';
  {$EXTERNALSYM SZ_ASYNC_CALLEE}
  SZ_ASYNC_CALLEE          = 'AsyncCallee';

  {$EXTERNALSYM MKSYS_URLMONIKER}
  MKSYS_URLMONIKER         = 6;

  {$EXTERNALSYM URL_MK_LEGACY}
  URL_MK_LEGACY            = 0;
  {$EXTERNALSYM URL_MK_UNIFORM}
  URL_MK_UNIFORM           = 1;
  {$EXTERNALSYM URL_MK_NO_CANONICALIZE}
  URL_MK_NO_CANONICALIZE   = 2;


// flags for FaultInIEFeature
const
  {$EXTERNALSYM FIEF_FLAG_FORCE_JITUI}
  FIEF_FLAG_FORCE_JITUI               = $1;    // force JIT ui even if
                                               // previoulsy rejected by
                                               // user in this session or
                                               // marked as Never Ask Again
  {$EXTERNALSYM FIEF_FLAG_PEEK}
  FIEF_FLAG_PEEK                      = $2;    // just peek, don't faultin
  {$EXTERNALSYM FIEF_FLAG_SKIP_INSTALLED_VERSION_CHECK}
  FIEF_FLAG_SKIP_INSTALLED_VERSION_CHECK = $4; // force JIT without checking
                                               // local version


const
  {$EXTERNALSYM FMFD_DEFAULT}
  FMFD_DEFAULT             = $00000000;
  {$EXTERNALSYM FMFD_URLASFILENAME}
  FMFD_URLASFILENAME       = $00000001;
  {$EXTERNALSYM FMFD_ENABLEMIMESNIFFING}
  FMFD_ENABLEMIMESNIFFING  = $00000002;
  {$EXTERNALSYM FMFD_IGNOREMIMETEXTPLAIN}
  FMFD_IGNOREMIMETEXTPLAIN  = $00000004;

// URLMON-specific defines for UrlMkSetSessionOption() above
  {$EXTERNALSYM URLMON_OPTION_USERAGENT}
  URLMON_OPTION_USERAGENT           = $10000001;
  {$EXTERNALSYM URLMON_OPTION_USERAGENT_REFRESH}
  URLMON_OPTION_USERAGENT_REFRESH   = $10000002;
  {$EXTERNALSYM URLMON_OPTION_URL_ENCODING}
  URLMON_OPTION_URL_ENCODING        = $10000004;
  {$EXTERNALSYM URLMON_OPTION_USE_BINDSTRINGCREDS}
  URLMON_OPTION_USE_BINDSTRINGCREDS = $10000008;

const
  {$EXTERNALSYM CF_NULL}
  CF_NULL                 = 0;
  {$EXTERNALSYM CFSTR_MIME_NULL}
  CFSTR_MIME_NULL         = nil;
  {$EXTERNALSYM CFSTR_MIME_TEXT}
  CFSTR_MIME_TEXT         = 'text/plain';
  {$EXTERNALSYM CFSTR_MIME_RICHTEXT}
  CFSTR_MIME_RICHTEXT     = 'text/richtext';
  {$EXTERNALSYM CFSTR_MIME_X_BITMAP}
  CFSTR_MIME_X_BITMAP     = 'image/x-xbitmap';
  {$EXTERNALSYM CFSTR_MIME_POSTSCRIPT}
  CFSTR_MIME_POSTSCRIPT   = 'application/postscript';
  {$EXTERNALSYM CFSTR_MIME_AIFF}
  CFSTR_MIME_AIFF         = 'audio/aiff';
  {$EXTERNALSYM CFSTR_MIME_BASICAUDIO}
  CFSTR_MIME_BASICAUDIO   = 'audio/basic';
  {$EXTERNALSYM CFSTR_MIME_WAV}
  CFSTR_MIME_WAV          = 'audio/wav';
  {$EXTERNALSYM CFSTR_MIME_X_WAV}
  CFSTR_MIME_X_WAV        = 'audio/x-wav';
  {$EXTERNALSYM CFSTR_MIME_GIF}
  CFSTR_MIME_GIF          = 'image/gif';
  {$EXTERNALSYM CFSTR_MIME_PJPEG}
  CFSTR_MIME_PJPEG        = 'image/pjpeg';
  {$EXTERNALSYM CFSTR_MIME_JPEG}
  CFSTR_MIME_JPEG         = 'image/jpeg';
  {$EXTERNALSYM CFSTR_MIME_TIFF}
  CFSTR_MIME_TIFF         = 'image/tiff';
  {$EXTERNALSYM CFSTR_MIME_X_PNG}
  CFSTR_MIME_X_PNG        = 'image/x-png';
  {$EXTERNALSYM CFSTR_MIME_BMP}
  CFSTR_MIME_BMP          = 'image/bmp';
  {$EXTERNALSYM CFSTR_MIME_X_ART}
  CFSTR_MIME_X_ART        = 'image/x-jg';
  {$EXTERNALSYM CFSTR_MIME_X_EMF}
  CFSTR_MIME_X_EMF        = 'image/x-emf';
  {$EXTERNALSYM CFSTR_MIME_X_WMF}
  CFSTR_MIME_X_WMF        = 'image/x-wmf';
  {$EXTERNALSYM CFSTR_MIME_AVI}
  CFSTR_MIME_AVI          = 'video/avi';
  {$EXTERNALSYM CFSTR_MIME_MPEG}
  CFSTR_MIME_MPEG         = 'video/mpeg';
  {$EXTERNALSYM CFSTR_MIME_FRACTALS}
  CFSTR_MIME_FRACTALS     = 'application/fractals';
  {$EXTERNALSYM CFSTR_MIME_RAWDATA}
  CFSTR_MIME_RAWDATA      = 'application/octet-stream';
  {$EXTERNALSYM CFSTR_MIME_RAWDATASTRM}
  CFSTR_MIME_RAWDATASTRM  = 'application/octet-stream';
  {$EXTERNALSYM CFSTR_MIME_PDF}
  CFSTR_MIME_PDF          = 'application/pdf';
  {$EXTERNALSYM CFSTR_MIME_HTA}
  CFSTR_MIME_HTA          = 'application/hta';
  {$EXTERNALSYM CFSTR_MIME_X_AIFF}
  CFSTR_MIME_X_AIFF       = 'audio/x-aiff';
  {$EXTERNALSYM CFSTR_MIME_X_REALAUDIO}
  CFSTR_MIME_X_REALAUDIO  = 'audio/x-pn-realaudio';
  {$EXTERNALSYM CFSTR_MIME_XBM}
  CFSTR_MIME_XBM          = 'image/xbm';
  {$EXTERNALSYM CFSTR_MIME_QUICKTIME}
  CFSTR_MIME_QUICKTIME    = 'video/quicktime';
  {$EXTERNALSYM CFSTR_MIME_X_MSVIDEO}
  CFSTR_MIME_X_MSVIDEO    = 'video/x-msvideo';
  {$EXTERNALSYM CFSTR_MIME_X_SGI_MOVIE}
  CFSTR_MIME_X_SGI_MOVIE  = 'video/x-sgi-movie';
  {$EXTERNALSYM CFSTR_MIME_HTML}
  CFSTR_MIME_HTML         = 'text/html';
  {$EXTERNALSYM CFSTR_MIME_XML}
  CFSTR_MIME_XML          = 'text/xml';

// MessageId: MK_S_ASYNCHRONOUS
// MessageText: Operation is successful, but will complete asynchronously.
//
  {$EXTERNALSYM MK_S_ASYNCHRONOUS}
  MK_S_ASYNCHRONOUS    = HResult($000401E8);
  {$EXTERNALSYM S_ASYNCHRONOUS}
  S_ASYNCHRONOUS       = MK_S_ASYNCHRONOUS;

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM E_PENDING}
  E_PENDING            = HResult($8000000A);
{$ENDIF JWA_INCLUDEMODE}

//
// WinINet and protocol specific errors are mapped to one of the following
// error which are returned in IBSC::OnStopBinding
//
// Note: FACILITY C is split into ranges of 1k
// C0000 - C03FF  INET_E_ (URLMON's original hresult)
// C0400 - C07FF  INET_E_CLIENT_xxx
// C0800 - C0BFF  INET_E_SERVER_xxx
// C0C00 - C0FFF  INET_E_????
// C1000 - C13FF  INET_E_AGENT_xxx (info delivery agents)

const
  {$EXTERNALSYM INET_E_INVALID_URL}
  INET_E_INVALID_URL               = HResult($800C0002);
  {$EXTERNALSYM INET_E_NO_SESSION}
  INET_E_NO_SESSION                = HResult($800C0003);
  {$EXTERNALSYM INET_E_CANNOT_CONNECT}
  INET_E_CANNOT_CONNECT            = HResult($800C0004);
  {$EXTERNALSYM INET_E_RESOURCE_NOT_FOUND}
  INET_E_RESOURCE_NOT_FOUND        = HResult($800C0005);
  {$EXTERNALSYM INET_E_OBJECT_NOT_FOUND}
  INET_E_OBJECT_NOT_FOUND          = HResult($800C0006);
  {$EXTERNALSYM INET_E_DATA_NOT_AVAILABLE}
  INET_E_DATA_NOT_AVAILABLE        = HResult($800C0007);
  {$EXTERNALSYM INET_E_DOWNLOAD_FAILURE}
  INET_E_DOWNLOAD_FAILURE          = HResult($800C0008);
  {$EXTERNALSYM INET_E_AUTHENTICATION_REQUIRED}
  INET_E_AUTHENTICATION_REQUIRED   = HResult($800C0009);
  {$EXTERNALSYM INET_E_NO_VALID_MEDIA}
  INET_E_NO_VALID_MEDIA            = HResult($800C000A);
  {$EXTERNALSYM INET_E_CONNECTION_TIMEOUT}
  INET_E_CONNECTION_TIMEOUT        = HResult($800C000B);
  {$EXTERNALSYM INET_E_INVALID_REQUEST}
  INET_E_INVALID_REQUEST           = HResult($800C000C);
  {$EXTERNALSYM INET_E_UNKNOWN_PROTOCOL}
  INET_E_UNKNOWN_PROTOCOL          = HResult($800C000D);
  {$EXTERNALSYM INET_E_SECURITY_PROBLEM}
  INET_E_SECURITY_PROBLEM          = HResult($800C000E);
  {$EXTERNALSYM INET_E_CANNOT_LOAD_DATA}
  INET_E_CANNOT_LOAD_DATA          = HResult($800C000F);
  {$EXTERNALSYM INET_E_CANNOT_INSTANTIATE_OBJECT}
  INET_E_CANNOT_INSTANTIATE_OBJECT = HResult($800C0010);
  {$EXTERNALSYM INET_E_REDIRECT_FAILED}
  INET_E_REDIRECT_FAILED           = HResult($800C0014);   
  {$EXTERNALSYM INET_E_REDIRECT_TO_DIR}
  INET_E_REDIRECT_TO_DIR           = HResult($800C0015);
  {$EXTERNALSYM INET_E_CANNOT_LOCK_REQUEST}
  INET_E_CANNOT_LOCK_REQUEST       = HResult($800C0016);
  {$EXTERNALSYM INET_E_USE_EXTEND_BINDING}
  INET_E_USE_EXTEND_BINDING        = HResult($800C0017);
  {$EXTERNALSYM INET_E_TERMINATED_BIND}
  INET_E_TERMINATED_BIND           = HResult($800C0018);
  {$EXTERNALSYM INET_E_ERROR_FIRST}
  INET_E_ERROR_FIRST               = HResult($800C0002);
  {$EXTERNALSYM INET_E_CODE_DOWNLOAD_DECLINED}
  INET_E_CODE_DOWNLOAD_DECLINED    = HResult($800C0100);
  {$EXTERNALSYM INET_E_RESULT_DISPATCHED}
  INET_E_RESULT_DISPATCHED         = HResult($800C0200);
  {$EXTERNALSYM INET_E_CANNOT_REPLACE_SFP_FILE}
  INET_E_CANNOT_REPLACE_SFP_FILE   = HResult($800C0300);
  {$EXTERNALSYM INET_E_CODE_INSTALL_SUPPRESSED}
  INET_E_CODE_INSTALL_SUPPRESSED   = HResult($800C0400);
  {$EXTERNALSYM INET_E_ERROR_LAST}
  INET_E_ERROR_LAST                = INET_E_CANNOT_REPLACE_SFP_FILE;

type
  {$EXTERNALSYM IPersistMoniker}
  IPersistMoniker = interface(IUnknown)
  ['{79eac9c9-baf9-11ce-8c82-00aa004ba90b}']
    function GetClassID(out pClassID: TCLSID): HResult; stdcall;
    function IsDirty: HResult; stdcall;
    function Load(fFullyAvailable: BOOL; pimkName: IMoniker; pibc:
      IBindCtx; grfMode: DWORD): HResult; stdcall;
    function Save(pimkName: IMoniker; pbc: IBindCtx; fRemember: BOOL): HResult; stdcall;
    function SaveCompleted(pimkName: IMoniker; pibc: IBindCtx): HResult; stdcall;
    function GetCurMoniker(out ppimkName: IMoniker): HResult; stdcall;
  end;

  {$EXTERNALSYM MONIKERPROPERTY}
  MONIKERPROPERTY = DWORD;
  TMonikerProperty = DWORD;

const
  {$EXTERNALSYM MIMETYPEPROP}
  MIMETYPEPROP        = 0;
  {$EXTERNALSYM USE_SRC_URL}
  USE_SRC_URL         = $1;
  {$EXTERNALSYM CLASSIDPROP}
  CLASSIDPROP         = $2;
  {$EXTERNALSYM TRUSTEDDOWNLOADPROP}
  TRUSTEDDOWNLOADPROP = $3;
  {$EXTERNALSYM POPUPLEVELPROP}
  POPUPLEVELPROP      = $4;

type
  {$EXTERNALSYM IMonikerProp}
  IMonikerProp = interface(IUnknown)
  ['{a5ca5f7f-1847-4d87-9c5b-918509f7511d}']
    function PutProperty(mkp: TMonikerProperty;
      val: PWideChar): HResult; stdcall;
  end;

  {$EXTERNALSYM IBinding}
  IBinding = interface(IUnknown)
  ['{79eac9c0-baf9-11ce-8c82-00aa004ba90b}']
    function Abort: HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function SetPriority(nPriority: Longint): HResult; stdcall;
    function GetPriority(out pnPriority: Longint): HResult; stdcall;
    function GetBindResult(out pclsidProtocol: TCLSID; out pdwResult: DWORD;
      out pszResult: POleStr; dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IBindProtocol}
  IBindProtocol = interface(IUnknown)
  ['{79eac9cd-baf9-11ce-8c82-00aa004ba90b}']
    function CreateBinding(szUrl: PWideChar; pbc: IBindCtx;
      out ppb: IBinding): HResult; stdcall;
  end;

type
  {$EXTERNALSYM BINDVERB}
  BINDVERB = DWORD;
  TBindVerb = BINDVERB;

const
  {$EXTERNALSYM BINDVERB_GET}
  BINDVERB_GET    = 0;
  {$EXTERNALSYM BINDVERB_POST}
  BINDVERB_POST   = $1;
  {$EXTERNALSYM BINDVERB_PUT}
  BINDVERB_PUT    = $2;
  {$EXTERNALSYM BINDVERB_CUSTOM}
  BINDVERB_CUSTOM = $3;

type
  {$EXTERNALSYM BINDINFOF}
  BINDINFOF = DWORD;
  TBindInfoF = BINDINFOF;

const
  {$EXTERNALSYM BINDINFOF_URLENCODESTGMEDDATA}
  BINDINFOF_URLENCODESTGMEDDATA = $1;
  {$EXTERNALSYM BINDINFOF_URLENCODEDEXTRAINFO}
  BINDINFOF_URLENCODEDEXTRAINFO = $2;

type
  {$EXTERNALSYM BINDF}
  BINDF = DWORD;
  TBindF = DWORD;

const
  {$EXTERNALSYM BINDF_ASYNCHRONOUS}
  BINDF_ASYNCHRONOUS             = $1;
  {$EXTERNALSYM BINDF_ASYNCSTORAGE}
  BINDF_ASYNCSTORAGE             = $2;
  {$EXTERNALSYM BINDF_NOPROGRESSIVERENDERING}
  BINDF_NOPROGRESSIVERENDERING   = $4;
  {$EXTERNALSYM BINDF_OFFLINEOPERATION}
  BINDF_OFFLINEOPERATION         = $8;
  {$EXTERNALSYM BINDF_GETNEWESTVERSION}
  BINDF_GETNEWESTVERSION         = $10;
  {$EXTERNALSYM BINDF_NOWRITECACHE}
  BINDF_NOWRITECACHE             = $20;
  {$EXTERNALSYM BINDF_NEEDFILE}
  BINDF_NEEDFILE                 = $40;
  {$EXTERNALSYM BINDF_PULLDATA}
  BINDF_PULLDATA                 = $80;
  {$EXTERNALSYM BINDF_IGNORESECURITYPROBLEM}
  BINDF_IGNORESECURITYPROBLEM    = $100;
  {$EXTERNALSYM BINDF_RESYNCHRONIZE}
  BINDF_RESYNCHRONIZE            = $200;
  {$EXTERNALSYM BINDF_HYPERLINK}
  BINDF_HYPERLINK                = $400;
  {$EXTERNALSYM BINDF_NO_UI}
  BINDF_NO_UI                    = $800;
  {$EXTERNALSYM BINDF_SILENTOPERATION}
  BINDF_SILENTOPERATION          = $1000;
  {$EXTERNALSYM BINDF_PRAGMA_NO_CACHE}
  BINDF_PRAGMA_NO_CACHE          = $2000;
  {$EXTERNALSYM BINDF_GETCLASSOBJECT}
  BINDF_GETCLASSOBJECT           = $4000;
  {$EXTERNALSYM BINDF_RESERVED_1}
  BINDF_RESERVED_1               = $8000;
  {$EXTERNALSYM BINDF_FREE_THREADED}
  BINDF_FREE_THREADED            = $10000;
  {$EXTERNALSYM BINDF_DIRECT_READ}
  BINDF_DIRECT_READ              = $20000;
  {$EXTERNALSYM BINDF_FORMS_SUBMIT}
  BINDF_FORMS_SUBMIT             = $40000;
  {$EXTERNALSYM BINDF_GETFROMCACHE_IF_NET_FAIL}
  BINDF_GETFROMCACHE_IF_NET_FAIL = $80000;
  {$EXTERNALSYM BINDF_FROMURLMON}
  BINDF_FROMURLMON               = $100000;
  {$EXTERNALSYM BINDF_FWD_BACK}
  BINDF_FWD_BACK                 = $200000;
  {$EXTERNALSYM BINDF_PREFERDEFAULTHANDLER}
  BINDF_PREFERDEFAULTHANDLER     = $400000;
  {$EXTERNALSYM BINDF_ENFORCERESTRICTED}
  BINDF_ENFORCERESTRICTED        = $800000;

  // Aliases
  {$EXTERNALSYM BINDF_DONTUSECACHE}
  BINDF_DONTUSECACHE             = BINDF_GETNEWESTVERSION;
  {$EXTERNALSYM BINDF_DONTPUTINCACHE}
  BINDF_DONTPUTINCACHE           = BINDF_NOWRITECACHE;
  {$EXTERNALSYM BINDF_NOCOPYDATA}
  BINDF_NOCOPYDATA               = BINDF_PULLDATA;

type
  {$EXTERNALSYM URL_ENCODING}
  URL_ENCODING = DWORD;
  TUrlEncoding = DWORD;

const
  {$EXTERNALSYM URL_ENCODING_NONE}
  URL_ENCODING_NONE         = 0;
  {$EXTERNALSYM URL_ENCODING_ENABLE_UTF8}
  URL_ENCODING_ENABLE_UTF8  = $10000000;
  {$EXTERNALSYM URL_ENCODING_DISABLE_UTF8}
  URL_ENCODING_DISABLE_UTF8 = $20000000;

type
  PBindInfo = ^TBindInfo;
  {$EXTERNALSYM _tagBINDINFO}
  _tagBINDINFO = record
    cbSize: ULONG;
    szExtraInfo: PWideChar;
    stgmedData: TStgMedium;
    grfBindInfoF: TBindInfoF;
    dwBindVerb: TBindVerb;
    szCustomVerb: PWideChar;
    cbstgmedData: DWORD;
    dwOptions: DWORD;
    dwOptionsFlags: DWORD;
    dwCodePage: DWORD;
    securityAttributes: TSecurityAttributes;
    iid: TIID;
    pUnk: IUnknown;
    dwReserved: DWORD;
  end;
  {$EXTERNALSYM BINDINFO}
  BINDINFO = _tagBINDINFO;
  TBindInfo = _tagBINDINFO;

  PRemSecurityAttributes = ^TRemSecurityAttributes;
  {$EXTERNALSYM _REMSECURITY_ATTRIBUTES}
  _REMSECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: DWORD;
    bInheritHandle: BOOL;
  end;
  {$EXTERNALSYM REMSECURITY_ATTRIBUTES}
  REMSECURITY_ATTRIBUTES = _REMSECURITY_ATTRIBUTES;
  TRemSecurityAttributes = _REMSECURITY_ATTRIBUTES;

  PRemBindInfo = ^TRemBindInfo;
  {$EXTERNALSYM _tagRemBINDINFO}
  _tagRemBINDINFO = record
    cbSize: ULONG;
    szExtraInfo: PWideChar;
    grfBindInfoF: TBindInfoF;
    dwBindVerb: TBindVerb;
    szCustomVerb: PWideChar;
    cbstgmedData: DWORD;
    dwOptions: DWORD;
    dwOptionsFlags: DWORD;
    dwCodePage: DWORD;
    securityAttributes: TRemSecurityAttributes;
    iid: TIID;
    pUnk: IUnknown;
    dwReserved: DWORD;
  end;
  {$EXTERNALSYM RemBINDINFO}
  RemBINDINFO = _tagRemBINDINFO;
  TRemBindInfo = _tagRemBINDINFO;

  PRemFormatEtc = ^TRemFormatEtc;
  {$EXTERNALSYM tagRemFORMATETC}
  tagRemFORMATETC = record
    cfFormat: DWORD;
    ptd: DWORD;
    dwAspect: DWORD;
    lindex: Longint;
    tymed: DWORD;
  end;
  {$EXTERNALSYM RemFORMATETC}
  RemFORMATETC = tagRemFORMATETC;
  TRemFormatEtc = tagRemFORMATETC;

  {$EXTERNALSYM BINDINFO_OPTIONS}
  BINDINFO_OPTIONS = DWORD;
  TBindInfoOptions = BINDINFO_OPTIONS;

const
  {$EXTERNALSYM BINDINFO_OPTIONS_WININETFLAG}
  BINDINFO_OPTIONS_WININETFLAG              = $10000;
  {$EXTERNALSYM BINDINFO_OPTIONS_ENABLE_UTF8}
  BINDINFO_OPTIONS_ENABLE_UTF8              = $20000;
  {$EXTERNALSYM BINDINFO_OPTIONS_DISABLE_UTF8}
  BINDINFO_OPTIONS_DISABLE_UTF8             = $40000;
  {$EXTERNALSYM BINDINFO_OPTIONS_USE_IE_ENCODING}
  BINDINFO_OPTIONS_USE_IE_ENCODING          = $80000;
  {$EXTERNALSYM BINDINFO_OPTIONS_BINDTOOBJECT}
  BINDINFO_OPTIONS_BINDTOOBJECT             = $100000;
  {$EXTERNALSYM BINDINFO_OPTIONS_SECURITYOPTOUT}
  BINDINFO_OPTIONS_SECURITYOPTOUT           = $200000;
  {$EXTERNALSYM BINDINFO_OPTIONS_IGNOREMIMETEXTPLAIN}
  BINDINFO_OPTIONS_IGNOREMIMETEXTPLAIN      = $400000;
  {$EXTERNALSYM BINDINFO_OPTIONS_USEBINDSTRINGCREDS}
  BINDINFO_OPTIONS_USEBINDSTRINGCREDS       = $800000;
  {$EXTERNALSYM BINDINFO_OPTIONS_IGNOREHTTPHTTPSREDIRECTS}
  BINDINFO_OPTIONS_IGNOREHTTPHTTPSREDIRECTS = $1000000;
  {$EXTERNALSYM BINDINFO_OPTIONS_SHDOCVW_NAVIGATE}
  BINDINFO_OPTIONS_SHDOCVW_NAVIGATE         = $80000000;

type
  {$EXTERNALSYM BSCF}
  BSCF = DWORD;
  TBSCF = DWORD;

const
  {$EXTERNALSYM BSCF_FIRSTDATANOTIFICATION}
  BSCF_FIRSTDATANOTIFICATION        = $1;
  {$EXTERNALSYM BSCF_INTERMEDIATEDATANOTIFICATION}
  BSCF_INTERMEDIATEDATANOTIFICATION = $2;
  {$EXTERNALSYM BSCF_LASTDATANOTIFICATION}
  BSCF_LASTDATANOTIFICATION         = $4;
  {$EXTERNALSYM BSCF_DATAFULLYAVAILABLE}
  BSCF_DATAFULLYAVAILABLE           = $8;
  {$EXTERNALSYM BSCF_AVAILABLEDATASIZEUNKNOWN}
  BSCF_AVAILABLEDATASIZEUNKNOWN     = $10;

type
  {$EXTERNALSYM tagBINDSTATUS}
  tagBINDSTATUS = DWORD;
  {$EXTERNALSYM BINDSTATUS}
  BINDSTATUS = tagBINDSTATUS;
  TBindStatus = tagBINDSTATUS;

const
  {$EXTERNALSYM BINDSTATUS_FINDINGRESOURCE}
  BINDSTATUS_FINDINGRESOURCE            = 1;
  {$EXTERNALSYM BINDSTATUS_CONNECTING}
  BINDSTATUS_CONNECTING                 = BINDSTATUS_FINDINGRESOURCE + 1;
  {$EXTERNALSYM BINDSTATUS_REDIRECTING}
  BINDSTATUS_REDIRECTING                = BINDSTATUS_CONNECTING + 1;
  {$EXTERNALSYM BINDSTATUS_BEGINDOWNLOADDATA}
  BINDSTATUS_BEGINDOWNLOADDATA          = BINDSTATUS_REDIRECTING + 1;
  {$EXTERNALSYM BINDSTATUS_DOWNLOADINGDATA}
  BINDSTATUS_DOWNLOADINGDATA            = BINDSTATUS_BEGINDOWNLOADDATA + 1;
  {$EXTERNALSYM BINDSTATUS_ENDDOWNLOADDATA}
  BINDSTATUS_ENDDOWNLOADDATA            = BINDSTATUS_DOWNLOADINGDATA + 1;
  {$EXTERNALSYM BINDSTATUS_BEGINDOWNLOADCOMPONENTS}
  BINDSTATUS_BEGINDOWNLOADCOMPONENTS    = BINDSTATUS_ENDDOWNLOADDATA + 1;
  {$EXTERNALSYM BINDSTATUS_INSTALLINGCOMPONENTS}
  BINDSTATUS_INSTALLINGCOMPONENTS       = BINDSTATUS_BEGINDOWNLOADCOMPONENTS + 1;
  {$EXTERNALSYM BINDSTATUS_ENDDOWNLOADCOMPONENTS}
  BINDSTATUS_ENDDOWNLOADCOMPONENTS      = BINDSTATUS_INSTALLINGCOMPONENTS + 1;
  {$EXTERNALSYM BINDSTATUS_USINGCACHEDCOPY}
  BINDSTATUS_USINGCACHEDCOPY            = BINDSTATUS_ENDDOWNLOADCOMPONENTS + 1;
  {$EXTERNALSYM BINDSTATUS_SENDINGREQUEST}
  BINDSTATUS_SENDINGREQUEST             = BINDSTATUS_USINGCACHEDCOPY + 1;
  {$EXTERNALSYM BINDSTATUS_CLASSIDAVAILABLE}
  BINDSTATUS_CLASSIDAVAILABLE           = BINDSTATUS_SENDINGREQUEST + 1;
  {$EXTERNALSYM BINDSTATUS_MIMETYPEAVAILABLE}
  BINDSTATUS_MIMETYPEAVAILABLE          = BINDSTATUS_CLASSIDAVAILABLE + 1;
  {$EXTERNALSYM BINDSTATUS_CACHEFILENAMEAVAILABLE}
  BINDSTATUS_CACHEFILENAMEAVAILABLE     = BINDSTATUS_MIMETYPEAVAILABLE + 1;
  {$EXTERNALSYM BINDSTATUS_BEGINSYNCOPERATION}
  BINDSTATUS_BEGINSYNCOPERATION         = BINDSTATUS_CACHEFILENAMEAVAILABLE + 1;
  {$EXTERNALSYM BINDSTATUS_ENDSYNCOPERATION}
  BINDSTATUS_ENDSYNCOPERATION           = BINDSTATUS_BEGINSYNCOPERATION + 1;
  {$EXTERNALSYM BINDSTATUS_BEGINUPLOADDATA}
  BINDSTATUS_BEGINUPLOADDATA            = BINDSTATUS_ENDSYNCOPERATION + 1;
  {$EXTERNALSYM BINDSTATUS_UPLOADINGDATA}
  BINDSTATUS_UPLOADINGDATA              = BINDSTATUS_BEGINUPLOADDATA + 1;
  {$EXTERNALSYM BINDSTATUS_ENDUPLOADDATA}
  BINDSTATUS_ENDUPLOADDATA              = BINDSTATUS_UPLOADINGDATA + 1;
  {$EXTERNALSYM BINDSTATUS_PROTOCOLCLASSID}
  BINDSTATUS_PROTOCOLCLASSID            = BINDSTATUS_ENDUPLOADDATA + 1;
  {$EXTERNALSYM BINDSTATUS_ENCODING}
  BINDSTATUS_ENCODING                   = BINDSTATUS_PROTOCOLCLASSID + 1;
  {$EXTERNALSYM BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE}
  BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE  = BINDSTATUS_ENCODING + 1;
  {$EXTERNALSYM BINDSTATUS_CLASSINSTALLLOCATION}
  BINDSTATUS_CLASSINSTALLLOCATION       = BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE + 1;
  {$EXTERNALSYM BINDSTATUS_DECODING}
  BINDSTATUS_DECODING                   = BINDSTATUS_CLASSINSTALLLOCATION + 1;
  {$EXTERNALSYM BINDSTATUS_LOADINGMIMEHANDLER}
  BINDSTATUS_LOADINGMIMEHANDLER         = BINDSTATUS_DECODING + 1;
  {$EXTERNALSYM BINDSTATUS_CONTENTDISPOSITIONATTACH}
  BINDSTATUS_CONTENTDISPOSITIONATTACH   = BINDSTATUS_LOADINGMIMEHANDLER + 1;
  {$EXTERNALSYM BINDSTATUS_FILTERREPORTMIMETYPE}
  BINDSTATUS_FILTERREPORTMIMETYPE       = BINDSTATUS_CONTENTDISPOSITIONATTACH + 1;
  {$EXTERNALSYM BINDSTATUS_CLSIDCANINSTANTIATE}
  BINDSTATUS_CLSIDCANINSTANTIATE        = BINDSTATUS_FILTERREPORTMIMETYPE + 1;
  {$EXTERNALSYM BINDSTATUS_IUNKNOWNAVAILABLE}
  BINDSTATUS_IUNKNOWNAVAILABLE          = BINDSTATUS_CLSIDCANINSTANTIATE + 1;
  {$EXTERNALSYM BINDSTATUS_DIRECTBIND}
  BINDSTATUS_DIRECTBIND                 = BINDSTATUS_IUNKNOWNAVAILABLE + 1;
  {$EXTERNALSYM BINDSTATUS_RAWMIMETYPE}
  BINDSTATUS_RAWMIMETYPE                = BINDSTATUS_DIRECTBIND + 1;
  {$EXTERNALSYM BINDSTATUS_PROXYDETECTING}
  BINDSTATUS_PROXYDETECTING             = BINDSTATUS_RAWMIMETYPE + 1;
  {$EXTERNALSYM BINDSTATUS_ACCEPTRANGES}
  BINDSTATUS_ACCEPTRANGES               = BINDSTATUS_PROXYDETECTING + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_SENT}
  BINDSTATUS_COOKIE_SENT                = BINDSTATUS_ACCEPTRANGES + 1;
  {$EXTERNALSYM BINDSTATUS_COMPACT_POLICY_RECEIVED}
  BINDSTATUS_COMPACT_POLICY_RECEIVED    = BINDSTATUS_COOKIE_SENT + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_SUPPRESSED}
  BINDSTATUS_COOKIE_SUPPRESSED          = BINDSTATUS_COMPACT_POLICY_RECEIVED + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_UNKNOWN}
  BINDSTATUS_COOKIE_STATE_UNKNOWN       = BINDSTATUS_COOKIE_SUPPRESSED + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_ACCEPT}
  BINDSTATUS_COOKIE_STATE_ACCEPT        = BINDSTATUS_COOKIE_STATE_UNKNOWN + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_REJECT}
  BINDSTATUS_COOKIE_STATE_REJECT        = BINDSTATUS_COOKIE_STATE_ACCEPT + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_PROMPT}
  BINDSTATUS_COOKIE_STATE_PROMPT        = BINDSTATUS_COOKIE_STATE_REJECT + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_LEASH}
  BINDSTATUS_COOKIE_STATE_LEASH         = BINDSTATUS_COOKIE_STATE_PROMPT + 1;
  {$EXTERNALSYM BINDSTATUS_COOKIE_STATE_DOWNGRADE}
  BINDSTATUS_COOKIE_STATE_DOWNGRADE     = BINDSTATUS_COOKIE_STATE_LEASH + 1;
  {$EXTERNALSYM BINDSTATUS_POLICY_HREF}
  BINDSTATUS_POLICY_HREF                = BINDSTATUS_COOKIE_STATE_DOWNGRADE + 1;
  {$EXTERNALSYM BINDSTATUS_P3P_HEADER}
  BINDSTATUS_P3P_HEADER                 = BINDSTATUS_POLICY_HREF + 1;
  {$EXTERNALSYM BINDSTATUS_SESSION_COOKIE_RECEIVED}
  BINDSTATUS_SESSION_COOKIE_RECEIVED    = BINDSTATUS_P3P_HEADER + 1;
  {$EXTERNALSYM BINDSTATUS_PERSISTENT_COOKIE_RECEIVED}
  BINDSTATUS_PERSISTENT_COOKIE_RECEIVED = BINDSTATUS_SESSION_COOKIE_RECEIVED + 1;
  {$EXTERNALSYM BINDSTATUS_SESSION_COOKIES_ALLOWED}
  BINDSTATUS_SESSION_COOKIES_ALLOWED    = BINDSTATUS_PERSISTENT_COOKIE_RECEIVED + 1;
  {$EXTERNALSYM BINDSTATUS_CACHECONTROL}
  BINDSTATUS_CACHECONTROL               = BINDSTATUS_SESSION_COOKIES_ALLOWED + 1;
  {$EXTERNALSYM BINDSTATUS_CONTENTDISPOSITIONFILENAME}
  BINDSTATUS_CONTENTDISPOSITIONFILENAME = BINDSTATUS_CACHECONTROL + 1;
  {$EXTERNALSYM BINDSTATUS_MIMETEXTPLAINMISMATCH}
  BINDSTATUS_MIMETEXTPLAINMISMATCH      = BINDSTATUS_CONTENTDISPOSITIONFILENAME + 1;
  {$EXTERNALSYM BINDSTATUS_PUBLISHERAVAILABLE}
  BINDSTATUS_PUBLISHERAVAILABLE         = BINDSTATUS_MIMETEXTPLAINMISMATCH + 1;
  {$EXTERNALSYM BINDSTATUS_DISPLAYNAMEAVAILABLE}
  BINDSTATUS_DISPLAYNAMEAVAILABLE       = BINDSTATUS_PUBLISHERAVAILABLE + 1;

type
  {$EXTERNALSYM IBindStatusCallback}
  IBindStatusCallback = interface(IUnknown)
  ['{79eac9c1-baf9-11ce-8c82-00aa004ba90b}']
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out pnPriority: Longint): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function OnProgress(ulProgress: ULONG; ulProgressMax, ulStatusCode: ULONG;
      szStatusText: PWideChar): HResult; stdcall;
    function OnStopBinding(hresult: HResult;
      szError: PWideChar): HResult; stdcall;
    function GetBindInfo(out grfBINDF: TBindF;
      var pbindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: TBSCF; dwSize: DWORD;
      pformatetc: PFormatEtc; pstgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(var riid: TIID;
      punk: IUnknown): HResult; stdcall;
  end;

  {$EXTERNALSYM IAuthenticate}
  IAuthenticate = interface(IUnknown)
  ['{79eac9d0-baf9-11ce-8c82-00aa004ba90b}']
    function Authenticate(out phwnd: HWND;
      out pszUsername, pszPassword: PWideChar): HResult; stdcall;
  end;

  {$EXTERNALSYM IHttpNegotiate}
  IHttpNegotiate = interface(IUnknown)
  ['{79eac9d2-baf9-11ce-8c82-00aa004ba90b}']
    function BeginningTransaction(szURL, szHeaders: PWideChar;
      dwReserved: DWORD; out pszAdditionalHeaders: PWideChar): HResult; stdcall;
    function OnResponse(dwResponseCode: DWORD;
      szResponseHeaders, szRequestHeaders: PWideChar;
      out pszAdditionalRequestHeaders: PWideChar): HResult; stdcall;
  end;

const
  {$EXTERNALSYM INVALID_P_ROOT_SECURITY_ID}
  INVALID_P_ROOT_SECURITY_ID = PByte(-1);

type
  {$EXTERNALSYM IHttpNegotiate2}
  IHttpNegotiate2 = interface(IHttpNegotiate)
  ['{4F9F9FCB-E0F4-48eb-B7AB-FA2EA9365CB4}']
    function GetRootSecurityId(pbSecurityId: PByte;
      var pcbSecurityId: DWORD; dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IWinInetFileStream}
  IWinInetFileStream = interface(IUnknown)
  ['{F134C4B7-B1F8-4e75-B886-74B90943BECB}']
    function SetHandleForUnlock(
      hWinInetLockHandle, dwReserved: DWORD): HResult; stdcall;
    function SetDeleteFile(dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IWindowForBindingUI}
  IWindowForBindingUI = interface(IUnknown)
  ['{79eac9d5-bafa-11ce-8c82-00aa004ba90b}']
    function GetWindow(const rguidReason: TGUID;
      out phwnd: HWND): HResult; stdcall;
  end;

type
  {$EXTERNALSYM CIP_STATUS}
  CIP_STATUS = DWORD;
  TCipStatus = DWORD;

const
  {$EXTERNALSYM CIP_DISK_FULL}
  CIP_DISK_FULL                            = 0;
  {$EXTERNALSYM CIP_ACCESS_DENIED}
  CIP_ACCESS_DENIED                        = CIP_DISK_FULL + 1;
  {$EXTERNALSYM CIP_NEWER_VERSION_EXISTS}
  CIP_NEWER_VERSION_EXISTS                 = CIP_ACCESS_DENIED + 1;
  {$EXTERNALSYM CIP_OLDER_VERSION_EXISTS}
  CIP_OLDER_VERSION_EXISTS                 = CIP_NEWER_VERSION_EXISTS + 1;
  {$EXTERNALSYM CIP_NAME_CONFLICT}
  CIP_NAME_CONFLICT                        = CIP_OLDER_VERSION_EXISTS + 1;
  {$EXTERNALSYM CIP_TRUST_VERIFICATION_COMPONENT_MISSING}
  CIP_TRUST_VERIFICATION_COMPONENT_MISSING = CIP_NAME_CONFLICT + 1;
  {$EXTERNALSYM CIP_EXE_SELF_REGISTERATION_TIMEOUT}
  CIP_EXE_SELF_REGISTERATION_TIMEOUT       = CIP_TRUST_VERIFICATION_COMPONENT_MISSING + 1;
  {$EXTERNALSYM CIP_UNSAFE_TO_ABORT}
  CIP_UNSAFE_TO_ABORT                      = CIP_EXE_SELF_REGISTERATION_TIMEOUT + 1;
  {$EXTERNALSYM CIP_NEED_REBOOT}
  CIP_NEED_REBOOT                          = CIP_UNSAFE_TO_ABORT + 1;
  {$EXTERNALSYM CIP_NEED_REBOOT_UI_PERMISSION}
  CIP_NEED_REBOOT_UI_PERMISSION            = CIP_NEED_REBOOT + 1;

type
  {$EXTERNALSYM ICodeInstall}
  ICodeInstall = interface(IWindowForBindingUI)
  ['{79eac9d1-baf9-11ce-8c82-00aa004ba90b}']
    function OnCodeInstallProblem(ulStatusCode: ULONG;
      szDestination, szSource: PWideChar; dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IWinInetInfo}
  IWinInetInfo = interface(IUnknown)
  ['{79eac9d6-bafa-11ce-8c82-00aa004ba90b}']
    function QueryOption(dwOption: DWORD; pBuffer: Pointer;
      var pcbBuf: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM WININETINFO_OPTION_LOCK_HANDLE}
  WININETINFO_OPTION_LOCK_HANDLE = 65534;

type
  {$EXTERNALSYM IHttpSecurity}
  IHttpSecurity = interface(IWindowForBindingUI)
  ['{79eac9d7-bafa-11ce-8c82-00aa004ba90b}']
    function OnSecurityProblem(dwProblem: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IWinInetHttpInfo}
  IWinInetHttpInfo = interface(IWinInetInfo)
  ['{79eac9d8-bafa-11ce-8c82-00aa004ba90b}']
    function QueryInfo(dwOption: DWORD; pBuffer: Pointer;
      var pcbBuf, pdwFlags: PDWORD; dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IWinInetCacheHints}
  IWinInetCacheHints = interface(IUnknown)
  ['{DD1EC3B3-8391-4fdb-A9E6-347C3CAAA7DD}']
    function SetCacheExtension(pwzExt: PWideChar; pszCacheFile: Pointer;
      var pcbCacheFile, pdwWinInetError: DWORD;
      dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IBindHost}
  IBindHost = interface(IUnknown)
  ['{fc4801a1-2ba9-11cf-a229-00aa003d7352}']
    function CreateMoniker(szName: POleStr; pBC: IBindCtx;
      out ppmk: IMoniker; dwReserved: DWORD): HResult; stdcall;
    function MonikerBindToStorage(pMk: IMoniker; pBC: IBindCtx;
      pBSC: IBindStatusCallback; const riid: TIID;
      out ppvObj: Pointer): HResult; stdcall;
    function MonikerBindToObject(pMk: IMoniker; pBC: IBindCtx;
      pBSC: IBindStatusCallback; const riid: TIID;
      out ppvObj: Pointer): HResult; stdcall;
  end;

// These are for backwards compatibility with previous URLMON versions
// Flags for the UrlDownloadToCacheFile
const
  {$EXTERNALSYM URLOSTRM_USECACHEDCOPY_ONLY}
  URLOSTRM_USECACHEDCOPY_ONLY  = $1;      // Only get from cache
  {$EXTERNALSYM URLOSTRM_USECACHEDCOPY}
  URLOSTRM_USECACHEDCOPY       = $2;      // Get from cache if available else download
  {$EXTERNALSYM URLOSTRM_GETNEWESTVERSION}
  URLOSTRM_GETNEWESTVERSION    = $3;      // Get new version only. But put it in cache too

type
  {$EXTERNALSYM IInternet}
  IInternet = interface(IUnknown)
  ['{79eac9e0-baf9-11ce-8c82-00aa004ba90b}']
  end;

  {$EXTERNALSYM tagBINDSTRING}
  tagBINDSTRING = DWORD;
  {$EXTERNALSYM BINDSTRING}
  BINDSTRING = DWORD;
  TBindString = DWORD;

const
  {$EXTERNALSYM BINDSTRING_HEADERS}
  BINDSTRING_HEADERS             = 1;
  {$EXTERNALSYM BINDSTRING_ACCEPT_MIMES}
  BINDSTRING_ACCEPT_MIMES        = BINDSTRING_HEADERS + 1;
  {$EXTERNALSYM BINDSTRING_EXTRA_URL}
  BINDSTRING_EXTRA_URL           = BINDSTRING_ACCEPT_MIMES + 1;
  {$EXTERNALSYM BINDSTRING_LANGUAGE}
  BINDSTRING_LANGUAGE            = BINDSTRING_EXTRA_URL + 1;
  {$EXTERNALSYM BINDSTRING_USERNAME}
  BINDSTRING_USERNAME            = BINDSTRING_LANGUAGE + 1;
  {$EXTERNALSYM BINDSTRING_PASSWORD}
  BINDSTRING_PASSWORD            = BINDSTRING_USERNAME + 1;
  {$EXTERNALSYM BINDSTRING_UA_PIXELS}
  BINDSTRING_UA_PIXELS           = BINDSTRING_PASSWORD + 1;
  {$EXTERNALSYM BINDSTRING_UA_COLOR}
  BINDSTRING_UA_COLOR            = BINDSTRING_UA_PIXELS + 1;
  {$EXTERNALSYM BINDSTRING_OS}
  BINDSTRING_OS                  = BINDSTRING_UA_COLOR + 1;
  {$EXTERNALSYM BINDSTRING_USER_AGENT}
  BINDSTRING_USER_AGENT          = BINDSTRING_OS + 1;
  {$EXTERNALSYM BINDSTRING_ACCEPT_ENCODINGS}
  BINDSTRING_ACCEPT_ENCODINGS    = BINDSTRING_USER_AGENT + 1;
  {$EXTERNALSYM BINDSTRING_POST_COOKIE}
  BINDSTRING_POST_COOKIE         = BINDSTRING_ACCEPT_ENCODINGS + 1;
  {$EXTERNALSYM BINDSTRING_POST_DATA_MIME}
  BINDSTRING_POST_DATA_MIME      = BINDSTRING_POST_COOKIE + 1;
  {$EXTERNALSYM BINDSTRING_URL}
  BINDSTRING_URL                 = BINDSTRING_POST_DATA_MIME + 1;
  {$EXTERNALSYM BINDSTRING_IID}
  BINDSTRING_IID                 = BINDSTRING_URL + 1;
  {$EXTERNALSYM BINDSTRING_FLAG_BIND_TO_OBJECT}
  BINDSTRING_FLAG_BIND_TO_OBJECT = BINDSTRING_IID + 1;
  {$EXTERNALSYM BINDSTRING_PTR_BIND_CONTEXT}
  BINDSTRING_PTR_BIND_CONTEXT    = BINDSTRING_FLAG_BIND_TO_OBJECT + 1;

type
  {$EXTERNALSYM IInternetBindInfo}
  IInternetBindInfo = interface(IUnknown)
  ['{79eac9e1-baf9-11ce-8c82-00aa004ba90b}']
    function GetBindInfo(out grfBINDF: TBindF;
      var pbindinfo: TBindInfo): HResult; stdcall;
    function GetBindString(ulStringType: ULONG; var ppwzStr: POleStr;
      cEl: ULONG; var pcElFetched: ULONG): HResult; stdcall;
  end;

type
  {$EXTERNALSYM _tagPI_FLAGS}
  _tagPI_FLAGS = DWORD;
  {$EXTERNALSYM PI_FLAGS}
  PI_FLAGS = DWORD;
  TPiFlags = DWORD;

const
  {$EXTERNALSYM PI_PARSE_URL}
  PI_PARSE_URL            = $1;
  {$EXTERNALSYM PI_FILTER_MODE}
  PI_FILTER_MODE          = $2;
  {$EXTERNALSYM PI_FORCE_ASYNC}
  PI_FORCE_ASYNC          = $4;
  {$EXTERNALSYM PI_USE_WORKERTHREAD}
  PI_USE_WORKERTHREAD     = $8;
  {$EXTERNALSYM PI_MIMEVERIFICATION}
  PI_MIMEVERIFICATION     = $10;
  {$EXTERNALSYM PI_CLSIDLOOKUP}
  PI_CLSIDLOOKUP          = $20;
  {$EXTERNALSYM PI_DATAPROGRESS}
  PI_DATAPROGRESS         = $40;
  {$EXTERNALSYM PI_SYNCHRONOUS}
  PI_SYNCHRONOUS          = $80;
  {$EXTERNALSYM PI_APARTMENTTHREADED}
  PI_APARTMENTTHREADED    = $100;
  {$EXTERNALSYM PI_CLASSINSTALL}
  PI_CLASSINSTALL         = $200;
  {$EXTERNALSYM PI_PASSONBINDCTX}
  PI_PASSONBINDCTX        = $2000;
  {$EXTERNALSYM PI_NOMIMEHANDLER}
  PI_NOMIMEHANDLER        = $8000;
  {$EXTERNALSYM PI_LOADAPPDIRECT}
  PI_LOADAPPDIRECT        = $4000;
  {$EXTERNALSYM PD_FORCE_SWITCH}
  PD_FORCE_SWITCH         = $10000;
  {$EXTERNALSYM PI_PREFERDEFAULTHANDLER}
  PI_PREFERDEFAULTHANDLER = $20000;

  {$EXTERNALSYM PI_DOCFILECLSIDLOOKUP}
  PI_DOCFILECLSIDLOOKUP   = PI_CLSIDLOOKUP;

type
  PProtocolData = ^TProtocolData;
  {$EXTERNALSYM _tagPROTOCOLDATA}
  _tagPROTOCOLDATA = record
    grfFlags: DWORD;
    dwState: DWORD;
    pData: Pointer;
    cbData: ULONG;
  end;
  {$EXTERNALSYM PROTOCOLDATA}
  PROTOCOLDATA = _tagPROTOCOLDATA;
  TProtocolData = _tagPROTOCOLDATA;

  PStartParam = ^TStartParam;
  {$EXTERNALSYM _tagStartParam}
  _tagStartParam = record
    iid: TIID;
    pIBindCtx: IBindCtx;
    pItf: IUnknown;
  end;
  {$EXTERNALSYM StartParam}
  StartParam = _tagStartParam;
  TStartParam = _tagStartParam;

  {$EXTERNALSYM IInternetProtocolSink}
  IInternetProtocolSink = interface;

  {$EXTERNALSYM IInternetProtocolRoot}
  IInternetProtocolRoot = interface(IUnknown)
  ['{79eac9e3-baf9-11ce-8c82-00aa004ba90b}']
    function Start(szUrl: PWideChar; pOIProtSink: IInternetProtocolSink;
      pOIBindInfo: IInternetBindInfo; grfPI: TPiFlags;
      dwReserved: THandle): HResult; stdcall;
    function Continue(var pProtocolData: TProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWORD): HResult; stdcall;
    function Terminate(dwOptions: DWORD): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
  end;

  {$EXTERNALSYM IInternetProtocol}
  IInternetProtocol = interface(IInternetProtocolRoot)
  ['{79eac9e4-baf9-11ce-8c82-00aa004ba90b}']
    function Read(pv: Pointer; cb: ULONG; out pcbRead: ULONG): HResult; stdcall;
    function Seek(dlibMove: TLargeInteger; dwOrigin: DWORD;
      out plibNewPosition: TULargeInteger): HResult; stdcall;
    function LockRequest(dwOptions: DWORD): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
  end;

  {$EXTERNALSYM IInternetProtocolSink}
  IInternetProtocolSink = interface(IUnknown)
  ['{79eac9e5-baf9-11ce-8c82-00aa004ba90b}']
    function Switch(var pProtocolData: TProtocolData): HResult; stdcall;
    function ReportProgress(ulStatusCode: ULONG;
      szStatusText: PWideChar): HResult; stdcall;
    function ReportData(grfBSCF: DWORD;
      ulProgress, ulProgressMax: ULONG): HResult; stdcall;
    function ReportResult(hrResult: HResult; dwError: DWORD;
      szResult: PWideChar): HResult; stdcall;
  end;

  {$EXTERNALSYM IInternetProtocolSinkStackable}
  IInternetProtocolSinkStackable = interface(IUnknown)
  ['{79eac9f0-baf9-11ce-8c82-00aa004ba90b}']
    function SwitchSink(pOIProtSink: IInternetProtocolSink): HResult; stdcall;
    function CommitSwitch: HResult; stdcall;
    function RollbackSwitch: HResult; stdcall;
  end;

type
  {$EXTERNALSYM _tagOIBDG_FLAGS}
  _tagOIBDG_FLAGS = DWORD;
  {$EXTERNALSYM OIBDG_FLAGS}
  OIBDG_FLAGS = DWORD;
  TOIBDGFlags = DWORD;

const
  {$EXTERNALSYM OIBDG_APARTMENTTHREADED}
  OIBDG_APARTMENTTHREADED = $0100;
  {$EXTERNALSYM OIBDG_DATAONLY}
  OIBDG_DATAONLY          = $1000;

type
  {$EXTERNALSYM IInternetSession}
  IInternetSession = interface(IUnknown)
  ['{79eac9e7-baf9-11ce-8c82-00aa004ba90b}']
    function RegisterNameSpace(pCF: IClassFactory; var rclsid: TCLSID;
      pwzProtocol: PWideChar; cPatterns: ULONG; const ppwzPatterns: PPWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function UnregisterNameSpace(pCF: IClassFactory;
      pszProtocol: PWideChar): HResult; stdcall;
    function RegisterMimeFilter(pCF: IClassFactory; var rclsid: TCLSID;
      pwzType: PWideChar): HResult; stdcall;
    function UnregisterMimeFilter(pCF: IClassFactory;
      pwzType: PWideChar): HResult; stdcall;
    function CreateBinding(pBC: IBindCtx; szUrl: PWideChar;
      pUnkOuter: IUnknown; out ppUnk: IUnknown;
      out ppOInetProt: IInternetProtocol; dwOption: DWORD): HResult; stdcall;
    function SetSessionOption(dwOption: DWORD; pBuffer: Pointer;
      dwBufferLength, dwReserved: DWORD): HResult; stdcall;
    function GetSessionOption(dwOption: DWORD; pBuffer: Pointer;
      var pdwBufferLength: DWORD; dwReserved: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IInternetThreadSwitch}
  IInternetThreadSwitch = interface(IUnknown)
  ['{79eac9e8-baf9-11ce-8c82-00aa004ba90b}']
    function Prepare: HResult; stdcall;
    function Continue: HResult; stdcall;
  end;

  {$EXTERNALSYM IInternetPriority}
  IInternetPriority = interface(IUnknown)
  ['{79eac9eb-baf9-11ce-8c82-00aa004ba90b}']
    function SetPriority(nPriority: Longint): HResult; stdcall;
    function GetPriority(out pnPriority: Longint): HResult; stdcall;
  end;


type
  {$EXTERNALSYM _tagPARSEACTION}
  _tagPARSEACTION = DWORD;
  {$EXTERNALSYM PARSEACTION}
  PARSEACTION = _tagPARSEACTION;
  TParseAction = _tagPARSEACTION;

const
  {$EXTERNALSYM PARSE_CANONICALIZE}
  PARSE_CANONICALIZE    = 1;
  {$EXTERNALSYM PARSE_FRIENDLY}
  PARSE_FRIENDLY        = PARSE_CANONICALIZE + 1;
  {$EXTERNALSYM PARSE_SECURITY_URL}
  PARSE_SECURITY_URL    = PARSE_FRIENDLY + 1;
  {$EXTERNALSYM PARSE_ROOTDOCUMENT}
  PARSE_ROOTDOCUMENT    = PARSE_SECURITY_URL + 1;
  {$EXTERNALSYM PARSE_DOCUMENT}
  PARSE_DOCUMENT        = PARSE_ROOTDOCUMENT + 1;
  {$EXTERNALSYM PARSE_ANCHOR}
  PARSE_ANCHOR          = PARSE_DOCUMENT + 1;
  {$EXTERNALSYM PARSE_ENCODE}
  PARSE_ENCODE          = PARSE_ANCHOR + 1;
  {$EXTERNALSYM PARSE_DECODE}
  PARSE_DECODE          = PARSE_ENCODE + 1;
  {$EXTERNALSYM PARSE_PATH_FROM_URL}
  PARSE_PATH_FROM_URL   = PARSE_DECODE + 1;
  {$EXTERNALSYM PARSE_URL_FROM_PATH}
  PARSE_URL_FROM_PATH   = PARSE_PATH_FROM_URL + 1;
  {$EXTERNALSYM PARSE_MIME}
  PARSE_MIME            = PARSE_URL_FROM_PATH + 1;
  {$EXTERNALSYM PARSE_SERVER}
  PARSE_SERVER          = PARSE_MIME + 1;
  {$EXTERNALSYM PARSE_SCHEMA}
  PARSE_SCHEMA          = PARSE_SERVER + 1;
  {$EXTERNALSYM PARSE_SITE}
  PARSE_SITE            = PARSE_SCHEMA + 1;
  {$EXTERNALSYM PARSE_DOMAIN}
  PARSE_DOMAIN          = PARSE_SITE + 1;
  {$EXTERNALSYM PARSE_LOCATION}
  PARSE_LOCATION        = PARSE_DOMAIN + 1;
  {$EXTERNALSYM PARSE_SECURITY_DOMAIN}
  PARSE_SECURITY_DOMAIN = PARSE_LOCATION + 1;
  {$EXTERNALSYM PARSE_ESCAPE}
  PARSE_ESCAPE          = PARSE_SECURITY_DOMAIN + 1;
  {$EXTERNALSYM PARSE_UNESCAPE}
  PARSE_UNESCAPE        = PARSE_ESCAPE + 1;

type
  {$EXTERNALSYM _tagPSUACTION}
  _tagPSUACTION = DWORD;
  {$EXTERNALSYM _tagPSUACTION}
  PSUACTION = _tagPSUACTION;
  TPSUAction = _tagPSUACTION;

const
  {$EXTERNALSYM PSU_DEFAULT}
  PSU_DEFAULT           = 1;
  {$EXTERNALSYM PSU_SECURITY_URL_ONLY}
  PSU_SECURITY_URL_ONLY = PSU_DEFAULT + 1;

type
  {$EXTERNALSYM _tagQUERYOPTION}
  _tagQUERYOPTION = DWORD;
  {$EXTERNALSYM QUERYOPTION}
  QUERYOPTION = _tagQUERYOPTION;
  TQueryOption = _tagQUERYOPTION;

const
  {$EXTERNALSYM QUERY_EXPIRATION_DATE}
  QUERY_EXPIRATION_DATE     = 1;
  {$EXTERNALSYM QUERY_TIME_OF_LAST_CHANGE}
  QUERY_TIME_OF_LAST_CHANGE = QUERY_EXPIRATION_DATE + 1;
  {$EXTERNALSYM QUERY_CONTENT_ENCODING}
  QUERY_CONTENT_ENCODING    = QUERY_TIME_OF_LAST_CHANGE + 1;
  {$EXTERNALSYM QUERY_CONTENT_TYPE}
  QUERY_CONTENT_TYPE        = QUERY_CONTENT_ENCODING + 1;
  {$EXTERNALSYM QUERY_REFRESH}
  QUERY_REFRESH             = QUERY_CONTENT_TYPE + 1;
  {$EXTERNALSYM QUERY_RECOMBINE}
  QUERY_RECOMBINE           = QUERY_REFRESH + 1;
  {$EXTERNALSYM QUERY_CAN_NAVIGATE}
  QUERY_CAN_NAVIGATE        = QUERY_RECOMBINE + 1;
  {$EXTERNALSYM QUERY_USES_NETWORK}
  QUERY_USES_NETWORK        = QUERY_CAN_NAVIGATE + 1;
  {$EXTERNALSYM QUERY_IS_CACHED}
  QUERY_IS_CACHED           = QUERY_USES_NETWORK + 1;
  {$EXTERNALSYM QUERY_IS_INSTALLEDENTRY}
  QUERY_IS_INSTALLEDENTRY   = QUERY_IS_CACHED + 1;
  {$EXTERNALSYM QUERY_IS_CACHED_OR_MAPPED}
  QUERY_IS_CACHED_OR_MAPPED = QUERY_IS_INSTALLEDENTRY + 1;
  {$EXTERNALSYM QUERY_USES_CACHE}
  QUERY_USES_CACHE          = QUERY_IS_CACHED_OR_MAPPED + 1;
  {$EXTERNALSYM QUERY_IS_SECURE}
  QUERY_IS_SECURE           = QUERY_USES_CACHE + 1;
  {$EXTERNALSYM QUERY_IS_SAFE}
  QUERY_IS_SAFE             = QUERY_IS_SECURE + 1;

type
  {$EXTERNALSYM IInternetProtocolInfo}
  IInternetProtocolInfo = interface(IUnknown)
  ['{79eac9ec-baf9-11ce-8c82-00aa004ba90b}']
    function ParseUrl(pwzUrl: PWideChar; ParseAction: TParseAction;
      dwParseFlags: DWORD; pwzResult: PWideChar; ccHResult: DWORD;
      out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
    function CombineUrl(pwzBaseUrl, pwzRelativeUrl: PWideChar;
      dwCombineFlags: DWORD; pwzResult: PWideChar; ccHResult: DWORD;
      out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
    function CompareUrl(pwzUrl1, pwzUrl2: PWideChar;
      dwCompareFlags: DWORD): HResult; stdcall;
    function QueryInfo(pwzUrl: PWideChar; QueryOption: TQueryOption;
      dwQueryFlags: DWORD; pBuffer: Pointer; cbBuffer: DWORD;
      var pcbBuf: DWORD; dwReserved: DWORD): HResult; stdcall;
  end;

type
  {$EXTERNALSYM IOInet}
  IOInet               = IInternet;
  {$EXTERNALSYM IOInetBindInfo}
  IOInetBindInfo       = IInternetBindInfo;
  {$EXTERNALSYM IOInetProtocolRoot}
  IOInetProtocolRoot   = IInternetProtocolRoot;
  {$EXTERNALSYM IOInetProtocol}
  IOInetProtocol       = IInternetProtocol;
  {$EXTERNALSYM IOInetProtocolSink}
  IOInetProtocolSink   = IInternetProtocolSink;
  {$EXTERNALSYM IOInetProtocolInfo}
  IOInetProtocolInfo   = IInternetProtocolInfo;
  {$EXTERNALSYM IOInetSession}
  IOInetSession        = IInternetSession;
  {$EXTERNALSYM IOInetPriority}
  IOInetPriority       = IInternetPriority;
  {$EXTERNALSYM IOInetThreadSwitch}
  IOInetThreadSwitch   = IInternetThreadSwitch;
  {$EXTERNALSYM IOInetProtocolSinkStackable}
  IOInetProtocolSinkStackable   = IInternetProtocolSinkStackable;

type
  {$EXTERNALSYM _tagINTERNETFEATURELIST}
  _tagINTERNETFEATURELIST = DWORD;
  {$EXTERNALSYM INTERNETFEATURELIST}
  INTERNETFEATURELIST = _tagINTERNETFEATURELIST;
  TInternetFeatureList = _tagINTERNETFEATURELIST;

const
  {$EXTERNALSYM FEATURE_OBJECT_CACHING}
  FEATURE_OBJECT_CACHING                 = 0;
  {$EXTERNALSYM FEATURE_ZONE_ELEVATION}
  FEATURE_ZONE_ELEVATION                 = FEATURE_OBJECT_CACHING + 1;
  {$EXTERNALSYM FEATURE_MIME_HANDLING}
  FEATURE_MIME_HANDLING                  = FEATURE_ZONE_ELEVATION + 1;
  {$EXTERNALSYM FEATURE_MIME_SNIFFING}
  FEATURE_MIME_SNIFFING                  = FEATURE_MIME_HANDLING + 1;
  {$EXTERNALSYM FEATURE_WINDOW_RESTRICTIONS}
  FEATURE_WINDOW_RESTRICTIONS            = FEATURE_MIME_SNIFFING + 1;
  {$EXTERNALSYM FEATURE_WEBOC_POPUPMANAGEMENT}
  FEATURE_WEBOC_POPUPMANAGEMENT          = FEATURE_WINDOW_RESTRICTIONS + 1;
  {$EXTERNALSYM FEATURE_BEHAVIORS}
  FEATURE_BEHAVIORS                      = FEATURE_WEBOC_POPUPMANAGEMENT + 1;
  {$EXTERNALSYM FEATURE_DISABLE_MK_PROTOCOL}
  FEATURE_DISABLE_MK_PROTOCOL            = FEATURE_BEHAVIORS + 1;
  {$EXTERNALSYM FEATURE_LOCALMACHINE_LOCKDOWN}
  FEATURE_LOCALMACHINE_LOCKDOWN          = FEATURE_DISABLE_MK_PROTOCOL + 1;
  {$EXTERNALSYM FEATURE_SECURITYBAND}
  FEATURE_SECURITYBAND                   = FEATURE_LOCALMACHINE_LOCKDOWN + 1;
  {$EXTERNALSYM FEATURE_RESTRICT_ACTIVEXINSTALL}
  FEATURE_RESTRICT_ACTIVEXINSTALL        = FEATURE_SECURITYBAND + 1;
  {$EXTERNALSYM FEATURE_VALIDATE_NAVIGATE_URL}
  FEATURE_VALIDATE_NAVIGATE_URL          = FEATURE_RESTRICT_ACTIVEXINSTALL + 1;
  {$EXTERNALSYM FEATURE_RESTRICT_FILEDOWNLOAD}
  FEATURE_RESTRICT_FILEDOWNLOAD          = FEATURE_VALIDATE_NAVIGATE_URL + 1;
  {$EXTERNALSYM FEATURE_ADDON_MANAGEMENT}
  FEATURE_ADDON_MANAGEMENT               = FEATURE_RESTRICT_FILEDOWNLOAD + 1;
  {$EXTERNALSYM FEATURE_PROTOCOL_LOCKDOWN}
  FEATURE_PROTOCOL_LOCKDOWN              = FEATURE_ADDON_MANAGEMENT + 1;
  {$EXTERNALSYM FEATURE_HTTP_USERNAME_PASSWORD_DISABLE}
  FEATURE_HTTP_USERNAME_PASSWORD_DISABLE = FEATURE_PROTOCOL_LOCKDOWN + 1;
  {$EXTERNALSYM FEATURE_SAFE_BINDTOOBJECT}
  FEATURE_SAFE_BINDTOOBJECT              = FEATURE_HTTP_USERNAME_PASSWORD_DISABLE + 1;
  {$EXTERNALSYM FEATURE_UNC_SAVEDFILECHECK}
  FEATURE_UNC_SAVEDFILECHECK             = FEATURE_SAFE_BINDTOOBJECT + 1;
  {$EXTERNALSYM FEATURE_GET_URL_DOM_FILEPATH_UNENCODED}
  FEATURE_GET_URL_DOM_FILEPATH_UNENCODED = FEATURE_UNC_SAVEDFILECHECK + 1;
  {$EXTERNALSYM FEATURE_ENTRY_COUNT}
  FEATURE_ENTRY_COUNT                    = FEATURE_GET_URL_DOM_FILEPATH_UNENCODED + 1;

// CoInternetSetFeatureEnabled can be used to set/reset features.
// The following flags control where the feature is set

  {$EXTERNALSYM SET_FEATURE_ON_THREAD}
  SET_FEATURE_ON_THREAD                       = $00000001;
  {$EXTERNALSYM SET_FEATURE_ON_PROCESS}
  SET_FEATURE_ON_PROCESS                      = $00000002;
  {$EXTERNALSYM SET_FEATURE_IN_REGISTRY}
  SET_FEATURE_IN_REGISTRY                     = $00000004;
  {$EXTERNALSYM SET_FEATURE_ON_THREAD_LOCALMACHINE}
  SET_FEATURE_ON_THREAD_LOCALMACHINE          = $00000008;
  {$EXTERNALSYM SET_FEATURE_ON_THREAD_INTRANET}
  SET_FEATURE_ON_THREAD_INTRANET              = $00000010;
  {$EXTERNALSYM SET_FEATURE_ON_THREAD_TRUSTED}
  SET_FEATURE_ON_THREAD_TRUSTED               = $00000020;
  {$EXTERNALSYM SET_FEATURE_ON_THREAD_INTERNET}
  SET_FEATURE_ON_THREAD_INTERNET              = $00000040;
  {$EXTERNALSYM SET_FEATURE_ON_THREAD_RESTRICTED}
  SET_FEATURE_ON_THREAD_RESTRICTED            = $00000080;

// CoInternetIsFeatureEnabled can be used to get features.
// The following flags control where the feature is obtained from
// default is from process

  {$EXTERNALSYM GET_FEATURE_FROM_THREAD}
  GET_FEATURE_FROM_THREAD                      = $00000001;
  {$EXTERNALSYM GET_FEATURE_FROM_PROCESS}
  GET_FEATURE_FROM_PROCESS                     = $00000002;
  {$EXTERNALSYM GET_FEATURE_FROM_REGISTRY}
  GET_FEATURE_FROM_REGISTRY                    = $00000004;
  {$EXTERNALSYM GET_FEATURE_FROM_THREAD_LOCALMACHINE}
  GET_FEATURE_FROM_THREAD_LOCALMACHINE         = $00000008;
  {$EXTERNALSYM GET_FEATURE_FROM_THREAD_INTRANET}
  GET_FEATURE_FROM_THREAD_INTRANET             = $00000010;
  {$EXTERNALSYM GET_FEATURE_FROM_THREAD_TRUSTED}
  GET_FEATURE_FROM_THREAD_TRUSTED              = $00000020;
  {$EXTERNALSYM GET_FEATURE_FROM_THREAD_INTERNET}
  GET_FEATURE_FROM_THREAD_INTERNET             = $00000040;
  {$EXTERNALSYM GET_FEATURE_FROM_THREAD_RESTRICTED}
  GET_FEATURE_FROM_THREAD_RESTRICTED           = $00000080;

  {$EXTERNALSYM INET_E_USE_DEFAULT_PROTOCOLHANDLER}
  INET_E_USE_DEFAULT_PROTOCOLHANDLER = HResult($800C0011);
  {$EXTERNALSYM INET_E_USE_DEFAULT_SETTING}
  INET_E_USE_DEFAULT_SETTING         = HResult($800C0012);
  {$EXTERNALSYM INET_E_DEFAULT_ACTION}
  INET_E_DEFAULT_ACTION              = INET_E_USE_DEFAULT_PROTOCOLHANDLER;
  {$EXTERNALSYM INET_E_QUERYOPTION_UNKNOWN}
  INET_E_QUERYOPTION_UNKNOWN         = HResult($800C0013);
  {$EXTERNALSYM INET_E_REDIRECTING}
  INET_E_REDIRECTING                 = HResult($800C0014);

//
// Static Protocol flags
//
  PROTOCOLFLAG_NO_PICS_CHECK     = $00000001;

(*
// Security manager CLSID's
EXTERN_C const IID CLSID_InternetSecurityManager;
EXTERN_C const IID CLSID_InternetZoneManager;
EXTERN_C const IID CLSID_PersistentZoneIdentifier;
*)

(*
// This service is used for delegation support on the Security Manager interface
#define SID_SInternetSecurityManager         IID_IInternetSecurityManager

#define SID_SInternetSecurityManagerEx         IID_IInternetSecurityManagerEx

#define SID_SInternetHostSecurityManager     IID_IInternetHostSecurityManager
*)

type
  {$EXTERNALSYM IInternetSecurityMgrSite}
  IInternetSecurityMgrSite = interface(IUnknown)
  ['{79eac9ed-baf9-11ce-8c82-00aa004ba90b}']
    function GetWindow(var phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM MUTZ_NOSAVEDFILECHECK}
  MUTZ_NOSAVEDFILECHECK        = $00000001; // don't check file: for saved file comment
  {$EXTERNALSYM MUTZ_ISFILE}
  MUTZ_ISFILE                  = $00000002; // Assume URL if File, url does not need file://
  {$EXTERNALSYM MUTZ_ACCEPT_WILDCARD_SCHEME}
  MUTZ_ACCEPT_WILDCARD_SCHEME  = $00000080; // Accept a wildcard scheme
  {$EXTERNALSYM MUTZ_ENFORCERESTRICTED}
  MUTZ_ENFORCERESTRICTED       = $00000100; // enforce restricted zone independent of URL
  {$EXTERNALSYM MUTZ_REQUIRESAVEDFILECHECK}
  MUTZ_REQUIRESAVEDFILECHECK   = $00000400; // always check the file for MOTW (overriding FEATURE_UNC_SAVEDFILECHECK)
  {$EXTERNALSYM MUTZ_DONT_UNESCAPE}
  MUTZ_DONT_UNESCAPE           = $00000800; // Do not unescape the url
// MapUrlToZone returns the zone index given a URL
  {$EXTERNALSYM MAX_SIZE_SECURITY_ID}
  MAX_SIZE_SECURITY_ID         = 512; // bytes

type
  {$EXTERNALSYM PUAF}
  PUAF = DWORD;
  TPUAF = DWORD;

const
  {$EXTERNALSYM PUAF_DEFAULT}
  PUAF_DEFAULT                      = 0;
  {$EXTERNALSYM PUAF_NOUI}
  PUAF_NOUI                         = $1;
  {$EXTERNALSYM PUAF_ISFILE}
  PUAF_ISFILE                       = $2;
  {$EXTERNALSYM PUAF_WARN_IF_DENIED}
  PUAF_WARN_IF_DENIED               = $4;
  {$EXTERNALSYM PUAF_FORCEUI_FOREGROUND}
  PUAF_FORCEUI_FOREGROUND           = $8;
  {$EXTERNALSYM PUAF_CHECK_TIFS}
  PUAF_CHECK_TIFS                   = $10;
  {$EXTERNALSYM PUAF_DONTCHECKBOXINDIALOG}
  PUAF_DONTCHECKBOXINDIALOG         = $20;
  {$EXTERNALSYM PUAF_TRUSTED}
  PUAF_TRUSTED                      = $40;
  {$EXTERNALSYM PUAF_ACCEPT_WILDCARD_SCHEME}
  PUAF_ACCEPT_WILDCARD_SCHEME       = $80;
  {$EXTERNALSYM PUAF_ENFORCERESTRICTED}
  PUAF_ENFORCERESTRICTED            = $100;
  {$EXTERNALSYM PUAF_NOSAVEDFILECHECK}
  PUAF_NOSAVEDFILECHECK             = $200;
  {$EXTERNALSYM PUAF_REQUIRESAVEDFILECHECK}
  PUAF_REQUIRESAVEDFILECHECK        = $400;
  {$EXTERNALSYM PUAF_LMZ_UNLOCKED}
  PUAF_LMZ_UNLOCKED                 = $10000;
  {$EXTERNALSYM PUAF_LMZ_LOCKED}
  PUAF_LMZ_LOCKED                   = $20000;
  {$EXTERNALSYM PUAF_DEFAULTZONEPOL}
  PUAF_DEFAULTZONEPOL               = $40000;
  {$EXTERNALSYM PUAF_NPL_USE_LOCKED_IF_RESTRICTED}
  PUAF_NPL_USE_LOCKED_IF_RESTRICTED = $80000;
  {$EXTERNALSYM PUAF_NOUIIFLOCKED}
  PUAF_NOUIIFLOCKED                 = $100000;
  {$EXTERNALSYM PUAF_DRAGPROTOCOLCHECK}
  PUAF_DRAGPROTOCOLCHECK            = $200000;

type
  {$EXTERNALSYM PUAFOUT}
  PUAFOUT = DWORD;
  TPUAFOut = DWORD;

const
  {$EXTERNALSYM PUAFOUT_DEFAULT}
  PUAFOUT_DEFAULT          = 0;
  {$EXTERNALSYM PUAFOUT_ISLOCKZONEPOLICY}
  PUAFOUT_ISLOCKZONEPOLICY = $1;

// This is the wrapper function that most clients will use.
// It figures out the current Policy for the passed in Action,
// and puts up UI if the current Policy indicates that the user
// should be queried. It returns back the Policy which the caller
// will use to determine if the action should be allowed
// This is the wrapper function to conveniently read a custom policy.

type
  {$EXTERNALSYM SZM_FLAGS}
  SZM_FLAGS = DWORD;
  TSzmFlags = DWORD;

const
  {$EXTERNALSYM SZM_CREATE}
  SZM_CREATE = 0;
  {$EXTERNALSYM SZM_DELETE}
  SZM_DELETE = $1;


// SetZoneMapping
//    lpszPattern: string denoting a URL pattern
//        Examples of valid patterns:
//            *://*.msn.com
//            http://*.sony.co.jp
//            *://et.msn.com
//            ftp://157.54.23.41/
//            https://localsvr
//            file:\localsvr\share
//            *://157.54.100-200.*
//        Examples of invalid patterns:
//            http://*.lcs.mit.edu
//            ftp://*
//    dwFlags: SZM_FLAGS values

type
  {$EXTERNALSYM IInternetSecurityManager}
  IInternetSecurityManager = interface(IUnknown)
  ['{79eac9ee-baf9-11ce-8c82-00aa004ba90b}']
    function SetSecuritySite(pSite: IInternetSecurityMgrSite): HResult; stdcall;
    function GetSecuritySite(
      out ppSite: IInternetSecurityMgrSite): HResult; stdcall;
    function MapUrlToZone(pwszUrl: PWideChar; out pdwZone: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetSecurityId(pwszUrl: PWideChar; pbSecurityId: PByte;
      var pcbSecurityId: DWORD; dwReserved: DWORD): HResult; stdcall;
    function ProcessUrlAction(pwszUrl: PWideChar; dwAction: DWORD;
      pPolicy: PByte; cbPolicy: DWORD; pContext: PByte; cbContext: DWORD;
      dwFlags, dwReserved: DWORD): HResult; stdcall;
    function QueryCustomPolicy(pwszUrl: PWideChar; const guidKey: TGUID;
      out ppPolicy: PByte; out pcbPolicy: DWORD; pContext: PByte;
      cbContext, dwReserved: DWORD): HResult; stdcall;
    function SetZoneMapping(dwZone: DWORD; lpszPattern: PWideChar;
      dwFlags: DWORD): HResult; stdcall;
    function GetZoneMappings(dwZone: DWORD;out ppenumString: IEnumString;
      dwFlags: DWORD): HResult; stdcall;
  end;

// This is the wrapper function that most clients will use.
// It figures out the current Policy for the passed in Action,
// and puts up UI if the current Policy indicates that the user
// should be queried. It returns back the Policy which the caller
// will use to determine if the action should be allowed

  {$EXTERNALSYM IInternetSecurityManagerEx}
  IInternetSecurityManagerEx = interface(IInternetSecurityManager)
  ['{F164EDF1-CC7C-4f0d-9A94-34222625C393}']
    function ProcessUrlActionEx(pwszUrl: PWideChar; dwAction: DWORD;
      pPolicy: PByte; cbPolicy: DWORD; pContext: PByte; cbContext: DWORD;
      dwFlags, dwReserved: DWORD; out pdwOutFlags: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IZoneIdentifier}
  IZoneIdentifier = interface(IUnknown)
  ['{cd45f185-1b21-48e2-967b-ead743a8914e}']
    function GetId(out pdwZone: DWORD): HResult; stdcall;
    function SetId(dwZone: DWORD): HResult; stdcall;
    function Remove: HResult; stdcall;
  end;

//This is the interface MSHTML exposes to its clients
//The clients need not pass in a URL to these functions
//since MSHTML maintains the notion of the current URL

  {$EXTERNALSYM IInternetHostSecurityManager}
  IInternetHostSecurityManager = interface(IUnknown)
  ['{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}']
    function GetSecurityId(pbSecurityId: PByte; out pcbSecurityId: DWORD;
      dwReserved: DWORD): HResult; stdcall;
    function ProcessUrlAction(dwAction: DWORD; pPolicy: PByte; cbPolicy: DWORD;
      pContext: PByte; cbContext, dwFlags, dwReserved: DWORD): HResult; stdcall;
    function QueryCustomPolicy(const guidKey: TGUID; out ppPolicy: PByte;
      out pcbPolicy: DWORD; pContext: PByte;
      cbContext, dwReserved: DWORD): HResult; stdcall;
  end;

// The zone manager maintains policies for a set of standard actions.
// These actions are identified by integral values (called action indexes)
// specified below.

// Minimum legal value for an action
const
  {$EXTERNALSYM URLACTION_MIN}
  URLACTION_MIN                                   = $00001000;

  {$EXTERNALSYM URLACTION_DOWNLOAD_MIN}
  URLACTION_DOWNLOAD_MIN                          = $00001000;
  {$EXTERNALSYM URLACTION_DOWNLOAD_SIGNED_ACTIVEX}
  URLACTION_DOWNLOAD_SIGNED_ACTIVEX               = $00001001;
  {$EXTERNALSYM URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX}
  URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX             = $00001004;
  {$EXTERNALSYM URLACTION_DOWNLOAD_CURR_MAX}
  URLACTION_DOWNLOAD_CURR_MAX                     = $00001004;
  {$EXTERNALSYM URLACTION_DOWNLOAD_MAX}
  URLACTION_DOWNLOAD_MAX                          = $000011FF;

  {$EXTERNALSYM URLACTION_ACTIVEX_MIN}
  URLACTION_ACTIVEX_MIN                           = $00001200;
  {$EXTERNALSYM URLACTION_ACTIVEX_RUN}
  URLACTION_ACTIVEX_RUN                           = $00001200;
  {$EXTERNALSYM URLPOLICY_ACTIVEX_CHECK_LIST}
  URLPOLICY_ACTIVEX_CHECK_LIST                    = $00010000;
  {$EXTERNALSYM URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY}
  URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY        = $00001201; // aggregate next four
  {$EXTERNALSYM URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY}
  URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY          = $00001202; //
  {$EXTERNALSYM URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY}
  URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY        = $00001203; //
  {$EXTERNALSYM URLACTION_SCRIPT_OVERRIDE_SAFETY}
  URLACTION_SCRIPT_OVERRIDE_SAFETY                = $00001401; //
  {$EXTERNALSYM URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY}
  URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY        = $00001204; //
  {$EXTERNALSYM URLACTION_ACTIVEX_TREATASUNTRUSTED}
  URLACTION_ACTIVEX_TREATASUNTRUSTED              = $00001205;
  {$EXTERNALSYM URLACTION_ACTIVEX_NO_WEBOC_SCRIPT}
  URLACTION_ACTIVEX_NO_WEBOC_SCRIPT               = $00001206;
  {$EXTERNALSYM URLACTION_ACTIVEX_CURR_MAX}
  URLACTION_ACTIVEX_CURR_MAX                      = $00001206;
  {$EXTERNALSYM URLACTION_ACTIVEX_MAX}
  URLACTION_ACTIVEX_MAX                           = $000013FF;

  {$EXTERNALSYM URLACTION_SCRIPT_MIN}
  URLACTION_SCRIPT_MIN                            = $00001400;
  {$EXTERNALSYM URLACTION_SCRIPT_RUN}
  URLACTION_SCRIPT_RUN                            = $00001400;
  {$EXTERNALSYM URLACTION_SCRIPT_JAVA_USE}
  URLACTION_SCRIPT_JAVA_USE                       = $00001402;
  {$EXTERNALSYM URLACTION_SCRIPT_SAFE_ACTIVEX}
  URLACTION_SCRIPT_SAFE_ACTIVEX                   = $00001405;
  {$EXTERNALSYM URLACTION_CROSS_DOMAIN_DATA}
  URLACTION_CROSS_DOMAIN_DATA                     = $00001406;
  {$EXTERNALSYM URLACTION_SCRIPT_PASTE}
  URLACTION_SCRIPT_PASTE                          = $00001407;
  {$EXTERNALSYM URLACTION_SCRIPT_CURR_MAX}
  URLACTION_SCRIPT_CURR_MAX                       = $00001407;
  {$EXTERNALSYM URLACTION_SCRIPT_MAX}
  URLACTION_SCRIPT_MAX                            = $000015FF;

  {$EXTERNALSYM URLACTION_HTML_MIN}
  URLACTION_HTML_MIN                              = $00001600;
  {$EXTERNALSYM URLACTION_HTML_SUBMIT_FORMS}
  URLACTION_HTML_SUBMIT_FORMS                     = $00001601; // aggregate next two
  {$EXTERNALSYM URLACTION_HTML_SUBMIT_FORMS_FROM}
  URLACTION_HTML_SUBMIT_FORMS_FROM                = $00001602; //
  {$EXTERNALSYM URLACTION_HTML_SUBMIT_FORMS_TO}
  URLACTION_HTML_SUBMIT_FORMS_TO                  = $00001603; //
  {$EXTERNALSYM URLACTION_HTML_FONT_DOWNLOAD}
  URLACTION_HTML_FONT_DOWNLOAD                    = $00001604;
  {$EXTERNALSYM URLACTION_HTML_JAVA_RUN}
  URLACTION_HTML_JAVA_RUN                         = $00001605; // derive from Java custom policy
  {$EXTERNALSYM URLACTION_HTML_USERDATA_SAVE}
  URLACTION_HTML_USERDATA_SAVE                    = $00001606;
  {$EXTERNALSYM URLACTION_HTML_SUBFRAME_NAVIGATE}
  URLACTION_HTML_SUBFRAME_NAVIGATE                = $00001607;
  {$EXTERNALSYM URLACTION_HTML_META_REFRESH}
  URLACTION_HTML_META_REFRESH                     = $00001608;
  {$EXTERNALSYM URLACTION_HTML_MIXED_CONTENT}
  URLACTION_HTML_MIXED_CONTENT                    = $00001609;
  {$EXTERNALSYM URLACTION_HTML_MAX}
  URLACTION_HTML_MAX                              = $000017FF;

  {$EXTERNALSYM URLACTION_SHELL_MIN}
  URLACTION_SHELL_MIN                             = $00001800;
  {$EXTERNALSYM URLACTION_SHELL_INSTALL_DTITEMS}
  URLACTION_SHELL_INSTALL_DTITEMS                 = $00001800;
  {$EXTERNALSYM URLACTION_SHELL_MOVE_OR_COPY}
  URLACTION_SHELL_MOVE_OR_COPY                    = $00001802;
  {$EXTERNALSYM URLACTION_SHELL_FILE_DOWNLOAD}
  URLACTION_SHELL_FILE_DOWNLOAD                   = $00001803;
  {$EXTERNALSYM URLACTION_SHELL_VERB}
  URLACTION_SHELL_VERB                            = $00001804;
  {$EXTERNALSYM URLACTION_SHELL_WEBVIEW_VERB}
  URLACTION_SHELL_WEBVIEW_VERB                    = $00001805;
  {$EXTERNALSYM URLACTION_SHELL_SHELLEXECUTE}
  URLACTION_SHELL_SHELLEXECUTE                    = $00001806;
  {$EXTERNALSYM URLACTION_SHELL_EXECUTE_HIGHRISK}
  URLACTION_SHELL_EXECUTE_HIGHRISK                = $00001806;
  {$EXTERNALSYM URLACTION_SHELL_EXECUTE_MODRISK}
  URLACTION_SHELL_EXECUTE_MODRISK                 = $00001807;
  {$EXTERNALSYM URLACTION_SHELL_EXECUTE_LOWRISK}
  URLACTION_SHELL_EXECUTE_LOWRISK                 = $00001808;
  {$EXTERNALSYM URLACTION_SHELL_POPUPMGR}
  URLACTION_SHELL_POPUPMGR                        = $00001809;
  {$EXTERNALSYM URLACTION_SHELL_RTF_OBJECTS_LOAD}
  URLACTION_SHELL_RTF_OBJECTS_LOAD                = $0000180A;
  {$EXTERNALSYM URLACTION_SHELL_ENHANCED_DRAGDROP_SECURITY}
  URLACTION_SHELL_ENHANCED_DRAGDROP_SECURITY      = $0000180B;
  {$EXTERNALSYM URLACTION_SHELL_CURR_MAX}
  URLACTION_SHELL_CURR_MAX                        = $0000180B;
  {$EXTERNALSYM URLACTION_SHELL_MAX}
  URLACTION_SHELL_MAX                             = $000019FF;

  {$EXTERNALSYM URLACTION_NETWORK_MIN}
  URLACTION_NETWORK_MIN                           = $00001A00;

  {$EXTERNALSYM URLACTION_CREDENTIALS_USE}
  URLACTION_CREDENTIALS_USE                       = $00001A00;
  {$EXTERNALSYM URLPOLICY_CREDENTIALS_SILENT_LOGON_OK}
  URLPOLICY_CREDENTIALS_SILENT_LOGON_OK           = $00000000;
  {$EXTERNALSYM URLPOLICY_CREDENTIALS_MUST_PROMPT_USER}
  URLPOLICY_CREDENTIALS_MUST_PROMPT_USER          = $00010000;
  {$EXTERNALSYM URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT}
  URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT        = $00020000;
  {$EXTERNALSYM URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY}
  URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY            = $00030000;

  {$EXTERNALSYM URLACTION_AUTHENTICATE_CLIENT}
  URLACTION_AUTHENTICATE_CLIENT                   = $00001A01;
  {$EXTERNALSYM URLPOLICY_AUTHENTICATE_CLEARTEXT_OK}
  URLPOLICY_AUTHENTICATE_CLEARTEXT_OK             = $00000000;
  {$EXTERNALSYM URLPOLICY_AUTHENTICATE_CHALLENGE_RESPONSE}
  URLPOLICY_AUTHENTICATE_CHALLENGE_RESPONSE       = $00010000;
  {$EXTERNALSYM URLPOLICY_AUTHENTICATE_MUTUAL_ONLY}
  URLPOLICY_AUTHENTICATE_MUTUAL_ONLY              = $00030000;

  {$EXTERNALSYM URLACTION_COOKIES}
  URLACTION_COOKIES                               = $00001A02;
  {$EXTERNALSYM URLACTION_COOKIES_SESSION}
  URLACTION_COOKIES_SESSION                       = $00001A03;
  {$EXTERNALSYM URLACTION_CLIENT_CERT_PROMPT}
  URLACTION_CLIENT_CERT_PROMPT                    = $00001A04;
  {$EXTERNALSYM URLACTION_COOKIES_THIRD_PARTY}
  URLACTION_COOKIES_THIRD_PARTY                   = $00001A05;
  {$EXTERNALSYM URLACTION_COOKIES_SESSION_THIRD_PARTY}
  URLACTION_COOKIES_SESSION_THIRD_PARTY           = $00001A06;
  {$EXTERNALSYM URLACTION_COOKIES_ENABLED}
  URLACTION_COOKIES_ENABLED                       = $00001A10;

  {$EXTERNALSYM URLACTION_NETWORK_CURR_MAX}
  URLACTION_NETWORK_CURR_MAX                      = $00001A10;
  {$EXTERNALSYM URLACTION_NETWORK_MAX}
  URLACTION_NETWORK_MAX                           = $00001BFF;

  {$EXTERNALSYM URLACTION_JAVA_MIN}
  URLACTION_JAVA_MIN                              = $00001C00;
  {$EXTERNALSYM URLACTION_JAVA_PERMISSIONS}
  URLACTION_JAVA_PERMISSIONS                      = $00001C00;
  {$EXTERNALSYM URLPOLICY_JAVA_PROHIBIT}
  URLPOLICY_JAVA_PROHIBIT                         = $00000000;
  {$EXTERNALSYM URLPOLICY_JAVA_HIGH}
  URLPOLICY_JAVA_HIGH                             = $00010000;
  {$EXTERNALSYM URLPOLICY_JAVA_MEDIUM}
  URLPOLICY_JAVA_MEDIUM                           = $00020000;
  {$EXTERNALSYM URLPOLICY_JAVA_LOW}
  URLPOLICY_JAVA_LOW                              = $00030000;
  {$EXTERNALSYM URLPOLICY_JAVA_CUSTOM}
  URLPOLICY_JAVA_CUSTOM                           = $00800000;
  {$EXTERNALSYM URLACTION_JAVA_CURR_MAX}
  URLACTION_JAVA_CURR_MAX                         = $00001C00;
  {$EXTERNALSYM URLACTION_JAVA_MAX}
  URLACTION_JAVA_MAX                              = $00001CFF;

// The following Infodelivery actions should have no default policies
// in the registry.  They assume that no default policy means fall
// back to the global restriction.  If an admin sets a policy per
// zone, then it overrides the global restriction.

  {$EXTERNALSYM URLACTION_INFODELIVERY_MIN}
  URLACTION_INFODELIVERY_MIN                       = $00001D00;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_ADDING_CHANNELS}
  URLACTION_INFODELIVERY_NO_ADDING_CHANNELS        = $00001D00;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_EDITING_CHANNELS}
  URLACTION_INFODELIVERY_NO_EDITING_CHANNELS       = $00001D01;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS}
  URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS      = $00001D02;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS}
  URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS   = $00001D03;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS}
  URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS  = $00001D04;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS}
  URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS = $00001D05;
  {$EXTERNALSYM URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING}
  URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING        = $00001D06;
  {$EXTERNALSYM URLACTION_INFODELIVERY_CURR_MAX}
  URLACTION_INFODELIVERY_CURR_MAX                  = $00001D06;
  {$EXTERNALSYM URLACTION_INFODELIVERY_MAX}
  URLACTION_INFODELIVERY_MAX                       = $00001DFF;

  {$EXTERNALSYM URLACTION_CHANNEL_SOFTDIST_MIN}
  URLACTION_CHANNEL_SOFTDIST_MIN                   = $00001E00;
  {$EXTERNALSYM URLACTION_CHANNEL_SOFTDIST_PERMISSIONS}
  URLACTION_CHANNEL_SOFTDIST_PERMISSIONS           = $00001E05;
  {$EXTERNALSYM URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT}
  URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT              = $00010000;
  {$EXTERNALSYM URLPOLICY_CHANNEL_SOFTDIST_PRECACHE}
  URLPOLICY_CHANNEL_SOFTDIST_PRECACHE              = $00020000;
  {$EXTERNALSYM URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL}
  URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL           = $00030000;
  {$EXTERNALSYM URLACTION_CHANNEL_SOFTDIST_MAX}
  URLACTION_CHANNEL_SOFTDIST_MAX                   = $00001EFF;

  {$EXTERNALSYM URLACTION_BEHAVIOR_MIN}
  URLACTION_BEHAVIOR_MIN                           = $00002000;
  {$EXTERNALSYM URLACTION_BEHAVIOR_RUN}
  URLACTION_BEHAVIOR_RUN                           = $00002000;
  {$EXTERNALSYM URLPOLICY_BEHAVIOR_CHECK_LIST}
  URLPOLICY_BEHAVIOR_CHECK_LIST                    = $00010000;

// The following actions correspond to the Feature options above.
// However, they are NOT in the same order.
  {$EXTERNALSYM URLACTION_FEATURE_MIN}
  URLACTION_FEATURE_MIN                            = $00002100;
  {$EXTERNALSYM URLACTION_FEATURE_MIME_SNIFFING}
  URLACTION_FEATURE_MIME_SNIFFING                  = $00002100;
  {$EXTERNALSYM URLACTION_FEATURE_ZONE_ELEVATION}
  URLACTION_FEATURE_ZONE_ELEVATION                 = $00002101;
  {$EXTERNALSYM URLACTION_FEATURE_WINDOW_RESTRICTIONS}
  URLACTION_FEATURE_WINDOW_RESTRICTIONS            = $00002102;

  {$EXTERNALSYM URLACTION_AUTOMATIC_DOWNLOAD_UI_MIN}
  URLACTION_AUTOMATIC_DOWNLOAD_UI_MIN              = $00002200;
  {$EXTERNALSYM URLACTION_AUTOMATIC_DOWNLOAD_UI}
  URLACTION_AUTOMATIC_DOWNLOAD_UI                  = $00002200;
  {$EXTERNALSYM URLACTION_AUTOMATIC_ACTIVEX_UI}
  URLACTION_AUTOMATIC_ACTIVEX_UI                   = $00002201;

  {$EXTERNALSYM URLACTION_ALLOW_RESTRICTEDPROTOCOLS}
  URLACTION_ALLOW_RESTRICTEDPROTOCOLS              = $00002300;

// For each action specified above the system maintains
// a set of policies for the action.
// The only policies supported currently are permissions (i.e. is something allowed)
// and logging status.
// IMPORTANT: If you are defining your own policies don't overload the meaning of the
// loword of the policy. You can use the hiword to store any policy bits which are only
// meaningful to your action.
// For an example of how to do this look at the URLPOLICY_JAVA above

// Permissions
  {$EXTERNALSYM URLPOLICY_ALLOW}
  URLPOLICY_ALLOW                = $00;
  {$EXTERNALSYM URLPOLICY_QUERY}
  URLPOLICY_QUERY                = $01;
  {$EXTERNALSYM URLPOLICY_DISALLOW}
  URLPOLICY_DISALLOW             = $03;

// Notifications are not done when user already queried.
  {$EXTERNALSYM URLPOLICY_NOTIFY_ON_ALLOW}
  URLPOLICY_NOTIFY_ON_ALLOW      = $10;
  {$EXTERNALSYM URLPOLICY_NOTIFY_ON_DISALLOW}
  URLPOLICY_NOTIFY_ON_DISALLOW   = $20;

// Logging is done regardless of whether user was queried.
  {$EXTERNALSYM URLPOLICY_LOG_ON_ALLOW}
  URLPOLICY_LOG_ON_ALLOW         = $40;
  {$EXTERNALSYM URLPOLICY_LOG_ON_DISALLOW}
  URLPOLICY_LOG_ON_DISALLOW      = $80;

  {$EXTERNALSYM URLPOLICY_MASK_PERMISSIONS}
  URLPOLICY_MASK_PERMISSIONS     = $0F;

{$EXTERNALSYM GetUrlPolicyPermissions}
function GetUrlPolicyPermissions(dw: DWORD): DWORD; {inline;}
{$EXTERNALSYM SetUrlPolicyPermissions}
procedure SetUrlPolicyPermissions(var dw: DWORD; dw2: DWORD); {inline;}

const
  {$EXTERNALSYM URLPOLICY_DONTCHECKDLGBOX}
  URLPOLICY_DONTCHECKDLGBOX = $100;

// The ordinal #'s that define the predefined zones internet explorer knows about.
// When we support user-defined zones their zone numbers should be between
// URLZONE_USER_MIN and URLZONE_USER_MAX
// Custom policy to query whether the local machine zone
// has been unlocked for current document.

type
  {$EXTERNALSYM tagURLZONE}
  tagURLZONE = DWORD;
  {$EXTERNALSYM URLZONE}
  URLZONE = tagURLZONE;
  TUrlZone = tagURLZONE;

const
  {$EXTERNALSYM URLZONE_PREDEFINED_MIN}
  URLZONE_PREDEFINED_MIN = 0;
  {$EXTERNALSYM URLZONE_LOCAL_MACHINE}
  URLZONE_LOCAL_MACHINE  = 0;
  {$EXTERNALSYM URLZONE_INTRANET}
  URLZONE_INTRANET       = URLZONE_LOCAL_MACHINE + 1;
  {$EXTERNALSYM URLZONE_TRUSTED}
  URLZONE_TRUSTED        = URLZONE_INTRANET + 1;
  {$EXTERNALSYM URLZONE_INTERNET}
  URLZONE_INTERNET       = URLZONE_TRUSTED + 1;
  {$EXTERNALSYM URLZONE_UNTRUSTED}
  URLZONE_UNTRUSTED      = URLZONE_INTERNET + 1;
  {$EXTERNALSYM URLZONE_PREDEFINED_MAX}
  URLZONE_PREDEFINED_MAX = 999;
  {$EXTERNALSYM URLZONE_USER_MIN}
  URLZONE_USER_MIN       = 1000;
  {$EXTERNALSYM URLZONE_USER_MAX}
  URLZONE_USER_MAX       = 10000;

// Enhanced Security Configuration zone mapping flag for IInternetSecurityManager::SetZoneMapping
type
  {$EXTERNALSYM tagURLTEMPLATE}
  tagURLTEMPLATE = DWORD;
  {$EXTERNALSYM URLTEMPLATE}
  URLTEMPLATE = tagURLTEMPLATE;
  TUrlTemplate = URLTemplate;

const
  {$EXTERNALSYM URLTEMPLATE_CUSTOM}
  URLTEMPLATE_CUSTOM         = 0;
  {$EXTERNALSYM URLTEMPLATE_PREDEFINED_MIN}
  URLTEMPLATE_PREDEFINED_MIN = $10000;
  {$EXTERNALSYM URLTEMPLATE_LOW}
  URLTEMPLATE_LOW            = $10000;
  {$EXTERNALSYM URLTEMPLATE_MEDLOW}
  URLTEMPLATE_MEDLOW         = $10500;
  {$EXTERNALSYM URLTEMPLATE_MEDIUM}
  URLTEMPLATE_MEDIUM         = $11000;
  {$EXTERNALSYM URLTEMPLATE_HIGH}
  URLTEMPLATE_HIGH           = $12000;
  {$EXTERNALSYM URLTEMPLATE_PREDEFINED_MAX}
  URLTEMPLATE_PREDEFINED_MAX = $20000;

  {$EXTERNALSYM MAX_ZONE_PATH}
  MAX_ZONE_PATH        = 260;
  {$EXTERNALSYM MAX_ZONE_DESCRIPTION}
  MAX_ZONE_DESCRIPTION = 200;

type
  {$EXTERNALSYM ZAFLAGS}
  ZAFLAGS = DWORD;
  TZAFlags = DWORD;

const
  {$EXTERNALSYM ZAFLAGS_CUSTOM_EDIT}
  ZAFLAGS_CUSTOM_EDIT            = $1;
  {$EXTERNALSYM ZAFLAGS_ADD_SITES}
  ZAFLAGS_ADD_SITES              = $2;
  {$EXTERNALSYM ZAFLAGS_REQUIRE_VERIFICATION}
  ZAFLAGS_REQUIRE_VERIFICATION   = $4;
  {$EXTERNALSYM ZAFLAGS_INCLUDE_PROXY_OVERRIDE}
  ZAFLAGS_INCLUDE_PROXY_OVERRIDE = $8;
  {$EXTERNALSYM ZAFLAGS_INCLUDE_INTRANET_SITES}
  ZAFLAGS_INCLUDE_INTRANET_SITES = $10;
  {$EXTERNALSYM ZAFLAGS_NO_UI}
  ZAFLAGS_NO_UI                  = $20;
  {$EXTERNALSYM ZAFLAGS_SUPPORTS_VERIFICATION}
  ZAFLAGS_SUPPORTS_VERIFICATION  = $40;
  {$EXTERNALSYM ZAFLAGS_UNC_AS_INTRANET}
  ZAFLAGS_UNC_AS_INTRANET        = $80;
  {$EXTERNALSYM ZAFLAGS_USE_LOCKED_ZONES}
  ZAFLAGS_USE_LOCKED_ZONES       = $10000;

type
  PZoneAttributes = ^TZoneAttributes;
  {$EXTERNALSYM _ZONEATTRIBUTES}
  _ZONEATTRIBUTES = record
    cbSize: ULONG;
    szDisplayName: array[0..259] of WideChar;
    szDescription: array[0..199] of WideChar;
    szIconPath: array[0..259] of WideChar;
    dwTemplateMinLevel: DWORD;
    dwTemplateRecommended: DWORD;
    dwTemplateCurrentLevel: DWORD;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM ZONEATTRIBUTES}
  ZONEATTRIBUTES = _ZONEATTRIBUTES;
  TZoneAttributes = _ZONEATTRIBUTES;

// Gets the zone attributes (information in registry other than actual security
// policies associated with the zone).  Zone attributes are fixed as:
// Sets the zone attributes (information in registry other than actual security
// policies associated with the zone).  Zone attributes as above.
// Returns S_OK or ??? if failed to write the zone attributes.
(* Registry Flags

    When reading, default behavior is:
        If HKLM allows override and HKCU value exists
            Then use HKCU value
            Else use HKLM value
    When writing, default behavior is same as HKCU
        If HKLM allows override
           Then Write to HKCU
           Else Fail
*)

type
  {$EXTERNALSYM _URLZONEREG}
  _URLZONEREG = DWORD;
  {$EXTERNALSYM URLZONEREG}
  URLZONEREG = _URLZONEREG;
  TUrlZoneReg = _URLZONEREG;

const
  {$EXTERNALSYM URLZONEREG_DEFAULT}
  URLZONEREG_DEFAULT = 0;
  {$EXTERNALSYM URLZONEREG_HKLM}
  URLZONEREG_HKLM    = URLZONEREG_DEFAULT + 1;
  {$EXTERNALSYM URLZONEREG_HKCU}
  URLZONEREG_HKCU    = URLZONEREG_HKLM + 1;


// Gets a named custom policy associated with a zone;
// e.g. the Java VM settings can be defined with a unique key such as 'Java'.
// Custom policy support is intended to allow extensibility from the predefined
// set of policies that IE4 has built in.
//
// pwszKey is the string name designating the custom policy.  Components are
//   responsible for having unique names.
// ppPolicy is the callee allocated buffer for the policy byte blob; caller is
//   responsible for freeing this buffer eventually.
// pcbPolicy is the size of the byte blob returned.
// dwRegFlags determines how registry is accessed (see above).
// Returns S_OK if key is found and buffer allocated; ??? if key is not found (no buffer alloced).
// Sets a named custom policy associated with a zone;
// e.g. the Java VM settings can be defined with a unique key such as 'Java'.
// Custom policy support is intended to allow extensibility from the predefined
// set of policies that IE4 has built in.
//
// pwszKey is the string name designating the custom policy.  Components are
//   responsible for having unique names.
// ppPolicy is the caller allocated buffer for the policy byte blob.
// pcbPolicy is the size of the byte blob to be set.
// dwRegFlags determines if HTCU or HKLM is set.
// Returns S_OK or ??? if failed to write the zone custom policy.
// Gets action policy associated with a zone, the builtin, fixed-length policies info.

// dwAction is the action code for the action as defined above.
// pPolicy is the caller allocated buffer for the policy data.
// cbPolicy is the size of the caller allocated buffer.
// dwRegFlags determines how registry is accessed (see above).
// Returns S_OK if action is valid; ??? if action is not valid.

type
  {$EXTERNALSYM IInternetZoneManager}
  IInternetZoneManager = interface(IUnknown)
  ['{79eac9ef-baf9-11ce-8c82-00aa004ba90b}']
    function GetZoneAttributes(dwZone: DWORD;
      var pZoneAttributes: TZoneAttributes): HResult; stdcall;
    function SetZoneAttributes(dwZone: DWORD;
      var pZoneAttributes: TZoneAttributes): HResult; stdcall;
    function GetZoneCustomPolicy(dwZone: DWORD; const guidKey: TGUID;
      out ppPolicy: PByte; out pcbPolicy: DWORD;
      urlZoneReg: TUrlZoneReg): HResult; stdcall;
    function SetZoneCustomPolicy(dwZone: DWORD; const guidKey: TGUID;
      pPolicy: PByte; cbPolicy: DWORD;
      urlZoneReg: TUrlZoneReg): HResult; stdcall;
    function GetZoneActionPolicy(dwZone, dwAction: DWORD; pPolicy: PByte;
      cbPolicy: DWORD; urlZoneReg: TUrlZoneReg): HResult; stdcall;
    function SetZoneActionPolicy(dwZone, dwAction: DWORD; pPolicy: PByte;
      cbPolicy: DWORD; urlZoneReg: TUrlZoneReg): HResult; stdcall;
    function PromptAction(dwAction: DWORD; hwndParent: HWND;
      pwszUrl, pwszText: PWideChar; dwPromptFlags: DWORD): HResult; stdcall;
    function LogAction(dwAction: DWORD; pwszUrl, pwszText: PWideChar;
      dwLogFlags: DWORD): HResult; stdcall;
    function CreateZoneEnumerator(out pdwEnum, pdwCount: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetZoneAt(dwEnum, dwIndex: DWORD;
      out pdwZone: DWORD): HResult; stdcall;
    function DestroyZoneEnumerator(dwEnum: DWORD): HResult; stdcall;
    function CopyTemplatePoliciesToZone(
      dwTemplate, dwZone, dwReserved: DWORD): HResult; stdcall;
  end;

// Gets action policy associated with a zone, the builtin, fixed-length policies info.

// dwAction is the action code for the action as defined above.
// pPolicy is the caller allocated buffer for the policy data.
// cbPolicy is the size of the caller allocated buffer.
// dwRegFlags determines how registry is accessed (see above).
// dwFlags determine which registry policies are accessed (see above).
// Returns S_OK if action is valid; ??? if action is not valid.

  {$EXTERNALSYM IInternetZoneManagerEx}
  IInternetZoneManagerEx = interface(IInternetZoneManager)
  ['{A4C23339-8E06-431e-9BF4-7E711C085648}']
    function GetZoneActionPolicyEx(dwZone, dwAction: DWORD; pPolicy: PByte;
      cbPolicy: DWORD; urlZoneReg: TUrlZoneReg;
      dwFlags: DWORD): HResult; stdcall;
    function SetZoneActionPolicyEx(dwZone, dwAction: DWORD; pPolicy: PByte;
      cbPolicy: DWORD; urlZoneReg: TUrlZoneReg;
      dwFlags: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_EMAIL}
  SOFTDIST_FLAG_USAGE_EMAIL         = $00000001;
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_PRECACHE}
  SOFTDIST_FLAG_USAGE_PRECACHE      = $00000002;
  {$EXTERNALSYM SOFTDIST_FLAG_USAGE_AUTOINSTALL}
  SOFTDIST_FLAG_USAGE_AUTOINSTALL   = $00000004;
  {$EXTERNALSYM SOFTDIST_FLAG_DELETE_SUBSCRIPTION}
  SOFTDIST_FLAG_DELETE_SUBSCRIPTION = $00000008;

  {$EXTERNALSYM SOFTDIST_ADSTATE_NONE}
  SOFTDIST_ADSTATE_NONE             = $00000000;
  {$EXTERNALSYM SOFTDIST_ADSTATE_AVAILABLE}
  SOFTDIST_ADSTATE_AVAILABLE        = $00000001;
  {$EXTERNALSYM SOFTDIST_ADSTATE_DOWNLOADED}
  SOFTDIST_ADSTATE_DOWNLOADED       = $00000002;
  {$EXTERNALSYM SOFTDIST_ADSTATE_INSTALLED}
  SOFTDIST_ADSTATE_INSTALLED        = $00000003;

type
  PCodeBaseHold = ^TCodeBaseHold;
  {$EXTERNALSYM _tagCODEBASEHOLD}
  _tagCODEBASEHOLD = record
    cbSize: ULONG;
    szDistUnit: PWideChar;
    szCodeBase: PWideChar;
    dwVersionMS: DWORD;
    dwVersionLS: DWORD;
    dwStyle: DWORD;
  end;
  {$EXTERNALSYM CODEBASEHOLD}
  CODEBASEHOLD = _tagCODEBASEHOLD;
  TCodeBaseHold = _tagCODEBASEHOLD;

  PSoftDistInfo = ^TSoftDistInfo;
  {$EXTERNALSYM _tagSOFTDISTINFO}
  _tagSOFTDISTINFO = record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwAdState: DWORD;
    szTitle: PWideChar;
    szAbstract: PWideChar;
    szHREF: PWideChar;
    dwInstalledVersionMS: DWORD;
    dwInstalledVersionLS: DWORD;
    dwUpdateVersionMS: DWORD;
    dwUpdateVersionLS: DWORD;
    dwAdvertisedVersionMS: DWORD;
    dwAdvertisedVersionLS: DWORD;
    dwReserved: DWORD;
  end;
  {$EXTERNALSYM SOFTDISTINFO}
  SOFTDISTINFO = _tagSOFTDISTINFO;
  TSoftDistInfo = _tagSOFTDISTINFO;

  {$EXTERNALSYM ISoftDistExt}
  ISoftDistExt = interface(IUnknown)
  ['{B15B8DC1-C7E1-11d0-8680-00AA00BDCB71}']
    function ProcessSoftDist(szCDFURL: PWideChar; pSoftDistElement:
{TODO: Delphi 2010 doesn't support IXMLElement obviously.
  Should be renamed when available.
 }
 (*     {$IFDEF DELPHI5}Pointer{$ELSE}{$IFDEF } IXMLElement{$ENDIF};*)
  Pointer;
      var lpsdi: TSoftDistInfo): HResult; stdcall;
    function GetFirstCodeBase(szCodeBase: PPWideChar;
      var dwMaxSize: DWORD): HResult; stdcall;
    function GetNextCodeBase(szCodeBase: PPWideChar;
      var dwMaxSize: DWORD): HResult; stdcall;
    function AsyncInstallDistributionUnit(pbc: IBindCtx; pvReserved: Pointer;
      flags: DWORD; var lpcbh: TCodeBaseHold): HResult; stdcall;
  end;

  {$EXTERNALSYM ICatalogFileInfo}
  ICatalogFileInfo = interface(IUnknown)
  ['{711C7600-6B48-11d1-B403-00AA00B92AF1}']
    function GetCatalogFile(out ppszCatalogFile: PAnsiChar): HResult; stdcall;
    function GetJavaTrust(out ppJavaTrust: Pointer): HResult; stdcall;
  end;

  {$EXTERNALSYM IDataFilter}
  IDataFilter = interface(IUnknown)
  ['{69d14c80-c18e-11d0-a9ce-006097942311}']
    function DoEncode(dwFlags: DWORD; lInBufferSize: Longint; pbInBuffer: PByte;
      lOutBufferSize: Longint; pbOutBuffer: PByte; lInBytesAvailable: Longint;
      out plInBytesRead, plOutBytesWritten: Longint;
      dwReserved: DWORD): HResult; stdcall;
    function DoDecode(dwFlags: DWORD; lInBufferSize: Longint; pbInBuffer: PByte;
      lOutBufferSize: Longint; pbOutBuffer: PByte; lInBytesAvailable: Longint;
      out plInBytesRead, plOutBytesWritten: Longint;
      dwReserved: DWORD): HResult; stdcall;
    function SetEncodingLevel(
      dwEncLevel: DWORD): HResult; stdcall;
  end;

  PProtocolFilterData = ^TProtocolFilterData;
  {$EXTERNALSYM _tagPROTOCOLFILTERDATA}
  _tagPROTOCOLFILTERDATA = record
    cbSize: DWORD;
    pProtocolSink: IInternetProtocolSink;
    pProtocol: IInternetProtocol;
    pUnk: IUnknown;
    dwFilterFlags: DWORD;
  end;
  {$EXTERNALSYM PROTOCOLFILTERDATA}
  PROTOCOLFILTERDATA = _tagPROTOCOLFILTERDATA;
  TProtocolFilterData = _tagPROTOCOLFILTERDATA;

  PDataInfo = ^TDataInfo;
  {$EXTERNALSYM _tagDATAINFO}
  _tagDATAINFO = record
    ulTotalSize: ULONG;
    ulavrPacketSize: ULONG;
    ulConnectSpeed: ULONG;
    ulProcessorSpeed: ULONG;
  end;
  {$EXTERNALSYM DATAINFO}
  DATAINFO = _tagDATAINFO;
  TDataInfo = _tagDATAINFO;

  {$EXTERNALSYM IEncodingFilterFactory}
  IEncodingFilterFactory = interface(IUnknown)
  ['{70bdde00-c18e-11d0-a9ce-006097942311}']
    function FindBestFilter(pwzCodeIn, pwzCodeOut: PWideChar; info: TDataInfo;
      out ppDF: IDataFilter): HResult; stdcall;
    function GetDefaultFilter(pwzCodeIn, pwzCodeOut: PWideChar;
      out ppDF: IDataFilter): HResult; stdcall;
  end;

  PHitLoggingInfo = ^THitLoggingInfo;
  {$EXTERNALSYM _tagHIT_LOGGING_INFO}
  _tagHIT_LOGGING_INFO = record
    dwStructSize: DWORD;
    lpszLoggedUrlName: PAnsiChar;
    StartTime: TSystemTime;
    EndTime: TSystemTime;
    lpszExtendedInfo: PAnsiChar;
  end;
  {$EXTERNALSYM HIT_LOGGING_INFO}
  HIT_LOGGING_INFO = _tagHIT_LOGGING_INFO;
  THitLoggingInfo = _tagHIT_LOGGING_INFO;

const
  {$EXTERNALSYM CONFIRMSAFETYACTION_LOADOBJECT}
  CONFIRMSAFETYACTION_LOADOBJECT = $00000001;

type
  PConfirmSafety = ^TConfirmSafety;
  {$EXTERNALSYM CONFIRMSAFETY}
  CONFIRMSAFETY = record
    clsid: TCLSID;
    pUnk: IUnknown;
    dwFlags: DWORD;
  end;
  TConfirmSafety = CONFIRMSAFETY;

  {$EXTERNALSYM IWrappedProtocol}
  IWrappedProtocol = interface(IUnknown)
  ['{53c84785-8425-4dc5-971b-e58d9c19f9b6}']
    function GetWrapperCode(out pnCode: Longint;
      dwReserved: DWORD): HResult; stdcall;
  end;

// Functions

{$EXTERNALSYM CreateURLMoniker}
function CreateURLMoniker(pMkCtx: IMoniker; szURL: PWideChar;
  out ppmk: IMoniker): HResult; stdcall;
{$EXTERNALSYM CreateURLMonikerEx}
function CreateURLMonikerEx(pMkCtx: IMoniker; szURL: PWideChar;
  out ppmk: IMoniker; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM GetClassURL}
function GetClassURL(szURL: PWideChar; var pClsID: TCLSID): HResult; stdcall;
{$EXTERNALSYM CreateAsyncBindCtx}
function CreateAsyncBindCtx(reserved: DWORD; pBSCb: IBindStatusCallback;
  pEFetc: IEnumFORMATETC; out ppBC: IBindCtx): HResult; stdcall;
{$EXTERNALSYM CreateAsyncBindCtxEx}
function CreateAsyncBindCtxEx(pbc: IBindCtx; dwOptions: DWORD;
  pBSCb: IBindStatusCallback; pEnum: IEnumFORMATETC;
  out ppBC: IBindCtx; reserved: DWORD): HResult; stdcall;
{$EXTERNALSYM MkParseDisplayNameEx}
function MkParseDisplayNameEx(pbc: IBindCtx; szDisplayName: PWideChar;
  out pchEaten: ULONG; out ppmk: IMoniker): HResult; stdcall;
{$EXTERNALSYM RegisterBindStatusCallback}
function RegisterBindStatusCallback(pBC: IBindCtx; pBSCb: IBindStatusCallback;
  out ppBSCBPrev: IBindStatusCallback; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM RevokeBindStatusCallback}
function RevokeBindStatusCallback(pBC: IBindCtx;
  pBSCb: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM GetClassFileOrMime}
function GetClassFileOrMime(pBC: IBindCtx; szFilename: PWideChar; pBuffer: Pointer;
  cbSize: DWORD; szMime: PWideChar; dwReserved: DWORD;
  var pclsid: TCLSID): HResult; stdcall;
{$EXTERNALSYM IsValidURL}
function IsValidURL(pBC: IBindCtx; szURL: PWideChar;
  dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoGetClassObjectFromURL}
function CoGetClassObjectFromURL(var rCLASSID: TCLSID;
  szCODE: PWideChar; dwFileVersionMS, dwFileVersionLS: DWORD;
  szTYPE: PWideChar; pBindCtx: IBindCtx; dwClsContext: DWORD;
  pvReserved: Pointer; var riid: TIID;
  var ppv): HResult; stdcall;
{$EXTERNALSYM FaultInIEFeature}
function FaultInIEFeature(hWnd: HWND; var pClassSpec {: uCLSSPEC};
  var pQuery {: QUERYCONTEXT}; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM GetComponentIDFromCLSSPEC}
function GetComponentIDFromCLSSPEC(var pClassspec {: uCLSSPEC};
  var ppszComponentID: PAnsiChar): HResult; stdcall;

//helper apis
{$EXTERNALSYM IsAsyncMoniker}
function IsAsyncMoniker(pmk: IMoniker): HResult; stdcall;

{$EXTERNALSYM CreateURLBinding}
function CreateURLBinding(lpszUrl: PWideChar; pbc: IBindCtx;
    out ppBdg: IBinding): HResult stdcall;

{$EXTERNALSYM RegisterMediaTypes}
function RegisterMediaTypes(ctypes: UINT; const rgszTypes: PPAnsiChar;
  out rgcfTypes: TClipFormat): HResult; stdcall;
{$EXTERNALSYM FindMediaType}
function FindMediaType(rgszTypes: PAnsiChar;
  out rgcfTypes: TClipFormat): HResult; stdcall;
{$EXTERNALSYM CreateFormatEnumerator}
function CreateFormatEnumerator(cfmtetc: UINT; rgfmtetc: PFORMATETC;
  out ppenumfmtetc: IEnumFORMATETC): HResult; stdcall;
{$EXTERNALSYM RegisterFormatEnumerator}
function RegisterFormatEnumerator(pBC: IBindCtx; pEFetc: IEnumFORMATETC;
  reserved: DWORD): HResult; stdcall;
{$EXTERNALSYM RevokeFormatEnumerator}
function RevokeFormatEnumerator(pBC: IBindCtx;
  pEFetc: IEnumFORMATETC): HResult; stdcall;
{$EXTERNALSYM RegisterMediaTypeClass}
function RegisterMediaTypeClass(pBC: IBindCtx; ctypes: UINT;
  const rgszTypes: PPAnsiChar; rgclsID: PCLSID;
  reserved: DWORD): HResult; stdcall;
{$EXTERNALSYM FindMediaTypeClass}
function FindMediaTypeClass(pBC: IBindCtx; szType: PAnsiChar;
  out pclsID: TCLSID; reserved: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlMkSetSessionOption}
function UrlMkSetSessionOption(dwOption: DWORD; pBuffer: Pointer;
  dwBufferLength, dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlMkGetSessionOption}
function UrlMkGetSessionOption(dwOption: DWORD; pBuffer: Pointer;
  dwBufferLength: DWORD; out pdwBufferLength: DWORD;
  dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM FindMimeFromData}
function FindMimeFromData(
                        pBC: IBindCtx;                // bind context - can be NULL
                        pwzUrl: PWideChar;            // url - can be null
                        pBuffer: Pointer;             // buffer with data to sniff - can be null (pwzUrl must be valid)
                        cbSize: DWORD;                // size of buffer
                        pwzMimeProposed: PWideChar;   // proposed mime if - can be null
                        dwMimeFlags: DWORD;           // will be defined
                        out ppwzMimeOut: PWideChar;   // the suggested mime
                        dwReserved: DWORD             // must be 0
  ): HResult; stdcall;

{$EXTERNALSYM ObtainUserAgentString}
function ObtainUserAgentString(dwOption: DWORD; pszUAOut: PAnsiChar;
  out cbSize: DWORD): HResult; stdcall;
{$EXTERNALSYM CompareSecurityIds}
function CompareSecurityIds(pbSecurityId1: PByte; dwLen1: DWORD;
  pbSecurityId2: PByte; dwLen2, dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CompatFlagsFromClsid}
function CompatFlagsFromClsid(var pclsid: TCLSID;
  out pdwCompatFlags, pdwMiscStatusFlags: DWORD): HResult; stdcall;

{$EXTERNALSYM URLOpenStreamA}
function URLOpenStreamA(pCaller: IUnknown; szURL: PAnsiChar; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenStreamW}
function URLOpenStreamW(pCaller: IUnknown; szURL: PWideChar; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenStream}
function URLOpenStream(pCaller: IUnknown; szURL: PTSTR; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenPullStreamA}
function URLOpenPullStreamA(pCaller: IUnknown; szURL: PAnsiChar;
  dwReserved: DWORD; lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenPullStreamW}
function URLOpenPullStreamW(pCaller: IUnknown; szURL: PWideChar;
  dwReserved: DWORD; lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenPullStream}
function URLOpenPullStream(pCaller: IUnknown; szURL: PTSTR; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToFileA}
function URLDownloadToFileA(pCaller: IUnknown; szURL, szFileName: PAnsiChar;
  dwReserved: DWORD; lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToFileW}
function URLDownloadToFileW(pCaller: IUnknown; szURL, szFileName: PWideChar;
  dwReserved: DWORD; lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToFile}
function URLDownloadToFile(pCaller: IUnknown; szURL, szFileName: PTSTR;
  dwReserved: DWORD; lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToCacheFileA}
function URLDownloadToCacheFileA(lpUnkCaller: IUnknown;
  szURL, szFileName: PAnsiChar; dwBufLength, dwReserved: DWORD;
  pBSC: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToCacheFileW}
function URLDownloadToCacheFileW(lpUnkCaller: IUnknown;
  szURL, szFileName: PWideChar; dwBufLength, dwReserved: DWORD;
  pBSC: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLDownloadToCacheFile}
function URLDownloadToCacheFile(lpUnkCaller: IUnknown;
  szURL, szFileName: PTSTR; dwBufLength, dwReserved: DWORD;
  pBSC: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenBlockingStreamA}
function URLOpenBlockingStreamA(pCaller: IUnknown; szURL: PAnsiChar;
  out ppStream: IStream; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenBlockingStreamW}
function URLOpenBlockingStreamW(pCaller: IUnknown; szURL: PWideChar;
  out ppStream: IStream; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;
{$EXTERNALSYM URLOpenBlockingStream}
function URLOpenBlockingStream(pCaller: IUnknown; szURL: PTSTR;
  out ppStream: IStream; dwReserved: DWORD;
  lpfnCB: IBindStatusCallback): HResult; stdcall;

{$EXTERNALSYM HlinkGoBack}
function HlinkGoBack(pUnk: IUnknown): HResult; stdcall;
{$EXTERNALSYM HlinkGoForward}
function HlinkGoForward(pUnk: IUnknown): HResult; stdcall;
{$EXTERNALSYM HlinkNavigateString}
function HlinkNavigateString(pUnk: IUnknown;
  szTarget: PWideChar): HResult; stdcall;
{$EXTERNALSYM HlinkNavigateMoniker}
function HlinkNavigateMoniker(pUnk: IUnknown;
  pmkTarget: IMoniker): HResult; stdcall;

{$EXTERNALSYM HlinkSimpleNavigateToString}
function HlinkSimpleNavigateToString(
  szTarget,                     // required - target document - null if local jump w/in doc
  szLocation,                   // optional, for navigation into middle of a doc
  szTargetFrameName: PWideChar; // optional, for targeting frame-sets
  pUnk: IUnknown;               // required - we'll search this for other necessary interfaces
  pbc: IBindCtx;                // optional. caller may register an IBSC in this
  pBSC: IBindStatusCallback;
  grfHLNF,                      // flags
  dwReserved: DWORD             // for future use, must be NULL
): HResult; stdcall;

{$EXTERNALSYM HlinkSimpleNavigateToMoniker}
function HlinkSimpleNavigateToMoniker(
  pmkTarget: IMoniker;          // required - target document - (may be null
  szLocation,                   // optional, for navigation into middle of a doc
  szTargetFrameName: PWideChar; // optional, for targeting frame-sets
  pUnk: IUnknown;               // required - we'll search this for other necessary interfaces
  pbc: IBindCtx;                // optional. caller may register an IBSC in this
  pBSC: IBindStatusCallback;
  grfHLNF,                      // flags
  dwReserved: DWORD             // for future use, must be NULL
): HResult; stdcall;

{$EXTERNALSYM CoInternetParseUrl}
function CoInternetParseUrl(pwzUrl: PWideChar; ParseAction: TParseAction;
  dwFlags: DWORD; pszResult: PWideChar; ccHResult: DWORD;
  out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM OInetParseUrl}
function OInetParseUrl(pwzUrl: PWideChar; ParseAction: TParseAction;
  dwFlags: DWORD; pszResult: PWideChar; ccHResult: DWORD;
  out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetCombineUrl}
function CoInternetCombineUrl(pwzBaseUrl: PWideChar; pwzRelativeUrl: PWideChar;
  dwCombineFlags: DWORD; pszResult: PWideChar; ccHResult: DWORD;
  out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM OInetCombineUrl}
function OInetCombineUrl(pwzBaseUrl: PWideChar; pwzRelativeUrl: PWideChar;
  dwCombineFlags: DWORD; pszResult: PWideChar; ccHResult: DWORD;
  out pccHResult: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetCompareUrl}
function CoInternetCompareUrl(pwzUrl1, pwzUrl2: PWideChar;
  dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM OInetCompareUrl}
function OInetCompareUrl(pwzUrl1, pwzUrl2: PWideChar;
  dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetGetProtocolFlags}
function CoInternetGetProtocolFlags(pwzUrl: PWideChar; var pdwFlags: DWORD;
  dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetQueryInfo}
function CoInternetQueryInfo(pwzUrl: PWideChar; QueryOption: TQueryOption;
  dwQueryFlags: DWORD; pvBuffer: Pointer; cbBuffer: DWORD;
  var pcbBuffer: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM OInetQueryInfo}
function OInetQueryInfo(pwzUrl: PWideChar; QueryOption: TQueryOption;
  dwQueryFlags: DWORD; pvBuffer: Pointer; cbBuffer: DWORD;
  var pcbBuffer: DWORD; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetGetSession}
function CoInternetGetSession(dwSessionMode: DWORD;
  out ppIInternetSession: IInternetSession;
  dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM OInetGetSession}
function OInetGetSession(dwSessionMode: DWORD;
  out ppIInternetSession: IInternetSession;
  dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetGetSecurityUrl}
function CoInternetGetSecurityUrl( pwzUrl: PWideChar; var ppwzSecUrl: PWideChar;
  psuAction: TPSUAction; dwReserved: DWORD): HResult; stdcall;
{$EXTERNALSYM AsyncInstallDistributionUnit}
function AsyncInstallDistributionUnit(szDistUnit, szTYPE, szExt: PWideChar;
  dwFileVersionMS, dwFileVersionLS: DWORD; szURL: PWideChar; pbc: IBindCtx;
  pvReserved: Pointer; flags: DWORD): HResult; stdcall;

{$EXTERNALSYM CoInternetSetFeatureEnabled}
function CoInternetSetFeatureEnabled(FeatureEntry: TInternetFeatureList;
  dwFlags: DWORD; fEnable: BOOL): HResult; stdcall;
{$EXTERNALSYM CoInternetIsFeatureEnabled}
function CoInternetIsFeatureEnabled(FeatureEntry: TInternetFeatureList;
  dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM CoInternetIsFeatureEnabledForUrl}
function CoInternetIsFeatureEnabledForUrl(FeatureEntry: TInternetFeatureList;
  dwFlags: DWORD; szURL: PWideChar;
  pSecMgr: IInternetSecurityManager): HResult; stdcall;
{$EXTERNALSYM CoInternetIsFeatureZoneElevationEnabled}
function CoInternetIsFeatureZoneElevationEnabled(szFromURL, szToURL: PWideChar;
  pSecMgr: IInternetSecurityManager; dwFlags: DWORD): HResult; stdcall;

{$EXTERNALSYM CopyStgMedium}
function CopyStgMedium(const pcstgmedSrc: TStgMedium;
  out pstgmedDest: TStgMedium): HResult; stdcall;
{$EXTERNALSYM CopyBindInfo}
function CopyBindInfo(const pcbiSrc: TBindInfo;
  out pbiDest: TBindInfo ): HResult; stdcall;
{$EXTERNALSYM ReleaseBindInfo}
procedure ReleaseBindInfo(const pbindinfo: TBindInfo); stdcall;

// Creates the security manager object. The first argument is the Service provider
// to allow for delegation
{$EXTERNALSYM CoInternetCreateSecurityManager}
function CoInternetCreateSecurityManager(pSP: IServiceProvider;
  out ppSM: IInternetSecurityManager; dwReserved: DWORD): HResult; stdcall;

{$EXTERNALSYM CoInternetCreateZoneManager}
function CoInternetCreateZoneManager(pSP: IServiceProvider;
  out ppZM: IInternetZoneManager; dwReserved: DWORD): HResult; stdcall;

// Logging-specific apis
{$EXTERNALSYM IsLoggingEnabledA}
function IsLoggingEnabledA(pszUrl: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM IsLoggingEnabledW}
function IsLoggingEnabledW(pwszUrl: PWideChar): BOOL; stdcall;
{$EXTERNALSYM IsLoggingEnabled}
function IsLoggingEnabled(pszUrl: PTSTR): BOOL; stdcall;

{$EXTERNALSYM WriteHitLogging}
function WriteHitLogging(const lpLogginginfo: THitLoggingInfo): BOOL; stdcall;

{$EXTERNALSYM GetSoftwareUpdateInfo}
function GetSoftwareUpdateInfo(szDistUnit: PWideChar;
  var psdi: TSoftDistInfo): HResult; stdcall;
{$EXTERNALSYM SetSoftwareUpdateAdvertisementState}
function SetSoftwareUpdateAdvertisementState(szDistUnit: PWideChar;
  dwAdState, dwAdvertisedVersionMS,
  dwAdvertisedVersionLS: DWORD): HResult; stdcall;

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}


{$IFNDEF JWA_INTERFACESECTION}

// Macro functions

function GetUrlPolicyPermissions(dw: DWORD): DWORD;
begin
  Result := dw and URLPOLICY_MASK_PERMISSIONS;
end;

procedure SetUrlPolicyPermissions(var dw: DWORD; dw2: DWORD);
begin
  dw := (dw and not URLPOLICY_MASK_PERMISSIONS) or dw2;
end;

{$IFNDEF DYNAMIC_LINK}
function CreateURLMoniker; external UrlMonDll name 'CreateURLMoniker';
function CreateURLMonikerEx; external UrlMonDll name 'CreateURLMonikerEx';
function GetClassURL; external UrlMonDll name 'GetClassURL';
function CreateAsyncBindCtx; external UrlMonDll name 'CreateAsyncBindCtx';
function CreateAsyncBindCtxEx; external UrlMonDll name 'CreateAsyncBindCtxEx';
function MkParseDisplayNameEx; external UrlMonDll name 'MkParseDisplayNameEx';
function RegisterBindStatusCallback; external UrlMonDll name 'RegisterBindStatusCallback';
function RevokeBindStatusCallback; external UrlMonDll name 'RevokeBindStatusCallback';
function GetClassFileOrMime; external UrlMonDll name 'GetClassFileOrMime';
function IsValidURL; external UrlMonDll name 'IsValidURL';
function CoGetClassObjectFromURL; external UrlMonDll name 'CoGetClassObjectFromURL';
function FaultInIEFeature; external UrlMonDll name 'FaultInIEFeature';
function GetComponentIDFromCLSSPEC; external UrlMonDll name 'GetComponentIDFromCLSSPEC';
function IsAsyncMoniker; external UrlMonDll name 'IsAsyncMoniker';
function RegisterMediaTypes; external UrlMonDll name 'RegisterMediaTypes';
function FindMediaType; external UrlMonDll name 'FindMediaType';
function CreateFormatEnumerator; external UrlMonDll name 'CreateFormatEnumerator';
function RegisterFormatEnumerator; external UrlMonDll name 'RegisterFormatEnumerator';
function RevokeFormatEnumerator; external UrlMonDll name 'RevokeFormatEnumerator';
function RegisterMediaTypeClass; external UrlMonDll name 'RegisterMediaTypeClass';
function FindMediaTypeClass; external UrlMonDll name 'FindMediaTypeClass';
function UrlMkSetSessionOption; external UrlMonDll name 'UrlMkSetSessionOption';
function UrlMkGetSessionOption; external UrlMonDll name 'UrlMkGetSessionOption';
function FindMimeFromData; external UrlMonDll name 'FindMimeFromData';
function ObtainUserAgentString; external UrlMonDll name 'ObtainUserAgentString';
function CompareSecurityIds; external UrlMonDll name 'CompareSecurityIds';
function CompatFlagsFromClsid; external UrlMonDll name 'CompatFlagsFromClsid';
function URLOpenStreamA; external UrlMonDll name 'URLOpenStreamA';
function URLOpenStreamW; external UrlMonDll name 'URLOpenStreamW';
function URLOpenStream; external UrlMonDll name 'URLOpenStream'+AWSuffix;
function URLOpenPullStreamA; external UrlMonDll name 'URLOpenPullStreamA';
function URLOpenPullStreamW; external UrlMonDll name 'URLOpenPullStreamW';
function URLOpenPullStream; external UrlMonDll name 'URLOpenPullStream'+AWSuffix;
function URLDownloadToFileA; external UrlMonDll name 'URLDownloadToFileA';
function URLDownloadToFileW; external UrlMonDll name 'URLDownloadToFileW';
function URLDownloadToFile; external UrlMonDll name 'URLDownloadToFile'+AWSuffix;
function URLDownloadToCacheFileA; external UrlMonDll name 'URLDownloadToCacheFileA';
function URLDownloadToCacheFileW; external UrlMonDll name 'URLDownloadToCacheFileW';
function URLDownloadToCacheFile; external UrlMonDll name 'URLDownloadToCacheFile'+AWSuffix;
function URLOpenBlockingStreamA; external UrlMonDll name 'URLOpenBlockingStreamA';
function URLOpenBlockingStreamW; external UrlMonDll name 'URLOpenBlockingStreamW';
function URLOpenBlockingStream; external UrlMonDll name 'URLOpenBlockingStream'+AWSuffix;
function HlinkGoBack; external UrlMonDll name 'HlinkGoBack';
function HlinkGoForward; external UrlMonDll name 'HlinkGoForward';
function HlinkNavigateString; external UrlMonDll name 'HlinkNavigateString';
function HlinkNavigateMoniker; external UrlMonDll name 'HlinkNavigateMoniker';
function HlinkSimpleNavigateToString; external UrlMonDll name 'HlinkSimpleNavigateToString';
function HlinkSimpleNavigateToMoniker; external UrlMonDll name 'HlinkSimpleNavigateToMoniker';
function CoInternetParseUrl; external UrlMonDll name 'CoInternetParseUrl';
function CoInternetCombineUrl; external UrlMonDll name 'CoInternetCombineUrl';
function CoInternetCompareUrl; external UrlMonDll name 'CoInternetCompareUrl';
function CoInternetGetProtocolFlags; external UrlMonDll name 'CoInternetGetProtocolFlags';
function CoInternetQueryInfo; external UrlMonDll name 'CoInternetQueryInfo';
function CoInternetGetSession; external UrlMonDll name 'CoInternetGetSession';
function CoInternetGetSecurityUrl; external UrlMonDll name 'CoInternetGetSecurityUrl';
function AsyncInstallDistributionUnit; external UrlMonDll name 'AsyncInstallDistributionUnit';
function CoInternetSetFeatureEnabled; external UrlMonDll name 'CoInternetSetFeatureEnabled';
function CoInternetIsFeatureEnabled; external UrlMonDll name 'CoInternetIsFeatureEnabled';
function CoInternetIsFeatureEnabledForUrl; external UrlMonDll name 'CoInternetIsFeatureEnabledForUrl';
function CoInternetIsFeatureZoneElevationEnabled; external UrlMonDll name 'CoInternetIsFeatureZoneElevationEnabled';
function CopyStgMedium; external UrlMonDll name 'CopyStgMedium';
function CopyBindInfo; external UrlMonDll name 'CopyBindInfo';
procedure ReleaseBindInfo; external UrlMonDll name 'ReleaseBindInfo';
function CoInternetCreateSecurityManager; external UrlMonDll name 'CoInternetCreateSecurityManager';
function CoInternetCreateZoneManager; external UrlMonDll name 'CoInternetCreateZoneManager';
function OInetParseUrl; external UrlMonDll name 'CoInternetParseUrl';
function OInetCombineUrl; external UrlMonDll name 'CoInternetCombineUrl';
function OInetCompareUrl; external UrlMonDll name 'CoInternetCompareUrl';
function OInetQueryInfo; external UrlMonDll name 'CoInternetQueryInfo';
function OInetGetSession; external UrlMonDll name 'CoInternetGetSession';
function IsLoggingEnabledA; external UrlMonDll name 'IsLoggingEnabledA';
function IsLoggingEnabledW; external UrlMonDll name 'IsLoggingEnabledW';
function IsLoggingEnabled; external UrlMonDll name 'IsLoggingEnabled'+AWSuffix;
function WriteHitLogging; external UrlMonDll name 'WriteHitLogging';
function GetSoftwareUpdateInfo; external UrlMonDll name 'GetSoftwareUpdateInfo';
function SetSoftwareUpdateAdvertisementState; external UrlMonDll name 'SetSoftwareUpdateAdvertisementState';
function CreateURLBinding ; external UrlMonDll name 'CreateURLBinding';
{$ELSE}

var
  _CreateURLMoniker: Pointer;

function CreateURLMoniker;
begin
  GetProcedureAddress(_CreateURLMoniker, UrlMonDll, 'CreateURLMoniker');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateURLMoniker]
  end;
end;

var
  _CreateURLMonikerEx: Pointer;

function CreateURLMonikerEx;
begin
  GetProcedureAddress(_CreateURLMonikerEx, UrlMonDll, 'CreateURLMonikerEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateURLMonikerEx]
  end;
end;

var
  _GetClassURL: Pointer;

function GetClassURL;
begin
  GetProcedureAddress(_GetClassURL, UrlMonDll, 'GetClassURL');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetClassURL]
  end;
end;

var
  _CreateAsyncBindCtx: Pointer;

function CreateAsyncBindCtx;
begin
  GetProcedureAddress(_CreateAsyncBindCtx, UrlMonDll, 'CreateAsyncBindCtx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateAsyncBindCtx]
  end;
end;

var
  _CreateAsyncBindCtxEx: Pointer;

function CreateAsyncBindCtxEx;
begin
  GetProcedureAddress(_CreateAsyncBindCtxEx, UrlMonDll, 'CreateAsyncBindCtxEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateAsyncBindCtxEx]
  end;
end;

var
  _MkParseDisplayNameEx: Pointer;

function MkParseDisplayNameEx;
begin
  GetProcedureAddress(_MkParseDisplayNameEx, UrlMonDll, 'MkParseDisplayNameEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MkParseDisplayNameEx]
  end;
end;

var
  _RegisterBindStatusCallback: Pointer;

function RegisterBindStatusCallback;
begin
  GetProcedureAddress(_RegisterBindStatusCallback, UrlMonDll, 'RegisterBindStatusCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterBindStatusCallback]
  end;
end;

var
  _RevokeBindStatusCallback: Pointer;

function RevokeBindStatusCallback;
begin
  GetProcedureAddress(_RevokeBindStatusCallback, UrlMonDll, 'RevokeBindStatusCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RevokeBindStatusCallback]
  end;
end;

var
  _GetClassFileOrMime: Pointer;

function GetClassFileOrMime;
begin
  GetProcedureAddress(_GetClassFileOrMime, UrlMonDll, 'GetClassFileOrMime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetClassFileOrMime]
  end;
end;

var
  _IsValidURL: Pointer;

function IsValidURL;
begin
  GetProcedureAddress(_IsValidURL, UrlMonDll, 'IsValidURL');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsValidURL]
  end;
end;

var
  _CoGetClassObjectFromURL: Pointer;

function CoGetClassObjectFromURL;
begin
  GetProcedureAddress(_CoGetClassObjectFromURL, UrlMonDll, 'CoGetClassObjectFromURL');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoGetClassObjectFromURL]
  end;
end;

var
  _FaultInIEFeature: Pointer;

function FaultInIEFeature;
begin
  GetProcedureAddress(_FaultInIEFeature, UrlMonDll, 'FaultInIEFeature');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FaultInIEFeature]
  end;
end;

var
  _GetComponentIDFromCLSSPEC: Pointer;

function GetComponentIDFromCLSSPEC;
begin
  GetProcedureAddress(_GetComponentIDFromCLSSPEC, UrlMonDll, 'GetComponentIDFromCLSSPEC');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetComponentIDFromCLSSPEC]
  end;
end;

var
  _IsAsyncMoniker: Pointer;

function IsAsyncMoniker;
begin
  GetProcedureAddress(_IsAsyncMoniker, UrlMonDll, 'IsAsyncMoniker');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsAsyncMoniker]
  end;
end;

var
  _RegisterMediaTypes: Pointer;

function RegisterMediaTypes;
begin
  GetProcedureAddress(_RegisterMediaTypes, UrlMonDll, 'RegisterMediaTypes');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterMediaTypes]
  end;
end;

var
  _FindMediaType: Pointer;

function FindMediaType;
begin
  GetProcedureAddress(_FindMediaType, UrlMonDll, 'FindMediaType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindMediaType]
  end;
end;

var
  _CreateFormatEnumerator: Pointer;

function CreateFormatEnumerator;
begin
  GetProcedureAddress(_CreateFormatEnumerator, UrlMonDll, 'CreateFormatEnumerator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateFormatEnumerator]
  end;
end;

var
  _RegisterFormatEnumerator: Pointer;

function RegisterFormatEnumerator;
begin
  GetProcedureAddress(_RegisterFormatEnumerator, UrlMonDll, 'RegisterFormatEnumerator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterFormatEnumerator]
  end;
end;

var
  _RevokeFormatEnumerator: Pointer;

function RevokeFormatEnumerator;
begin
  GetProcedureAddress(_RevokeFormatEnumerator, UrlMonDll, 'RevokeFormatEnumerator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RevokeFormatEnumerator]
  end;
end;

var
  _RegisterMediaTypeClass: Pointer;

function RegisterMediaTypeClass;
begin
  GetProcedureAddress(_RegisterMediaTypeClass, UrlMonDll, 'RegisterMediaTypeClass');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterMediaTypeClass]
  end;
end;

var
  _FindMediaTypeClass: Pointer;

function FindMediaTypeClass;
begin
  GetProcedureAddress(_FindMediaTypeClass, UrlMonDll, 'FindMediaTypeClass');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindMediaTypeClass]
  end;
end;

var
  _UrlMkSetSessionOption: Pointer;

function UrlMkSetSessionOption;
begin
  GetProcedureAddress(_UrlMkSetSessionOption, UrlMonDll, 'UrlMkSetSessionOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlMkSetSessionOption]
  end;
end;

var
  _UrlMkGetSessionOption: Pointer;

function UrlMkGetSessionOption;
begin
  GetProcedureAddress(_UrlMkGetSessionOption, UrlMonDll, 'UrlMkGetSessionOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlMkGetSessionOption]
  end;
end;

var
  _FindMimeFromData: Pointer;

function FindMimeFromData;
begin
  GetProcedureAddress(_FindMimeFromData, UrlMonDll, 'FindMimeFromData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindMimeFromData]
  end;
end;

var
  _ObtainUserAgentString: Pointer;

function ObtainUserAgentString;
begin
  GetProcedureAddress(_ObtainUserAgentString, UrlMonDll, 'ObtainUserAgentString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ObtainUserAgentString]
  end;
end;

var
  _CompareSecurityIds: Pointer;

function CompareSecurityIds;
begin
  GetProcedureAddress(_CompareSecurityIds, UrlMonDll, 'CompareSecurityIds');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CompareSecurityIds]
  end;
end;

var
  _CompatFlagsFromClsid: Pointer;

function CompatFlagsFromClsid;
begin
  GetProcedureAddress(_CompatFlagsFromClsid, UrlMonDll, 'CompatFlagsFromClsid');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CompatFlagsFromClsid]
  end;
end;

var
  _URLOpenStreamA: Pointer;

function URLOpenStreamA;
begin
  GetProcedureAddress(_URLOpenStreamA, UrlMonDll, 'URLOpenStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenStreamA]
  end;
end;

var
  _URLOpenStreamW: Pointer;

function URLOpenStreamW;
begin
  GetProcedureAddress(_URLOpenStreamW, UrlMonDll, 'URLOpenStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenStreamW]
  end;
end;

var
  _URLOpenStream: Pointer;

function URLOpenStream;
begin
  GetProcedureAddress(_URLOpenStream, UrlMonDll, 'URLOpenStream'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenStream]
  end;
end;

var
  _URLOpenPullStreamA: Pointer;

function URLOpenPullStreamA;
begin
  GetProcedureAddress(_URLOpenPullStreamA, UrlMonDll, 'URLOpenPullStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenPullStreamA]
  end;
end;

var
  _URLOpenPullStreamW: Pointer;

function URLOpenPullStreamW;
begin
  GetProcedureAddress(_URLOpenPullStreamW, UrlMonDll, 'URLOpenPullStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenPullStreamW]
  end;
end;

var
  _URLOpenPullStream: Pointer;

function URLOpenPullStream;
begin
  GetProcedureAddress(_URLOpenPullStream, UrlMonDll, 'URLOpenPullStream'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenPullStream]
  end;
end;

var
  _URLDownloadToFileA: Pointer;

function URLDownloadToFileA;
begin
  GetProcedureAddress(_URLDownloadToFileA, UrlMonDll, 'URLDownloadToFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToFileA]
  end;
end;

var
  _URLDownloadToFileW: Pointer;

function URLDownloadToFileW;
begin
  GetProcedureAddress(_URLDownloadToFileW, UrlMonDll, 'URLDownloadToFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToFileW]
  end;
end;

var
  _URLDownloadToFile: Pointer;

function URLDownloadToFile;
begin
  GetProcedureAddress(_URLDownloadToFile, UrlMonDll, 'URLDownloadToFile'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToFile]
  end;
end;

var
  _URLDownloadToCacheFileA: Pointer;

function URLDownloadToCacheFileA;
begin
  GetProcedureAddress(_URLDownloadToCacheFileA, UrlMonDll, 'URLDownloadToCacheFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToCacheFileA]
  end;
end;

var
  _URLDownloadToCacheFileW: Pointer;

function URLDownloadToCacheFileW;
begin
  GetProcedureAddress(_URLDownloadToCacheFileW, UrlMonDll, 'URLDownloadToCacheFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToCacheFileW]
  end;
end;

var
  _URLDownloadToCacheFile: Pointer;

function URLDownloadToCacheFile;
begin
  GetProcedureAddress(_URLDownloadToCacheFile, UrlMonDll, 'URLDownloadToCacheFile'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLDownloadToCacheFile]
  end;
end;

var
  _URLOpenBlockingStreamA: Pointer;

function URLOpenBlockingStreamA;
begin
  GetProcedureAddress(_URLOpenBlockingStreamA, UrlMonDll, 'URLOpenBlockingStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenBlockingStreamA]
  end;
end;

var
  _URLOpenBlockingStreamW: Pointer;

function URLOpenBlockingStreamW;
begin
  GetProcedureAddress(_URLOpenBlockingStreamW, UrlMonDll, 'URLOpenBlockingStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenBlockingStreamW]
  end;
end;

var
  _URLOpenBlockingStream: Pointer;

function URLOpenBlockingStream;
begin
  GetProcedureAddress(_URLOpenBlockingStream, UrlMonDll, 'URLOpenBlockingStream'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_URLOpenBlockingStream]
  end;
end;

var
  _HlinkGoBack: Pointer;

function HlinkGoBack;
begin
  GetProcedureAddress(_HlinkGoBack, UrlMonDll, 'HlinkGoBack');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkGoBack]
  end;
end;

var
  _HlinkGoForward: Pointer;

function HlinkGoForward;
begin
  GetProcedureAddress(_HlinkGoForward, UrlMonDll, 'HlinkGoForward');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkGoForward]
  end;
end;

var
  _HlinkNavigateString: Pointer;

function HlinkNavigateString;
begin
  GetProcedureAddress(_HlinkNavigateString, UrlMonDll, 'HlinkNavigateString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkNavigateString]
  end;
end;

var
  _HlinkNavigateMoniker: Pointer;

function HlinkNavigateMoniker;
begin
  GetProcedureAddress(_HlinkNavigateMoniker, UrlMonDll, 'HlinkNavigateMoniker');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkNavigateMoniker]
  end;
end;

var
  _HlinkSimpleNavigateToString: Pointer;

function HlinkSimpleNavigateToString;
begin
  GetProcedureAddress(_HlinkSimpleNavigateToString, UrlMonDll, 'HlinkSimpleNavigateToString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkSimpleNavigateToString]
  end;
end;

var
  _HlinkSimpleNavigateToMoniker: Pointer;

function HlinkSimpleNavigateToMoniker;
begin
  GetProcedureAddress(_HlinkSimpleNavigateToMoniker, UrlMonDll, 'HlinkSimpleNavigateToMoniker');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HlinkSimpleNavigateToMoniker]
  end;
end;

var
  _CoInternetParseUrl: Pointer;

function CoInternetParseUrl;
begin
  GetProcedureAddress(_CoInternetParseUrl, UrlMonDll, 'CoInternetParseUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetParseUrl]
  end;
end;

var
  _CoInternetCombineUrl: Pointer;

function CoInternetCombineUrl;
begin
  GetProcedureAddress(_CoInternetCombineUrl, UrlMonDll, 'CoInternetCombineUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetCombineUrl]
  end;
end;

var
  _CoInternetCompareUrl: Pointer;

function CoInternetCompareUrl;
begin
  GetProcedureAddress(_CoInternetCompareUrl, UrlMonDll, 'CoInternetCompareUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetCompareUrl]
  end;
end;

var
  _CoInternetGetProtocolFlags: Pointer;

function CoInternetGetProtocolFlags;
begin
  GetProcedureAddress(_CoInternetGetProtocolFlags, UrlMonDll, 'CoInternetGetProtocolFlags');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetGetProtocolFlags]
  end;
end;

var
  _CoInternetQueryInfo: Pointer;

function CoInternetQueryInfo;
begin
  GetProcedureAddress(_CoInternetQueryInfo, UrlMonDll, 'CoInternetQueryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetQueryInfo]
  end;
end;

var
  _CoInternetGetSession: Pointer;

function CoInternetGetSession;
begin
  GetProcedureAddress(_CoInternetGetSession, UrlMonDll, 'CoInternetGetSession');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetGetSession]
  end;
end;

var
  _CoInternetGetSecurityUrl: Pointer;

function CoInternetGetSecurityUrl;
begin
  GetProcedureAddress(_CoInternetGetSecurityUrl, UrlMonDll, 'CoInternetGetSecurityUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetGetSecurityUrl]
  end;
end;

var
  _AsyncInstallDistributionUnit: Pointer;

function AsyncInstallDistributionUnit;
begin
  GetProcedureAddress(_AsyncInstallDistributionUnit, UrlMonDll, 'AsyncInstallDistributionUnit');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AsyncInstallDistributionUnit]
  end;
end;

var
  _CoInternetSetFeatureEnabled: Pointer;

function CoInternetSetFeatureEnabled;
begin
  GetProcedureAddress(_CoInternetSetFeatureEnabled, UrlMonDll, 'CoInternetSetFeatureEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetSetFeatureEnabled]
  end;
end;

var
  _CoInternetIsFeatureEnabled: Pointer;

function CoInternetIsFeatureEnabled;
begin
  GetProcedureAddress(_CoInternetIsFeatureEnabled, UrlMonDll, 'CoInternetIsFeatureEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetIsFeatureEnabled]
  end;
end;

var
  //_CoInternetIsFeatureEnabledForUrl: Pointer;
  _CoInternetIsFeatureEFU: Pointer;

function CoInternetIsFeatureEnabledForUrl;
begin
  GetProcedureAddress(_CoInternetIsFeatureEFU, UrlMonDll, 'CoInternetIsFeatureEnabledForUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetIsFeatureEFU]
  end;
end;

var
  //_CoInternetIsFeatureZoneElevationEnabled: Pointer;
  _CoInternetIsFeatureZEE: Pointer;

function CoInternetIsFeatureZoneElevationEnabled;
begin
  GetProcedureAddress(_CoInternetIsFeatureZEE, UrlMonDll, 'CoInternetIsFeatureZoneElevationEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetIsFeatureZEE]
  end;
end;

var
  _CopyStgMedium: Pointer;

function CopyStgMedium;
begin
  GetProcedureAddress(_CopyStgMedium, UrlMonDll, 'CopyStgMedium');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CopyStgMedium]
  end;
end;

var
  _CopyBindInfo: Pointer;

function CopyBindInfo;
begin
  GetProcedureAddress(_CopyBindInfo, UrlMonDll, 'CopyBindInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CopyBindInfo]
  end;
end;

var
  _ReleaseBindInfo: Pointer;

procedure ReleaseBindInfo;
begin
  GetProcedureAddress(_ReleaseBindInfo, UrlMonDll, 'ReleaseBindInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReleaseBindInfo]
  end;
end;

var
  _CoInternetCreateSecurityManager: Pointer;

function CoInternetCreateSecurityManager;
begin
  GetProcedureAddress(_CoInternetCreateSecurityManager, UrlMonDll, 'CoInternetCreateSecurityManager');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetCreateSecurityManager]
  end;
end;

var
  _CoInternetCreateZoneManager: Pointer;

function CoInternetCreateZoneManager;
begin
  GetProcedureAddress(_CoInternetCreateZoneManager, UrlMonDll, 'CoInternetCreateZoneManager');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInternetCreateZoneManager]
  end;
end;

var
  _OInetParseUrl: Pointer;

function OInetParseUrl;
begin
  GetProcedureAddress(_OInetParseUrl, UrlMonDll, 'OInetParseUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OInetParseUrl]
  end;
end;

var
  _OInetCombineUrl: Pointer;

function OInetCombineUrl;
begin
  GetProcedureAddress(_OInetCombineUrl, UrlMonDll, 'OInetCombineUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OInetCombineUrl]
  end;
end;

var
  _OInetCompareUrl: Pointer;

function OInetCompareUrl;
begin
  GetProcedureAddress(_OInetCompareUrl, UrlMonDll, 'OInetCompareUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OInetCompareUrl]
  end;
end;

var
  _OInetQueryInfo: Pointer;

function OInetQueryInfo;
begin
  GetProcedureAddress(_OInetQueryInfo, UrlMonDll, 'OInetQueryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OInetQueryInfo]
  end;
end;

var
  _OInetGetSession: Pointer;

function OInetGetSession;
begin
  GetProcedureAddress(_OInetGetSession, UrlMonDll, 'OInetGetSession');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OInetGetSession]
  end;
end;

var
  _IsLoggingEnabledA: Pointer;

function IsLoggingEnabledA;
begin
  GetProcedureAddress(_IsLoggingEnabledA, UrlMonDll, 'IsLoggingEnabledA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsLoggingEnabledA]
  end;
end;

var
  _IsLoggingEnabledW: Pointer;

function IsLoggingEnabledW;
begin
  GetProcedureAddress(_IsLoggingEnabledW, UrlMonDll, 'IsLoggingEnabledW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsLoggingEnabledW]
  end;
end;

var
  _IsLoggingEnabled: Pointer;

function IsLoggingEnabled;
begin
  GetProcedureAddress(_IsLoggingEnabled, UrlMonDll, 'IsLoggingEnabled'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsLoggingEnabled]
  end;
end;

var
  _WriteHitLogging: Pointer;

function WriteHitLogging;
begin
  GetProcedureAddress(_WriteHitLogging, UrlMonDll, 'WriteHitLogging');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteHitLogging]
  end;
end;

var
  _GetSoftwareUpdateInfo: Pointer;

function GetSoftwareUpdateInfo;
begin
  GetProcedureAddress(_GetSoftwareUpdateInfo, UrlMonDll, 'GetSoftwareUpdateInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetSoftwareUpdateInfo]
  end;
end;

var
  //_SetSoftwareUpdateAdvertisementState: Pointer;
  _SetSoftwareUpdateAS: Pointer;

function SetSoftwareUpdateAdvertisementState;
begin
  GetProcedureAddress(_SetSoftwareUpdateAS, UrlMonDll, 'SetSoftwareUpdateAdvertisementState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetSoftwareUpdateAS]
  end;
end;

var
  _CreateURLBinding: Pointer;

function CreateURLBinding;
begin
  GetProcedureAddress(_CreateURLBinding, UrlMonDll, 'CreateURLBinding');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateURLBinding]
  end;
end;


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}