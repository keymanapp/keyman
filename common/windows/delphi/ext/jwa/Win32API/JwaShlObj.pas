{******************************************************************************}
{                                                                              }
{ Shell Objects Interface Unit for Object Pascal                               }
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
{ The original code is: shlobj.h, shlguid.h, shobjidl.h           			   }
{                       isguid.h, exdisp.h, shtypes.h             			   }
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
unit JwaShlObj;

{$I ..\Includes\JediAPILib.inc}
interface

uses
  {these are necessary for
   DROPEFFECT_COPY and more
   TColorRef
   TCoors
   LF_FACESIZE
   TNetResource

  }
  Windows, ActiveX,
  CommCtrl, {$IFDEF DELPHI6_UP}msxml,{$ENDIF DELPHI6_UP}
  ShDocVw,
  //
  JwaWinBase, JwaWinUser, JwaWinType, JwaActiveX, JwaWinInet,
  JwaShlDisp, JwaShellApi, JwaUrlMon;


 { Windows, Messages, ActiveX, WinInet, CommCtrl, msxml, ShDocVw,
  RVShlDisp, RVShellAPI, RVUrlMon;}


//===========================================================================
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
// File: shlobj.h
//
//===========================================================================

{$WEAKPACKAGEUNIT}
{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

{$HPPEMIT '#include "shlobj.h"'}

{$HPPEMIT 'interface DECLSPEC_UUID("985F64F0-D410-4E02-BE22-DA07F2B5C5E1") IDefViewID;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214EA-0000-0000-C000-000000000046") IPersistFolder;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1AC3D9F0-175C-11d1-95BE-00609797EA4F") IPersistFolder2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1079acfc-29bd-11d3-8e0d-00c04f6837d5") IPersistIDList;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F2-0000-0000-C000-000000000046") IEnumIDList;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E6-0000-0000-C000-000000000046") IShellFolder;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0E700BE1-9DB6-11d1-A1CE-00C04FD75D13") IEnumExtraSearch;'}
{$HPPEMIT 'interface DECLSPEC_UUID("93F2F68C-1D1B-11d3-A30E-00C04F79ABD1") IShellFolder2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E2-0000-0000-C000-000000000046") IShellBrowser;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E3-0000-0000-C000-000000000046") IShellView;'}
{$HPPEMIT 'interface DECLSPEC_UUID("88E39E80-3578-11CF-AE69-08002B2E1262") IShellView2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("cde725b0-ccc9-4519-917e-325d72fab4ce") IFolderView;'}
{$HPPEMIT 'interface DECLSPEC_UUID("C0A651F5-B48B-11d2-B5ED-006097C686F6") IFolderFilterSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E") IFolderFilter;'}
{$HPPEMIT 'interface DECLSPEC_UUID("CB728B20-F786-11CE-92AD-00AA00A74CD0") IProfferService;'}
{$HPPEMIT 'interface DECLSPEC_UUID("757A7D9F-919A-4118-99D7-DBB208C8CC66") IPropertyUI;'}
{$HPPEMIT 'interface DECLSPEC_UUID("9AF64809-5864-4C26-A720-C1F78C086EE3") ICategoryProvider;'}
{$HPPEMIT 'interface DECLSPEC_UUID("A3B14589-9174-49A8-89A3-06A1AE2B9BA7") ICategorizer;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214EE-0000-0000-C000-000000000046") IShellLinkA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F9-0000-0000-C000-000000000046") IShellLinkW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("49FF1172-EADC-446D-9285-156453A6431C") IActionProgressDialog;'}
{$HPPEMIT 'interface DECLSPEC_UUID("C1FB73D0-EC3A-4BA2-B512-8CDB9187B6D1") IHWEventHandler;'}
{$HPPEMIT 'interface DECLSPEC_UUID("DDEFE873-6997-4E68-BE26-39B633ADBE12") IQueryCancelAutoPlay;'}
{$HPPEMIT 'interface DECLSPEC_UUID("49FF1173-EADC-446D-9285-156453A6431C") IActionProgress;'}
{$HPPEMIT 'interface DECLSPEC_UUID("49FF1173-EADC-446D-9285-156453A6431C") IActionProgress;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E8-0000-0000-C000-000000000046") IShellExtInit;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E9-0000-0000-C000-000000000046") IShellPropSheetExt;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214FE-0000-0000-C000-000000000046") IRemoteComputer;'}
{$HPPEMIT 'interface DECLSPEC_UUID("7307055C-B24A-486B-9F25-163E597A28A9") IQueryContinue;'}
{$HPPEMIT 'interface DECLSPEC_UUID("BA9711BA-5893-4787-A7E1-41277151550B") IUserNotification;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1DF0D7F1-B267-4D28-8B10-12E23202A5C4") IItemNameLimits;'}
{$HPPEMIT 'interface DECLSPEC_UUID("49C929EE-A1B7-4C58-B539-E63BE392B6F3") INetCrawler;'}
{$HPPEMIT 'interface DECLSPEC_UUID("BB2E617C-0920-11D1-9A0B-00C04FC2D6C1") IExtractImage;'}
{$HPPEMIT 'interface DECLSPEC_UUID("953BB1EE-93B4-11d1-98A3-00C04FB687DA") IExtractImage2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("e9ead8e6-2a25-410e-9b58-a9fbef1dd1a2") IUserEventTimerCallback;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0F504B94-6E42-42E6-99E0-E20FAFE52AB4") IUserEventTimer;'}
{$HPPEMIT 'interface DECLSPEC_UUID("012dd920-7b26-11d0-8ca9-00a0c92dbfe8") IDockingWindow;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EB0FE172-1A3A-11D0-89B3-00A0C90A90AC") IDeskBand;'}
{$HPPEMIT 'interface DECLSPEC_UUID("56FDF342-FD6D-11d0-958A-006097C9A090") ITaskbarList;'}
{$HPPEMIT 'interface DECLSPEC_UUID("602D4995-B13A-429b-A66E-1935E44F4317") ITaskbarList2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("3d73a659-e5d0-4d42-afc0-5121ba425c8d") ICDBurn;'}
{$HPPEMIT 'interface DECLSPEC_UUID("88960f5b-422f-4e7b-8013-73415381c3c3") IWizardSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("c02ea696-86cc-491e-9b23-74394a0444a8") IWizardExtension;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0e6b3f66-98d1-48c0-a222-fbde74e2fbc5") IWebWizardExtension;'}
{$HPPEMIT 'interface DECLSPEC_UUID("aa9198bb-ccec-472d-beed-19a4f6733f7a") IPublishingWizard;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1ea58f02-d55a-411d-b09e-9e65ac21605b") IFolderViewHost;'}
{$HPPEMIT 'interface DECLSPEC_UUID("3CD141F4-3C6A-11d2-BCAA-00C04FD929DB") IAutoCompleteDropDown;'}
{$HPPEMIT 'interface DECLSPEC_UUID("b4db1657-70d7-485e-8e3e-6fcb5a5c1802") IModalWindow;'}
{$HPPEMIT 'interface DECLSPEC_UUID("a09db586-9180-41ac-9114-460a7f362b76") IPassportWizard;'}
{$HPPEMIT 'interface DECLSPEC_UUID("2271dcca-74fc-4414-8fb7-c56b05ace2d7") ICDBurnExt;'}
{$HPPEMIT 'interface DECLSPEC_UUID("70F55181-5FEA-4900-B6B8-7343CB0A348C") IDVGetEnum;'}
{$HPPEMIT 'interface DECLSPEC_UUID("D2B57227-3D23-4b95-93C0-492BD454C356") IInsertItem;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EB0FE173-1A3A-11D0-89B3-00A0C90A90AC") IDeskBar;'}
{$HPPEMIT 'interface DECLSPEC_UUID("568804CD-CBD7-11d0-9816-00C04FD91972") IMenuBand;'}
{$HPPEMIT 'interface DECLSPEC_UUID("47c01f95-e185-412c-b5c5-4f27df965aea") IFolderBandPriv;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4CF504B0-DE96-11D0-8B3F-00A0C911E8E5") IBandSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("d92995f8-cf5e-4a76-bf59-ead39ea2b97e") INamespaceWalkCB;'}
{$HPPEMIT 'interface DECLSPEC_UUID("57ced8a7-3f4a-432c-9350-30f24483f74f") INamespaceWalk;'}
{$HPPEMIT 'interface DECLSPEC_UUID("A9521922-0812-4d44-9EC3-7FD38C726F3D") IRegTreeItem;'}
{$HPPEMIT 'interface DECLSPEC_UUID("D1E7AFEB-6A2E-11d0-8C78-00C04FD918B4") IMenuPopup;'}
{$HPPEMIT 'interface DECLSPEC_UUID("43826d1e-e718-42ee-bc55-a1e261c37bfe") IShellItem;'}
{$HPPEMIT 'interface DECLSPEC_UUID("505f1513-6b3e-4892-a272-59f8889a4d3e") IImageRecompress;'}
{$HPPEMIT 'interface DECLSPEC_UUID("9A93B3FB-4E75-4c74-871A-2CDA667F39A5") IDefViewSafety;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0811AEBE-0B87-4C54-9E72-548CF649016B") IContextMenuSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("ADD8BA80-002B-11D0-8F0F-00C04FD7D062") IDelegateFolder;'}
{$HPPEMIT 'interface DECLSPEC_UUID("10DF43C8-1DBE-11d3-8B34-006097DF5BD4") IBrowserFrameOptions;'}
{$HPPEMIT 'interface DECLSPEC_UUID("D2BC4C84-3F72-4a52-A604-7BCBF3982CBB") INewWindowManager;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4CA300A1-9B8D-11d1-8B22-00C04FD918D0") IShellMenuCallback;'}
{$HPPEMIT 'interface DECLSPEC_UUID("73db1241-1e85-4581-8e4f-a81e1d0f8c57") IAttachmentExecute;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EE1F7637-E138-11d1-8379-00C04FD918D0") IShellMenu;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E4-0000-0000-C000-000000000046") IContextMenu;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F4-0000-0000-C000-000000000046") IContextMenu2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("BCFCE0A0-EC17-11D0-8D10-00A0C90F2719") IContextMenu3;'}
{$HPPEMIT 'interface DECLSPEC_UUID("CEF04FDF-FE72-11D2-87A5-00C04F6837CF") IPersistFolder3;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214EB-0000-0000-C000-000000000046") IExtractIconA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214FA-0000-0000-C000-000000000046") IExtractIconW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E5-0000-0000-C000-000000000046") IShellIcon;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0C6C4200-C589-11D0-999A-00C04FD655E1") IShellIconOverlayIdentifier;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F10B5E34-DD3B-42A7-AA7D-2F4EC54BB09B") IShellIconOverlayManager;'}
{$HPPEMIT 'interface DECLSPEC_UUID("7D688A70-C613-11D0-999B-00C04FD655E1") IShellIconOverlay;'}
{$HPPEMIT 'interface DECLSPEC_UUID("45E2B4AE-B1C3-11D0-B92F-00A0C90312E1") IShellLinkDataList;'}
{$HPPEMIT 'interface DECLSPEC_UUID("5CD52983-9449-11D2-963A-00C04F79ADF0") IResolveShellLink;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F5-0000-0000-C000-000000000046") IShellExecuteHookA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214FB-0000-0000-C000-000000000046") IShellExecuteHookW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("AC60F6A0-0FD9-11D0-99CB-00C04FD64497") IURLSearchHook;'}
{$HPPEMIT 'interface DECLSPEC_UUID("09F656A2-41AF-480C-88F7-16CC0D164615") ISearchContext;'}
{$HPPEMIT 'interface DECLSPEC_UUID("5EE44DA4-6D32-46E3-86BC-07540DEDD0E0") IURLSearchHook2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214E1-0000-0000-C000-000000000046") INewShortcutHookA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F7-0000-0000-C000-000000000046") INewShortcutHookW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214EF-0000-0000-C000-000000000046") ICopyHookA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214FC-0000-0000-C000-000000000046") ICopyHookW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F3-0000-0000-C000-000000000046") IFileViewerSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F0-0000-0000-C000-000000000046") IFileViewerA;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F8-0000-0000-C000-000000000046") IFileViewerW;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214F1-0000-0000-C000-000000000046") ICommDlgBrowser;'}
{$HPPEMIT 'interface DECLSPEC_UUID("10339516-2894-11D2-9039-00C04F8EEB3E") ICommDlgBrowser2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("01e18d10-4d8b-11d2-855d-006008059367") IFileSystemBindData;'}
{$HPPEMIT 'interface DECLSPEC_UUID("000214EC-0000-0000-C000-000000000046") IShellDetails;'}
{$HPPEMIT 'interface DECLSPEC_UUID("00BB2761-6A77-11D0-A535-00C04FD7D062") IObjMgr;'}
{$HPPEMIT 'interface DECLSPEC_UUID("91956D21-9276-11D1-921A-006097DF5BD4") ICurrentWorkingDirectory;'}
{$HPPEMIT 'interface DECLSPEC_UUID("77A130B0-94FD-11D0-A544-00C04FD7d062") IACList;'}
{$HPPEMIT 'interface DECLSPEC_UUID("470141a0-5186-11d2-bbb6-0060977b464c") IACList2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EBBC7C04-315E-11D2-B62F-006097DF5BD4") IProgressDialog;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F1DB8392-7331-11D0-8C99-00A0C92DBFE8") IInputObjectSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("68284FAA-6A48-11D0-8C78-00C04FD918B4") IInputObject;'}
{$HPPEMIT 'interface DECLSPEC_UUID("2A342FC2-7B26-11D0-8CA9-00A0C92DBFE8") IDockingWindowSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("47D2657A-7B27-11D0-8CA9-00A0C92DBFE8") IDockingWindowFrame;'}
{$HPPEMIT 'interface DECLSPEC_UUID("85788D00-6807-11D0-B810-00C04FD706EC") IRunnableTask;'}
{$HPPEMIT 'interface DECLSPEC_UUID("6CCB7BE0-6807-11D0-B810-00C04FD706EC") IShellTaskScheduler;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4EA39266-7211-409F-B622-F63DBD16C533") IThumbnailCapture;'}
{$HPPEMIT 'interface DECLSPEC_UUID("6DFD582B-92E3-11D1-98A3-00C04FB687DA") IEnumShellImageStore;'}
{$HPPEMIT 'interface DECLSPEC_UUID("48C8118C-B924-11D1-98D5-00C04FB687DA") IShellImageStore;'}
{$HPPEMIT 'interface DECLSPEC_UUID("7FE80CC8-C247-11D0-B93A-00A0C90312E1") IShellFolderBand;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F490EB00-1240-11D1-9888-006097DEACF9") IActiveDesktop;'}
{$HPPEMIT 'interface DECLSPEC_UUID("52502EE0-EC80-11D0-89AB-00C04FC2972D") IActiveDesktopP;'}
{$HPPEMIT 'interface DECLSPEC_UUID("B22754E2-4574-11D1-9888-006097DEACF9") IADesktopP2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("E8025004-1C42-11D2-BE2C-00A0C9A83DA1") IColumnProvider;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4657278b-411b-11d2-839a-00c04fd918d0") IDropTargetHelper;'}
{$HPPEMIT 'interface DECLSPEC_UUID("de5bf786-477a-11d2-839d-00c04fd918d0") IDragSourceHelper;'}
{$HPPEMIT 'interface DECLSPEC_UUID("D82BE2B1-5764-11D0-A96E-00C04FD705A2") IShellChangeNotify;'}
{$HPPEMIT 'interface DECLSPEC_UUID("00021500-0000-0000-C000-000000000046") IQueryInfo;'}
{$HPPEMIT 'interface DECLSPEC_UUID("710EB7A0-45ED-11D0-924A-0020AFC7AC4D") IDefViewFrame;'}
{$HPPEMIT 'interface DECLSPEC_UUID("87D605E0-C511-11CF-89A9-00A0C9054129") IDocViewSite;'}
{$HPPEMIT 'interface DECLSPEC_UUID("596A9A94-013E-11D1-8D34-00A0C90F2719") IBanneredBar;'}
{$HPPEMIT 'interface DECLSPEC_UUID("2047E320-F2A9-11CE-AE65-08002B2E1262") IShellFolderViewCB;'}
{$HPPEMIT 'interface DECLSPEC_UUID("FB700430-952C-11D1-946F-000000000000") INamedPropertyBag;'}

{$HPPEMIT 'typedef System::DelphiInterface<IDefViewID> _di_IDefViewID;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPersistFolder> _di_IPersistFolder;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPersistFolder2> _di_IPersistFolder2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPersistIDList> _di_IPersistIDList;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumIDList> _di_IEnumIDList;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolder> _di_IShellFolder;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumExtraSearch> _di_IEnumExtraSearch;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolder2> _di_IShellFolder2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellBrowser> _di_IShellBrowser;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellView> _di_IShellView;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellView2> _di_IShellView2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFolderView> _di_IFolderView;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFolderFilterSite> _di_IFolderFilterSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFolderFilter> _di_IFolderFilter;'}
{$HPPEMIT 'typedef System::DelphiInterface<IProfferService> _di_IProfferService;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPropertyUI> _di_IPropertyUI;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICategoryProvider> _di_ICategoryProvider;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICategorizer> _di_ICategorizer;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellLinkA> _di_IShellLinkA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellLinkW> _di_IShellLinkW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IActionProgressDialog> _di_IActionProgressDialog;'}
{$HPPEMIT 'typedef System::DelphiInterface<IHWEventHandler> _di_IHWEventHandler;'}
{$HPPEMIT 'typedef System::DelphiInterface<IQueryCancelAutoPlay> _di_IQueryCancelAutoPlay;'}
{$HPPEMIT 'typedef System::DelphiInterface<IActionProgress> _di_IActionProgress;'}
{$HPPEMIT 'typedef System::DelphiInterface<IActionProgress> _di_IActionProgress;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellExtInit> _di_IShellExtInit;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellPropSheetExt> _di_IShellPropSheetExt;'}
{$HPPEMIT 'typedef System::DelphiInterface<IRemoteComputer> _di_IRemoteComputer;'}
{$HPPEMIT 'typedef System::DelphiInterface<IQueryContinue> _di_IQueryContinue;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUserNotification> _di_IUserNotification;'}
{$HPPEMIT 'typedef System::DelphiInterface<IItemNameLimits> _di_IItemNameLimits;'}
{$HPPEMIT 'typedef System::DelphiInterface<INetCrawler> _di_INetCrawler;'}
{$HPPEMIT 'typedef System::DelphiInterface<IExtractImage> _di_IExtractImage;'}
{$HPPEMIT 'typedef System::DelphiInterface<IExtractImage2> _di_IExtractImage2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUserEventTimerCallback> _di_IUserEventTimerCallback;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUserEventTimer> _di_IUserEventTimer;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDockingWindow> _di_IDockingWindow;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDeskBand> _di_IDeskBand;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITaskbarList> _di_ITaskbarList;'}
{$HPPEMIT 'typedef System::DelphiInterface<ITaskbarList2> _di_ITaskbarList2;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICDBurn> _di_ICDBurn;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWizardSite> _di_IWizardSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWizardExtension> _di_IWizardExtension;'}
{$HPPEMIT 'typedef System::DelphiInterface<IWebWizardExtension> _di_IWebWizardExtension;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPublishingWizard> _di_IPublishingWizard;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFolderViewHost> _di_IFolderViewHost;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAutoCompleteDropDown> _di_IAutoCompleteDropDown;'}
{$HPPEMIT 'typedef System::DelphiInterface<IModalWindow> _di_IModalWindow;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPassportWizard> _di_IPassportWizard;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICDBurnExt> _di_ICDBurnExt;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDVGetEnum> _di_IDVGetEnum;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInsertItem> _di_IInsertItem;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDeskBar> _di_IDeskBar;'}
{$HPPEMIT 'typedef System::DelphiInterface<IMenuBand> _di_IMenuBand;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFolderBandPriv> _di_IFolderBandPriv;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBandSite> _di_IBandSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<INamespaceWalkCB> _di_INamespaceWalkCB;'}
{$HPPEMIT 'typedef System::DelphiInterface<INamespaceWalk> _di_INamespaceWalk;'}
{$HPPEMIT 'typedef System::DelphiInterface<IRegTreeItem> _di_IRegTreeItem;'}
{$HPPEMIT 'typedef System::DelphiInterface<IMenuPopup> _di_IMenuPopup;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellItem> _di_IShellItem;'}
{$HPPEMIT 'typedef System::DelphiInterface<IImageRecompress> _di_IImageRecompress;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDefViewSafety> _di_IDefViewSafety;'}
{$HPPEMIT 'typedef System::DelphiInterface<IContextMenuSite> _di_IContextMenuSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDelegateFolder> _di_IDelegateFolder;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBrowserFrameOptions> _di_IBrowserFrameOptions;'}
{$HPPEMIT 'typedef System::DelphiInterface<INewWindowManager> _di_INewWindowManager;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellMenuCallback> _di_IShellMenuCallback;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAttachmentExecute> _di_IAttachmentExecute;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellMenu> _di_IShellMenu;'}
{$HPPEMIT 'typedef System::DelphiInterface<IContextMenu> _di_IContextMenu;'}
{$HPPEMIT 'typedef System::DelphiInterface<IContextMenu2> _di_IContextMenu2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IContextMenu3> _di_IContextMenu3;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPersistFolder3> _di_IPersistFolder3;'}
{$HPPEMIT 'typedef System::DelphiInterface<IExtractIconA> _di_IExtractIconA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IExtractIconW> _di_IExtractIconW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellIcon> _di_IShellIcon;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellIconOverlayIdentifier> _di_IShellIconOverlayIdentifier;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellIconOverlayManager> _di_IShellIconOverlayManager;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellIconOverlay> _di_IShellIconOverlay;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellLinkDataList> _di_IShellLinkDataList;'}
{$HPPEMIT 'typedef System::DelphiInterface<IResolveShellLink> _di_IResolveShellLink;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellExecuteHookA> _di_IShellExecuteHookA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellExecuteHookW> _di_IShellExecuteHookW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IURLSearchHook> _di_IURLSearchHook;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISearchContext> _di_ISearchContext;'}
{$HPPEMIT 'typedef System::DelphiInterface<IURLSearchHook2> _di_IURLSearchHook2;'}
{$HPPEMIT 'typedef System::DelphiInterface<INewShortcutHookA> _di_INewShortcutHookA;'}
{$HPPEMIT 'typedef System::DelphiInterface<INewShortcutHookW> _di_INewShortcutHookW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICopyHookA> _di_ICopyHookA;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICopyHookW> _di_ICopyHookW;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFileViewerSite> _di_IFileViewerSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFileViewerA> _di_IFileViewerA;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFileViewerW> _di_IFileViewerW;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICommDlgBrowser> _di_ICommDlgBrowser;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICommDlgBrowser2> _di_ICommDlgBrowser2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFileSystemBindData> _di_IFileSystemBindData;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellDetails> _di_IShellDetails;'}
{$HPPEMIT 'typedef System::DelphiInterface<IObjMgr> _di_IObjMgr;'}
{$HPPEMIT 'typedef System::DelphiInterface<ICurrentWorkingDirectory> _di_ICurrentWorkingDirectory;'}
{$HPPEMIT 'typedef System::DelphiInterface<IACList> _di_IACList;'}
{$HPPEMIT 'typedef System::DelphiInterface<IACList2> _di_IACList2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IProgressDialog> _di_IProgressDialog;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInputObjectSite> _di_IInputObjectSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInputObject> _di_IInputObject;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDockingWindowSite> _di_IDockingWindowSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDockingWindowFrame> _di_IDockingWindowFrame;'}
{$HPPEMIT 'typedef System::DelphiInterface<IRunnableTask> _di_IRunnableTask;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellTaskScheduler> _di_IShellTaskScheduler;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellTaskScheduler2> _di_IShellTaskScheduler2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IThumbnailCapture> _di_IThumbnailCapture;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumShellImageStore> _di_IEnumShellImageStore;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellImageStore> _di_IShellImageStore;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolderBand> _di_IShellFolderBand;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDeskBarClient> _di_IDeskBarClient;'}
{$HPPEMIT 'typedef System::DelphiInterface<IActiveDesktop> _di_IActiveDesktop;'}
{$HPPEMIT 'typedef System::DelphiInterface<IActiveDesktopP> _di_IActiveDesktopP;'}
{$HPPEMIT 'typedef System::DelphiInterface<IADesktopP2> _di_IADesktopP2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IColumnProvider> _di_IColumnProvider;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDropTargetHelper> _di_IDropTargetHelper;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDragSourceHelper> _di_IDragSourceHelper;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellChangeNotify> _di_IShellChangeNotify;'}
{$HPPEMIT 'typedef System::DelphiInterface<IQueryInfo> _di_IQueryInfo;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDefViewFrame> _di_IDefViewFrame;'}
{$HPPEMIT 'typedef System::DelphiInterface<IDocViewSite> _di_IDocViewSite;'}
{$HPPEMIT 'typedef System::DelphiInterface<IInitializeObject> _di_IInitializeObject;'}
{$HPPEMIT 'typedef System::DelphiInterface<IBanneredBar> _di_IBanneredBar;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolderViewCB> _di_IShellFolderViewCB;'}
{$HPPEMIT 'typedef System::DelphiInterface<INamedPropertyBag> _di_INamedPropertyBag;'}
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//===========================================================================
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
//===========================================================================

// -- shlguid.h --
{$IFNDEF JWA_INCLUDEMODE}
const
  {$EXTERNALSYM CLSID_ShellDesktop}
  CLSID_ShellDesktop: TGUID = (
    D1:$00021400; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CLSID_ShellLink}
  CLSID_ShellLink: TGUID = (
    D1:$00021401; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM CLSID_NetworkPlaces}
  CLSID_NetworkPlaces: TGUID = (
    D1:$208D2C60; D2:$3AEA; D3:$1069; D4:($A2,$D7,$08,$00,$2B,$30,$30,$9D));
  {$EXTERNALSYM CLSID_NetworkDomain}
  CLSID_NetworkDomain: TGUID = (
    D1:$46E06680; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  {$EXTERNALSYM CLSID_NetworkServer}
  CLSID_NetworkServer: TGUID = (
    D1:$C0542A90; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  {$EXTERNALSYM CLSID_NetworkShare}
  CLSID_NetworkShare: TGUID = (
    D1:$54A754C0; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  {$EXTERNALSYM CLSID_MyComputer}
  CLSID_MyComputer: TGUID = (
    D1:$20D04FE0; D2:$3AEA; D3:$1069; D4:($A2,$D8,$08,$00,$2B,$30,$30,$9D));
  {$EXTERNALSYM CLSID_Internet}
  CLSID_Internet: TGUID = (
    D1:$871C5380; D2:$42A0; D3:$1069; D4:($A2,$EA,$08,$00,$2B,$30,$30,$9D));
  {$EXTERNALSYM CLSID_ShellFSFolder}
  CLSID_ShellFSFolder: TGUID = (
    D1:$F3364BA0; D2:$65B9; D3:$11CE; D4:($A9,$BA,$00,$AA,$00,$4A,$E8,$37));
  {$EXTERNALSYM CLSID_RecycleBin}
  CLSID_RecycleBin: TGUID = (
    D1:$645FF040; D2:$5081; D3:$101B; D4:($9F,$08,$00,$AA,$00,$2F,$95,$4E));
  {$EXTERNALSYM CLSID_ControlPanel}
  CLSID_ControlPanel: TGUID = (
    D1:$21EC2020; D2:$3AEA; D3:$1069; D4:($A2,$DD,$08,$00,$2B,$30,$30,$9D));
  {$EXTERNALSYM CLSID_Printers}
  CLSID_Printers: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));
  {$EXTERNALSYM CLSID_MyDocuments}
  CLSID_MyDocuments: TGUID = (
    D1:$450D8FBA; D2:$AD25; D3:$11D0; D4:($98,$A8,$08,$00,$36,$1B,$11,$03));
{$ENDIF JWA_INCLUDEMODE}

// string version of above CLSIDs
// useful for doing psfDesktop->ParseDisplayName(L"::" L STR_MYDOCS_CLSID, ...);
const
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM STR_MYDOCS_CLSID}
  STR_MYDOCS_CLSID = '{450D8FBA-AD25-11D0-98A8-0800361B1103}';

  {$EXTERNALSYM CATID_BrowsableShellExt}
  CATID_BrowsableShellExt: TGUID = (
    D1:$00021490; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CATID_BrowseInPlace}
  CATID_BrowseInPlace: TGUID = (
    D1:$00021491; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CATID_DeskBand}
  CATID_DeskBand: TGUID = (
    D1:$00021492; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CATID_InfoBand}
  CATID_InfoBand: TGUID = (
    D1:$00021493; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CATID_CommBand}
  CATID_CommBand: TGUID = (
    D1:$00021494; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM PSGUID_INTERNETSHORTCUT}
  PSGUID_INTERNETSHORTCUT: TGUID = (
    D1:$000214A0; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM FMTID_Intshcut}
  FMTID_Intshcut: TGUID = (
    D1:$000214A0; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM PSGUID_INTERNETSITE}
  PSGUID_INTERNETSITE: TGUID = (
    D1:$000214A1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM FMTID_InternetSite}
  FMTID_InternetSite: TGUID = (
    D1:$000214A1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM CGID_Explorer}
  CGID_Explorer: TGUID = (
    D1:$000214D0; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CGID_ShellDocView}
  CGID_ShellDocView: TGUID = (
    D1:$000214D1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM CGID_ShellServiceObject}
  CGID_ShellServiceObject: TGUID = (
    D1:$000214D2; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM CGID_ExplorerBarDoc}
  CGID_ExplorerBarDoc: TGUID = (
    D1:$000214D3; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM IID_INewShortcutHookA}
  IID_INewShortcutHookA: TGUID = (
    D1:$000214E1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellBrowser}
  IID_IShellBrowser: TGUID = (
    D1:$000214E2; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellView}
  IID_IShellView: TGUID = (
    D1:$000214E3; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IContextMenu}
  IID_IContextMenu: TGUID = (
    D1:$000214E4; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellIcon}
  IID_IShellIcon: TGUID = (
    D1:$000214E5; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellFolder}
  IID_IShellFolder: TGUID = (
    D1:$000214E6; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellExtInit}
  IID_IShellExtInit: TGUID = (
    D1:$000214E8; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellPropSheetExt}
  IID_IShellPropSheetExt: TGUID = (
    D1:$000214E9; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IPersistFolder}
  IID_IPersistFolder: TGUID = (
    D1:$000214EA; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IExtractIconA}
  IID_IExtractIconA: TGUID = (
    D1:$000214EB; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellDetails}
  IID_IShellDetails: TGUID = (
    D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IDelayedRelease}
  IID_IDelayedRelease: TGUID = (
    D1:$000214ED; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellLinkA}
  IID_IShellLinkA: TGUID = (
    D1:$000214EE; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellCopyHookA}
  IID_IShellCopyHookA: TGUID = (
    D1:$000214EF; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IFileViewerA}
  IID_IFileViewerA: TGUID = (
    D1:$000214F0; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_ICommDlgBrowser}
  IID_ICommDlgBrowser: TGUID = (
    D1:$000214F1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IEnumIDList}
  IID_IEnumIDList: TGUID = (
    D1:$000214F2; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IFileViewerSite}
  IID_IFileViewerSite: TGUID = (
    D1:$000214F3; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IContextMenu2}
  IID_IContextMenu2: TGUID = (
    D1:$000214F4; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellExecuteHookA}
  IID_IShellExecuteHookA: TGUID = (
    D1:$000214F5; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IPropSheetPage}
  IID_IPropSheetPage: TGUID = (
    D1:$000214F6; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_INewShortcutHookW}
  IID_INewShortcutHookW: TGUID = (
    D1:$000214F7; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IFileViewerW}
  IID_IFileViewerW: TGUID = (
    D1:$000214F8; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellLinkW}
  IID_IShellLinkW: TGUID = (
    D1:$000214F9; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IExtractIconW}
  IID_IExtractIconW: TGUID = (
    D1:$000214FA; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellExecuteHookW}
  IID_IShellExecuteHookW: TGUID = (
    D1:$000214FB; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellCopyHookW}
  IID_IShellCopyHookW: TGUID = (
    D1:$000214FC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM IID_IRemoteComputer}
  IID_IRemoteComputer: TGUID = (
    D1:$000214FE; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM IID_ICopyHookA}
  IID_ICopyHookA: TGUID = (
    D1:$000214EF; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_ICopyHookW}
  IID_ICopyHookW: TGUID = (
    D1:$000214FC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM SID_LinkSite}
  SID_LinkSite: TGUID = (
    D1:$000214F9; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM IID_IQueryInfo}
  IID_IQueryInfo: TGUID = (
    D1:$00021500; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  {$EXTERNALSYM IID_IBriefcaseStg}
  IID_IBriefcaseStg: TGUID = (
    D1:$8BCE1FA1; D2:$0921; D3:$101B; D4:($B1,$FF,$00,$DD,$01,$0C,$CC,$48));
  {$EXTERNALSYM IID_IShellView2}
  IID_IShellView2: TGUID = (
    D1:$88E39E80; D2:$3578; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));

  {$EXTERNALSYM IID_IShellLinkDataList}
  IID_IShellLinkDataList: TGUID = (
    D1:$45E2B4AE; D2:$B1C3; D3:$11D0; D4:($B9,$2F,$00,$A0,$C9,$03,$12,$E1));
  {$EXTERNALSYM IID_IResolveShellLink}
  IID_IResolveShellLink: TGUID = (
    D1:$5CD52983; D2:$9449; D3:$11D2; D4:($96,$3A,$00,$C0,$4F,$79,$AD,$F0));

  {$EXTERNALSYM IID_IURLSearchHook}
  IID_IURLSearchHook: TGUID = (
    D1:$AC60F6A0; D2:$0FD9; D3:$11D0; D4:($99,$CB,$00,$C0,$4F,$D6,$44,$97));
  {$EXTERNALSYM IID_ISearchContext}
  IID_ISearchContext: TGUID = (
    D1:$09F656A2; D2:$41AF; D3:$480C; D4:($88,$F7,$16,$CC,$0D,$16,$46,$15));
  {$EXTERNALSYM IID_IURLSearchHook2}
  IID_IURLSearchHook2: TGUID = (
    D1:$5EE44DA4; D2:$6D32; D3:$46E3; D4:($86,$BC,$07,$54,$0D,$ED,$D0,$E0));

{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IID_IDefViewID}
  IID_IDefViewID: TGUID = (
    D1:$985F64F0; D2:$D410; D3:$4E02; D4:($BE,$22,$DA,$07,$F2,$B5,$C5,$E1));

  {$EXTERNALSYM CLSID_ShellTaskScheduler}
  CLSID_ShellTaskScheduler: TGUID = (
    D1:$603D3800; D2:$BD81; D3:$11D0; D4:($A3,$A5,$00,$C0,$4F,$D7,$06,$EC));
  {$EXTERNALSYM IID_IShellTaskScheduler}
  IID_IShellTaskScheduler: TGUID = (
    D1:$6CCB7BE0; D2:$6807; D3:$11D0; D4:($B8,$10,$00,$C0,$4F,$D7,$06,$EC));

type
  {$EXTERNALSYM IDefViewID}
  IDefViewID = interface(IUnknown)
  ['{985F64F0-D410-4E02-BE22-DA07F2B5C5E1}']
  end;

const
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CLSID_FolderShortcut}
  CLSID_FolderShortcut: TGUID = (
    D1:$0AFACED1; D2:$E828; D3:$11D1; D4:($91,$87,$B5,$32,$F1,$E9,$57,$5D));

  {$EXTERNALSYM CLSID_StgFolder}
  CLSID_StgFolder: TGUID = (
    D1:$E773F1AF; D2:$3A65; D3:$4866; D4:($85,$7D,$84,$6F,$C9,$C4,$59,$8A));

  {$EXTERNALSYM IID_IInputObject}
  IID_IInputObject: TGUID = (
    D1:$68284FAA; D2:$6A48; D3:$11D0; D4:($8C,$78,$00,$C0,$4F,$D9,$18,$B4));
  {$EXTERNALSYM IID_IInputObjectSite}
  IID_IInputObjectSite: TGUID = (
    D1:$F1DB8392; D2:$7331; D3:$11D0; D4:($8C,$99,$00,$A0,$C9,$2D,$BF,$E8));

  {$EXTERNALSYM IID_IDockingWindowSite}
  IID_IDockingWindowSite: TGUID = (
    D1:$2A342FC2; D2:$7B26; D3:$11D0; D4:($8C,$A9,$00,$A0,$C9,$2D,$BF,$E8));
  {$EXTERNALSYM IID_IDockingWindowFrame}
  IID_IDockingWindowFrame: TGUID = (
    D1:$47D2657A; D2:$7B27; D3:$11D0; D4:($8C,$A9,$00,$A0,$C9,$2D,$BF,$E8));

  {$EXTERNALSYM IID_IShellIconOverlay}
  IID_IShellIconOverlay: TGUID = (
    D1:$7D688A70; D2:$C613; D3:$11D0; D4:($99,$9B,$00,$C0,$4F,$D6,$55,$E1));
  {$EXTERNALSYM IID_IShellIconOverlayIdentifier}
  IID_IShellIconOverlayIdentifier: TGUID = (
    D1:$0C6C4200; D2:$C589; D3:$11D0; D4:($99,$9A,$00,$C0,$4F,$D6,$55,$E1));

  {$EXTERNALSYM IID_ICommDlgBrowser2}
  IID_ICommDlgBrowser2: TGUID = (
    D1:$10339516; D2:$2894; D3:$11D2; D4:($90,$39,$00,$C0,$4F,$8E,$EB,$3E));

  {$EXTERNALSYM IID_IShellFolderViewCB}
  IID_IShellFolderViewCB: TGUID = (
    D1:$2047E320; D2:$F2A9; D3:$11CE; D4:($AE,$65,$08,$00,$2B,$2E,$12,$62));
  {$EXTERNALSYM SID_ShellFolderViewCB}
  SID_ShellFolderViewCB: TGUID = (
    D1:$2047E320; D2:$F2A9; D3:$11CE; D4:($AE,$65,$08,$00,$2B,$2E,$12,$62));

  {$EXTERNALSYM IID_IPersistFolder3}
  IID_IPersistFolder3: TGUID = (
    D1:$CEF04FDF; D2:$FE72; D3:$11D2; D4:($87,$A5,$00,$C0,$4F,$68,$37,$CF));

  {$EXTERNALSYM CLSID_CFSIconOverlayManager}
  CLSID_CFSIconOverlayManager: TGUID = (
    D1:$63B51F81; D2:$C868; D3:$11D0; D4:($99,$9C,$00,$C0,$4F,$D6,$55,$E1));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IID_IShellIconOverlayManager}
  IID_IShellIconOverlayManager: TGUID = (
    D1:$F10B5E34; D2:$DD3B; D3:$42A7; D4:($AA,$7D,$2F,$4E,$C5,$4B,$B0,$9B));

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM IID_IRunnableTask}
  IID_IRunnableTask: TGUID = (
    D1:$85788D00; D2:$6807; D3:$11D0; D4:($B8,$10,$00,$C0,$4F,$D7,$06,$EC));

  {$EXTERNALSYM IID_IThumbnailCapture}
  IID_IThumbnailCapture: TGUID = (
    D1:$4EA39266; D2:$7211; D3:$409F; D4:($B6,$22,$F6,$3D,$BD,$16,$C5,$33));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IID_IShellImageStore}
  IID_IShellImageStore: TGUID = (
    D1:$48C8118C; D2:$B924; D3:$11D1; D4:($98,$D5,$00,$C0,$4F,$B6,$87,$DA));

  {$EXTERNALSYM IID_IEnumShellImageStore}
  IID_IEnumShellImageStore: TGUID = (
    D1:$6DFD582B; D2:$92E3; D3:$11D1; D4:($98,$A3,$00,$C0,$4F,$B6,$87,$DA));

  {$EXTERNALSYM CLSID_ShellThumbnailDiskCache}
  CLSID_ShellThumbnailDiskCache: TGUID = (
    D1:$1EBDCF80; D2:$A200; D3:$11D0; D4:($A3,$A4,$00,$C0,$4F,$D7,$06,$EC));

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM IID_IContextMenu3}
  IID_IContextMenu3: TGUID = (
    D1:$BCFCE0A0; D2:$EC17; D3:$11D0; D4:($8D,$10,$00,$A0,$C9,$0F,$27,$19));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM SID_DefView}
  SID_DefView: TGUID = (
    D1:$6D12FE80; D2:$7911; D3:$11CF; D4:($95,$34,$00,$00,$C0,$5B,$AE,$0B));

  {$EXTERNALSYM CGID_DefView}
  CGID_DefView: TGUID = (
    D1:$4AF07F10; D2:$D231; D3:$11D0; D4:($B9,$42,$00,$A0,$C9,$03,$12,$E1));

  {$EXTERNALSYM CLSID_MenuBand}
  CLSID_MenuBand: TGUID = (
    D1:$5B4DAE26; D2:$B807; D3:$11D0; D4:($98,$15,$00,$C0,$4F,$D9,$19,$72));

// IShellFolderBand stuff
  {$EXTERNALSYM IID_IShellFolderBand}
  IID_IShellFolderBand: TGUID = (
    D1:$7FE80CC8; D2:$C247; D3:$11D0; D4:($B9,$3A,$00,$A0,$C9,$03,$12,$E1));
  {$EXTERNALSYM IID_IDefViewFrame}
  IID_IDefViewFrame: TGUID = (
    D1:$710EB7A0; D2:$45ED; D3:$11D0; D4:($92,$4A,$00,$20,$AF,$C7,$AC,$4D));

  {$EXTERNALSYM VID_LargeIcons}
  VID_LargeIcons: TGUID = (
    D1:$0057D0E0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  {$EXTERNALSYM VID_SmallIcons}
  VID_SmallIcons: TGUID = (
    D1:$089000C0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  {$EXTERNALSYM VID_List}
  VID_List      : TGUID = (
    D1:$0E1FA5E0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  {$EXTERNALSYM VID_Details}
  VID_Details   : TGUID = (
    D1:$137E7700; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  {$EXTERNALSYM VID_Tile}
  VID_Tile      : TGUID = (
    D1:$65F125E5; D2:$7BE1; D3:$4810; D4:($BA,$9D,$D2,$71,$C8,$43,$2C,$E3));


// NOTE: This has to be the same as the old CLSID_Thumbnails
  {$EXTERNALSYM VID_Thumbnails}
  VID_Thumbnails: TGUID = (
    D1:$8BEBB290; D2:$52D0; D3:$11D0; D4:($B7,$F4,$00,$C0,$4F,$D7,$06,$EC));

// ThumbStrip
  {$EXTERNALSYM VID_ThumbStrip}
  VID_ThumbStrip: TGUID = (
    D1:$8EEFA624; D2:$D1E9; D3:$445B; D4:($94,$B7,$74,$FB,$CE,$2E,$A1,$1A));

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM SID_SShellBrowser}
  SID_SShellBrowser: TGUID = (// IID_IShellBrowser
    D1:$000214E2; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM SID_SShellDesktop}
  SID_SShellDesktop: TGUID = (// CLSID_ShellDesktop
    D1:$00021400; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

//
//  IShellDiscardable is an IID-only interface. If the object supports this
// interface it can be discarded anytime. IWebBrowser::PutProperty QI's for
// this interface to detect discardable properties.
//
  {$EXTERNALSYM IID_IDiscardableBrowserProperty}
  IID_IDiscardableBrowserProperty: TGUID = (
    D1:$49C3DE7C; D2:$D329; D3:$11D0; D4:($AB,$73,$00,$C0,$4F,$C3,$3E,$80));

// IShellChangeNotify is a sink of PItemIDList notification events
//
  {$EXTERNALSYM IID_IShellChangeNotify}
  IID_IShellChangeNotify: TGUID = (
    D1:$D82BE2B1; D2:$5764; D3:$11D0; D4:($A9,$6E,$00,$C0,$4F,$D7,$05,$A2));

  {$EXTERNALSYM IID_IFileViewer}
  IID_IFileViewer: TGUID = (       // IID_IFileViewerA
    D1:$000214F0; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellLink}
  IID_IShellLink: TGUID = (        // IID_IShellLinkA
    D1:$000214EE; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IExtractIcon}
  IID_IExtractIcon: TGUID = (      // IID_IExtractIconA
    D1:$000214EB; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellCopyHook}
  IID_IShellCopyHook: TGUID = (    // IID_IShellCopyHookA
    D1:$000214EF; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_IShellExecuteHook}
  IID_IShellExecuteHook: TGUID = ( // IID_IShellExecuteHookA
    D1:$000214F5; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$EXTERNALSYM IID_INewShortcutHook}
  IID_INewShortcutHook: TGUID = (  // IID_INewShortcutHookA
    D1:$000214E1; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));


//
// isguids.h - Internet Shortcut GUID definitions.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//

  {$EXTERNALSYM CLSID_InternetShortcut}
  CLSID_InternetShortcut: TGUID = (
    D1:$FBF23B40; D2:$E3F0; D3:$101B; D4:($84,$88,$00,$AA,$00,$3E,$56,$F8));

  {$EXTERNALSYM IID_IUniformResourceLocatorA}
  IID_IUniformResourceLocatorA: TGUID = (
    D1:$FBF23B80; D2:$E3F0; D3:$101B; D4:($84,$88,$00,$AA,$00,$3E,$56,$F8));
  {$EXTERNALSYM IID_IUniformResourceLocatorW}
  IID_IUniformResourceLocatorW: TGUID = (
    D1:$CABB0DA0; D2:$DA57; D3:$11CF; D4:($99,$74,$00,$20,$AF,$D7,$97,$62));
  {$EXTERNALSYM IID_IUniformResourceLocator}
  IID_IUniformResourceLocator: TGUID = (
    D1:$FBF23B80; D2:$E3F0; D3:$101B; D4:($84,$88,$00,$AA,$00,$3E,$56,$F8));


//#include <shldisp.h>

// UrlHistory Guids
  {$EXTERNALSYM CLSID_CUrlHistory}
  CLSID_CUrlHistory: TGUID = (
    D1:$3C374A40; D2:$BAE4; D3:$11CF; D4:($BF,$7D,$00,$AA,$00,$69,$46,$EE));
  {$EXTERNALSYM SID_SUrlHistory}
  SID_SUrlHistory: TGUID = (
    D1:$3C374A40; D2:$BAE4; D3:$11CF; D4:($BF,$7D,$00,$AA,$00,$69,$46,$EE));

//UrlSearchHook Guids
  {$EXTERNALSYM CLSID_CURLSearchHook}
  CLSID_CURLSearchHook: TGUID = (
    D1:$CFBFAE00; D2:$17A6; D3:$11D0; D4:($99,$CB,$00,$C0,$4F,$D6,$44,$97));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM SID_SInternetExplorer}
  SID_SInternetExplorer: TGUID = '{0002DF05-0000-0000-C000-000000000046}'; //IID_IWebBrowserApp
  {$EXTERNALSYM SID_SWebBrowserApp}
  SID_SWebBrowserApp: TGUID = '{0002DF05-0000-0000-C000-000000000046}';    //IID_IWebBrowserApp

// AutoComplete Guids
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM IID_IAutoCompList}
  IID_IAutoCompList: TGUID = (
    D1:$00BB2760; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  {$EXTERNALSYM IID_IObjMgr}
  IID_IObjMgr: TGUID = (
    D1:$00BB2761; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  {$EXTERNALSYM IID_IACList}
  IID_IACList: TGUID = (
    D1:$77A130B0; D2:$94FD; D3:$11D0; D4:($A5,$44,$00,$C0,$4F,$D7,$d0,$62));
  {$EXTERNALSYM IID_IACList2}
  IID_IACList2: TGUID = (
    D1:$470141a0; D2:$5186; D3:$11d2; D4:($bb,$b6,$00,$60,$97,$7b,$46,$4c));
  {$EXTERNALSYM IID_ICurrentWorkingDirectory}
  IID_ICurrentWorkingDirectory: TGUID = (
    D1:$91956D21; D2:$9276; D3:$11D1; D4:($92,$1A,$00,$60,$97,$DF,$5B,$D4));
  {$EXTERNALSYM CLSID_AutoComplete}
  CLSID_AutoComplete: TGUID = (
    D1:$00BB2763; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  {$EXTERNALSYM CLSID_ACLHistory}
  CLSID_ACLHistory: TGUID = (
    D1:$00BB2764; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  {$EXTERNALSYM CLSID_ACListISF}
  CLSID_ACListISF: TGUID = (
    D1:$03C036F1; D2:$A186; D3:$11D0; D4:($82,$4A,$00,$AA,$00,$5B,$43,$83));
  {$EXTERNALSYM CLSID_ACLMRU}
  CLSID_ACLMRU: TGUID = (
    D1:$6756a641; D2:$de71; D3:$11d0; D4:($83,$1b,$00,$aa,$00,$5b,$43,$83));
  {$EXTERNALSYM CLSID_ACLMulti}
  CLSID_ACLMulti: TGUID = (
    D1:$00BB2765; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  {$EXTERNALSYM CLSID_ACLCustomMRU}
  CLSID_ACLCustomMRU: TGUID = (
    D1:$6935DB93; D2:$21E8; D3:$4CCC; D4:($BE,$B9,$9F,$E3,$C7,$7A,$29,$7A));


// IProgressDialog
  {$EXTERNALSYM CLSID_ProgressDialog}
  CLSID_ProgressDialog: TGUID = (
    D1:$F8383852; D2:$FCD3; D3:$11D1; D4:($A6,$B9,$00,$60,$97,$DF,$5B,$D4));
  {$EXTERNALSYM IID_IProgressDialog}
  IID_IProgressDialog: TGUID = (
    D1:$EBBC7C04; D2:$315E; D3:$11D2; D4:($B6,$2F,$00,$60,$97,$DF,$5B,$D4));

//
// Progress objects exposed via QueryService
//
  {$EXTERNALSYM SID_SProgressUI}
  SID_SProgressUI: TGUID = (
    D1:$F8383852; D2:$FCD3; D3:$11D1; D4:($A6,$B9,$00,$60,$97,$DF,$5B,$D4));


//
// Top-most browser implementation in the heirarchy. use IServiceProvider::QueryService()
// to get to interfaces (IID_IShellBrowser, IID_IBrowserService, etc.)
//
  {$EXTERNALSYM SID_STopLevelBrowser}
  SID_STopLevelBrowser: TGUID = (
    D1:$4C96BE40; D2:$915C; D3:$11CF; D4:($99,$D3,$00,$AA,$00,$4A,$E8,$37));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM CLSID_FileTypes}
  CLSID_FileTypes: TGUID = (
    D1:$B091E540; D2:$83E3; D3:$11CF; D4:($A7,$13,$00,$20,$AF,$D7,$97,$62));

{$IFNDEF JWA_INCLUDEMODE}    
  {$EXTERNALSYM CLSID_ActiveDesktop}
  CLSID_ActiveDesktop: TGUID = (
    D1:$75048700; D2:$EF1F; D3:$11D0; D4:($98,$88,$00,$60,$97,$DE,$AC,$F9));

  {$EXTERNALSYM IID_IActiveDesktop}
  IID_IActiveDesktop: TGUID = (
    D1:$F490EB00; D2:$1240; D3:$11D1; D4:($98,$88,$00,$60,$97,$DE,$AC,$F9));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IID_IActiveDesktopP}
  IID_IActiveDesktopP: TGUID = (
    D1:$52502EE0; D2:$EC80; D3:$11D0; D4:($89,$AB,$00,$C0,$4F,$C2,$97,$2D));

  {$EXTERNALSYM IID_IADesktopP2}
  IID_IADesktopP2: TGUID = (
    D1:$B22754E2; D2:$4574; D3:$11D1; D4:($98,$88,$00,$60,$97,$DE,$AC,$F9));

{$IFNDEF JWA_INCLUDEMODE}    
  {$EXTERNALSYM IID_ISynchronizedCallBack}
  IID_ISynchronizedCallBack: TGUID = (
    D1:$74C26041; D2:$70D1; D3:$11D1; D4:($B7,$5A,$00,$A0,$C9,$05,$64,$FE));

// Extended column handler interfaces
  {$EXTERNALSYM IID_IShellDetails3}
  IID_IShellDetails3: TGUID = (
    D1:$D2A105C0; D2:$87D5; D3:$11D1; D4:($83,$91,$00,$00,$F8,$04,$61,$CF));

  {$EXTERNALSYM IID_IQueryAssociations}
  IID_IQueryAssociations: TGUID = (
    D1:$C46CA590; D2:$3C3F; D3:$11D2; D4:($BE,$E6,$00,$00,$F8,$05,$CA,$57));

  {$EXTERNALSYM CLSID_QueryAssociations}
  CLSID_QueryAssociations: TGUID = (
    D1:$A07034FD; D2:$6CAA; D3:$4954; D4:($AC,$3F,$97,$A2,$72,$16,$F9,$8A));

  {$EXTERNALSYM IID_IColumnProvider}
  IID_IColumnProvider: TGUID = (
    D1:$E8025004; D2:$1C42; D3:$11D2; D4:($BE,$2C,$00,$A0,$C9,$A8,$3D,$A1));

  {$EXTERNALSYM CLSID_DocFileColumnProvider}
  CLSID_DocFileColumnProvider: TGUID = (
    D1:$24F14F01; D2:$7B1C; D3:$11D1; D4:($83,$8F,$00,$00,$F8,$04,$61,$CF));

  {$EXTERNALSYM CLSID_LinkColumnProvider}
  CLSID_LinkColumnProvider: TGUID = (
    D1:$24F14F02; D2:$7B1C; D3:$11D1; D4:($83,$8F,$00,$00,$F8,$04,$61,$CF));

  {$EXTERNALSYM CLSID_FileSysColumnProvider}
  CLSID_FileSysColumnProvider: TGUID = (
    D1:$0D2E74C4; D2:$3C34; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));

// For Internet Shortcut Creation Command
// The shell uses this command to allow trident to save off it's per frame data in the shortcut
// and to allow it to fire the OnPersist() event on every frame
  {$EXTERNALSYM CGID_ShortCut}
  CGID_ShortCut: TGUID = (
    D1:$93A68750; D2:$951A; D3:$11D1; D4:($94,$6F,$00,$00,$00,$00,$00,$00));

  {$EXTERNALSYM IID_INamedPropertyBag}
  IID_INamedPropertyBag: TGUID = (
    D1:$FB700430; D2:$952C; D3:$11D1; D4:($94,$6F,$00,$00,$00,$00,$00,$00));

  {$EXTERNALSYM CLSID_InternetButtons}
  CLSID_InternetButtons: TGUID = (
    D1:$1E796980; D2:$9CC5; D3:$11D1; D4:($A8,$3F,$00,$C0,$4F,$C9,$9D,$61));

  {$EXTERNALSYM CLSID_MSOButtons}
  CLSID_MSOButtons: TGUID = (
    D1:$178F34B8; D2:$A282; D3:$11D2; D4:($86,$C5,$00,$C0,$4F,$8E,$EA,$99));

  {$EXTERNALSYM CLSID_ToolbarExtButtons}
  CLSID_ToolbarExtButtons: TGUID = (
    D1:$2CE4B5D8; D2:$A28F; D3:$11D2; D4:($86,$C5,$00,$C0,$4F,$8E,$EA,$99));

  {$EXTERNALSYM CLSID_DarwinAppPublisher}
  CLSID_DarwinAppPublisher: TGUID = (
    D1:$CFCCC7A0; D2:$A282; D3:$11D1; D4:($90,$82,$00,$60,$08,$05,$93,$82));

  {$EXTERNALSYM CLSID_DocHostUIHandler}
  CLSID_DocHostUIHandler: TGUID = (
    D1:$7057e952; D2:$bd1b; D3:$11d1; D4:($89,$19,$00,$c0,$4f,$c2,$c8,$36));

  {$EXTERNALSYM IID_IShellFolder2}
  IID_IShellFolder2: TGUID = (
    D1:$93F2F68C; D2:$1D1B; D3:$11D3; D4:($A3,$0E,$00,$C0,$4F,$79,$AB,$D1));

  {$EXTERNALSYM PSGUID_SHELLDETAILS}
  PSGUID_SHELLDETAILS: TGUID = (
    D1:$28636AA6; D2:$953D; D3:$11D2; D4:($B5,$D6,$00,$C0,$4F,$D9,$18,$D0));
  {$EXTERNALSYM FMTID_ShellDetails}
  FMTID_ShellDetails: TGUID = (
    D1:$28636AA6; D2:$953D; D3:$11D2; D4:($B5,$D6,$00,$C0,$4F,$D9,$18,$D0));

  {$EXTERNALSYM PID_FINDDATA}
  PID_FINDDATA        = 0;
  {$EXTERNALSYM PID_NETRESOURCE}
  PID_NETRESOURCE     = 1;
  {$EXTERNALSYM PID_DESCRIPTIONID}
  PID_DESCRIPTIONID   = 2;
  {$EXTERNALSYM PID_WHICHFOLDER}
  PID_WHICHFOLDER     = 3;
  {$EXTERNALSYM PID_NETWORKLOCATION}
  PID_NETWORKLOCATION = 4;
  {$EXTERNALSYM PID_COMPUTERNAME}
  PID_COMPUTERNAME    = 5;

// PSGUID_STORAGE comes from ntquery.h
  {$EXTERNALSYM PSGUID_STORAGE}
  PSGUID_STORAGE: TGUID = (
    D1:$B725F130; D2:$47EF; D3:$101A; D4:($A5,$F1,$02,$60,$8C,$9E,$EB,$AC));
  {$EXTERNALSYM FMTID_Storage}
  FMTID_Storage: TGUID = (
    D1:$B725F130; D2:$47EF; D3:$101A; D4:($A5,$F1,$02,$60,$8C,$9E,$EB,$AC));

// Image properties
  {$EXTERNALSYM PSGUID_IMAGEPROPERTIES}
  PSGUID_IMAGEPROPERTIES: TGUID = (
    D1:$14B81DA1; D2:$0135; D3:$4D31; D4:($96,$D9,$6C,$BF,$C9,$67,$1A,$99));
  {$EXTERNALSYM FMTID_ImageProperties}
  FMTID_ImageProperties: TGUID = (
    D1:$14B81DA1; D2:$0135; D3:$4D31; D4:($96,$D9,$6C,$BF,$C9,$67,$1A,$99));

// The GUIDs used to identify shell item attributes (columns).
// See IShellFolder2::GetDetailsEx implementations...

  {$EXTERNALSYM PSGUID_DISPLACED}
  PSGUID_DISPLACED: TGUID = (
    D1:$9B174B33; D2:$40FF; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));
  {$EXTERNALSYM FMTID_Displaced}
  FMTID_Displaced: TGUID = (
    D1:$9B174B33; D2:$40FF; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));

  {$EXTERNALSYM PID_DISPLACED_FROM}
  PID_DISPLACED_FROM  = 2;
  {$EXTERNALSYM PID_DISPLACED_DATE}
  PID_DISPLACED_DATE  = 3;

  {$EXTERNALSYM PSGUID_BRIEFCASE}
  PSGUID_BRIEFCASE: TGUID = (
    D1:$328D8B21; D2:$7729; D3:$4BFC; D4:($95,$4C,$90,$2B,$32,$9D,$56,$B0));
  {$EXTERNALSYM FMTID_Briefcase}
  FMTID_Briefcase: TGUID = (
    D1:$328D8B21; D2:$7729; D3:$4BFC; D4:($95,$4C,$90,$2B,$32,$9D,$56,$B0));

  {$EXTERNALSYM PID_SYNC_COPY_IN}
  PID_SYNC_COPY_IN    = 2;

  {$EXTERNALSYM PSGUID_MISC}
  PSGUID_MISC: TGUID = (
    D1:$9B174B34; D2:$40FF; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));
  {$EXTERNALSYM FMTID_Misc}
  FMTID_Misc: TGUID = (
    D1:$9B174B34; D2:$40FF; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));

  {$EXTERNALSYM PID_MISC_STATUS}
  PID_MISC_STATUS         = 2;
  {$EXTERNALSYM PID_MISC_ACCESSCOUNT}
  PID_MISC_ACCESSCOUNT    = 3;
  {$EXTERNALSYM PID_MISC_OWNER}
  PID_MISC_OWNER          = 4;
  {$EXTERNALSYM PID_HTMLINFOTIPFILE}
  PID_HTMLINFOTIPFILE     = 5;
  {$EXTERNALSYM PID_MISC_PICS}
  PID_MISC_PICS           = 6;

  {$EXTERNALSYM PSGUID_WEBVIEW}
  PSGUID_WEBVIEW: TGUID = (
    D1:$F2275480; D2:$F782; D3:$4291; D4:($BD,$94,$F1,$36,$93,$51,$3A,$EC));
  {$EXTERNALSYM FMTID_WebView}
  FMTID_WebView: TGUID = (
    D1:$F2275480; D2:$F782; D3:$4291; D4:($BD,$94,$F1,$36,$93,$51,$3A,$EC));

  {$EXTERNALSYM PID_DISPLAY_PROPERTIES}
  PID_DISPLAY_PROPERTIES  = 0;
  {$EXTERNALSYM PID_INTROTEXT}
  PID_INTROTEXT           = 1;

  {$EXTERNALSYM PSGUID_MUSIC}
  PSGUID_MUSIC: TGUID = (
    D1:$56A3372E; D2:$CE9C; D3:$11D2; D4:($9F,$0E,$00,$60,$97,$C6,$86,$F6));
  {$EXTERNALSYM FMTID_MUSIC}
  FMTID_MUSIC: TGUID = (
    D1:$56A3372E; D2:$CE9C; D3:$11D2; D4:($9F,$0E,$00,$60,$97,$C6,$86,$F6));

  {$EXTERNALSYM PIDSI_ARTIST}
  PIDSI_ARTIST    = 2;
  {$EXTERNALSYM PIDSI_SONGTITLE}
  PIDSI_SONGTITLE = 3;
  {$EXTERNALSYM PIDSI_ALBUM}
  PIDSI_ALBUM     = 4;
  {$EXTERNALSYM PIDSI_YEAR}
  PIDSI_YEAR      = 5;
  {$EXTERNALSYM PIDSI_COMMENT}
  PIDSI_COMMENT   = 6;
  {$EXTERNALSYM PIDSI_TRACK}
  PIDSI_TRACK     = 7;
  {$EXTERNALSYM PIDSI_GENRE}
  PIDSI_GENRE     = 11;
  {$EXTERNALSYM PIDSI_LYRICS}
  PIDSI_LYRICS    = 12;

  {$EXTERNALSYM PSGUID_DRM}
  PSGUID_DRM: TGUID = (
    D1:$AEAC19E4; D2:$89AE; D3:$4508; D4:($B9,$B7,$BB,$86,$7A,$BE,$E2,$ED));
  {$EXTERNALSYM FMTID_DRM}
  FMTID_DRM: TGUID = (
    D1:$AEAC19E4; D2:$89AE; D3:$4508; D4:($B9,$B7,$BB,$86,$7A,$BE,$E2,$ED));

  {$EXTERNALSYM PIDDRSI_PROTECTED}
  PIDDRSI_PROTECTED    = 2;
  {$EXTERNALSYM PIDDRSI_DESCRIPTION}
  PIDDRSI_DESCRIPTION  = 3;
  {$EXTERNALSYM PIDDRSI_PLAYCOUNT}
  PIDDRSI_PLAYCOUNT    = 4;
  {$EXTERNALSYM PIDDRSI_PLAYSTARTS}
  PIDDRSI_PLAYSTARTS   = 5;
  {$EXTERNALSYM PIDDRSI_PLAYEXPIRES}
  PIDDRSI_PLAYEXPIRES  = 6;
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM PSGUID_VIDEO}
  PSGUID_VIDEO: TGUID = '{64440491-4c8b-11d1-8b70-080036b11a03}';

{$IFNDEF JWA_INCLUDEMODE}
//  FMTID_VideoSummaryInformation property identifiers
  {$EXTERNALSYM PIDVSI_STREAM_NAME}
  PIDVSI_STREAM_NAME      = $00000002; // "StreamName", VT_LPWSTR
  {$EXTERNALSYM PIDVSI_FRAME_WIDTH}
  PIDVSI_FRAME_WIDTH      = $00000003; // "FrameWidth", VT_UI4
  {$EXTERNALSYM PIDVSI_FRAME_HEIGHT}
  PIDVSI_FRAME_HEIGHT     = $00000004; // "FrameHeight", VT_UI4
  {$EXTERNALSYM PIDVSI_TIMELENGTH}
  PIDVSI_TIMELENGTH       = $00000007; // "TimeLength", VT_UI4, milliseconds
  {$EXTERNALSYM PIDVSI_FRAME_COUNT}
  PIDVSI_FRAME_COUNT      = $00000005; // "FrameCount". VT_UI4
  {$EXTERNALSYM PIDVSI_FRAME_RATE}
  PIDVSI_FRAME_RATE       = $00000006; // "FrameRate", VT_UI4, frames/millisecond
  {$EXTERNALSYM PIDVSI_DATA_RATE}
  PIDVSI_DATA_RATE        = $00000008; // "DataRate", VT_UI4, bytes/second
  {$EXTERNALSYM PIDVSI_SAMPLE_SIZE}
  PIDVSI_SAMPLE_SIZE      = $00000009; // "SampleSize", VT_UI4
  {$EXTERNALSYM PIDVSI_COMPRESSION}
  PIDVSI_COMPRESSION      = $0000000A; // "Compression", VT_LPWSTR
  {$EXTERNALSYM PIDVSI_STREAM_NUMBER}
  PIDVSI_STREAM_NUMBER    = $0000000B; // "StreamNumber", VT_UI2
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM PSGUID_AUDIO}
  PSGUID_AUDIO: TGUID = '{64440490-4c8b-11d1-8b70-080036b11a03}';

{$IFNDEF JWA_INCLUDEMODE}  
//  FMTID_AudioSummaryInformation property identifiers
  {$EXTERNALSYM PIDASI_FORMAT}
  PIDASI_FORMAT           = $00000002; // VT_BSTR
  {$EXTERNALSYM PIDASI_TIMELENGTH}
  PIDASI_TIMELENGTH       = $00000003; // VT_UI4, milliseconds
  {$EXTERNALSYM PIDASI_AVG_DATA_RATE}
  PIDASI_AVG_DATA_RATE    = $00000004; // VT_UI4,  Hz
  {$EXTERNALSYM PIDASI_SAMPLE_RATE}
  PIDASI_SAMPLE_RATE      = $00000005; // VT_UI4,  bits
  {$EXTERNALSYM PIDASI_SAMPLE_SIZE}
  PIDASI_SAMPLE_SIZE      = $00000006; // VT_UI4,  bits
  {$EXTERNALSYM PIDASI_CHANNEL_COUNT}
  PIDASI_CHANNEL_COUNT    = $00000007; // VT_UI4
  {$EXTERNALSYM PIDASI_STREAM_NUMBER}
  PIDASI_STREAM_NUMBER    = $00000008; // VT_UI2
  {$EXTERNALSYM PIDASI_STREAM_NAME}
  PIDASI_STREAM_NAME      = $00000009; // VT_LPWSTR
  {$EXTERNALSYM PIDASI_COMPRESSION}
  PIDASI_COMPRESSION      = $0000000A; // VT_LPWSTR

  {$EXTERNALSYM PSGUID_CONTROLPANEL}
  PSGUID_CONTROLPANEL: TGUID = '{305ca226-d286-468e-b848-2b2e8e697b74}';
  {$EXTERNALSYM PID_CONTROLPANEL_CATEGORY}
  PID_CONTROLPANEL_CATEGORY = 2;

  {$EXTERNALSYM PSGUID_VOLUME}
  PSGUID_VOLUME: TGUID = '{9B174B35-40FF-11d2-A27E-00C04FC30871}';

  {$EXTERNALSYM FMTID_Volume}
  FMTID_Volume: TGUID = (
    D1:$9B174B35; D2:$40FF; D3:$11D2; D4:($A2,$7E,$00,$C0,$4F,$C3,$08,$71));

  {$EXTERNALSYM PID_VOLUME_FREE}
  PID_VOLUME_FREE         = 2;
  {$EXTERNALSYM PID_VOLUME_CAPACITY}
  PID_VOLUME_CAPACITY     = 3;
  {$EXTERNALSYM PID_VOLUME_FILESYSTEM}
  PID_VOLUME_FILESYSTEM   = 4;

  {$EXTERNALSYM PSGUID_SHARE}
  PSGUID_SHARE: TGUID = '{D8C3986F-813B-449c-845D-87B95D674ADE}';

  {$EXTERNALSYM PID_SHARE_CSC_STATUS}
  PID_SHARE_CSC_STATUS    = 2;

  {$EXTERNALSYM PSGUID_LINK}
  PSGUID_LINK: TGUID = '{B9B4B3FC-2B51-4a42-B5D8-324146AFCF25}';

  {$EXTERNALSYM PID_LINK_TARGET}
  PID_LINK_TARGET         = 2;

  {$EXTERNALSYM PSGUID_QUERY_D}
  PSGUID_QUERY_D: TGUID = (
    D1:$49691C90; D2:$7E17; D3:$101A; D4:($A9,$1C,$08,$00,$2B,$2E,$CD,$A9));
  {$EXTERNALSYM FMTID_Query}
  FMTID_Query: TGUID = (
    D1:$49691C90; D2:$7E17; D3:$101A; D4:($A9,$1C,$08,$00,$2B,$2E,$CD,$A9));

  {$EXTERNALSYM PID_QUERY_RANK}
  PID_QUERY_RANK  = 2;

{$ENDIF JWA_INCLUDEMODE}

// FMTID_SummaryInformation, see OLE docs for PID_ values for these
  {$EXTERNALSYM PSGUID_SUMMARYINFORMATION}
  PSGUID_SUMMARYINFORMATION: TGUID = '{f29f85e0-4ff9-1068-ab91-08002b27b3d9}';
// FMTID_DocumentSummaryInformation, see OLE docs on the PID_ values for this
  {$EXTERNALSYM PSGUID_DOCUMENTSUMMARYINFORMATION}
  PSGUID_DOCUMENTSUMMARYINFORMATION: TGUID = '{d5cdd502-2e9c-101b-9397-08002b2cf9ae}';

// FMTID_MediaFileSummaryInformation, see propidl.h PID_ values for this
  PSGUID_MEDIAFILESUMMARYINFORMATION: TGUID = '{64440492-4c8b-11d1-8b70-080036b11a03}';

// FMTID_ImageSummaryInformation,, see propidl.h PID_ values for this
  PSGUID_IMAGESUMMARYINFORMATION: TGUID = '{6444048f-4c8b-11d1-8b70-080036b11a03}';

{$IFNDEF JWA_INCLUDEMODE}  
  {$EXTERNALSYM IID_IEnumExtraSearch}
  IID_IEnumExtraSearch: TGUID = (
    D1:$0E700BE1; D2:$9DB6; D3:$11D1; D4:($A1,$CE,$00,$C0,$4F,$D7,$5D,$13));

  {$EXTERNALSYM CLSID_MountedVolume}
  CLSID_MountedVolume: TGUID = (
    D1:$12518493; D2:$00B2; D3:$11d2; D4:($9F,$A5,$9E,$34,$20,$52,$41,$53));

  {$EXTERNALSYM CLSID_HWShellExecute}
  CLSID_HWShellExecute: TGUID = (
    D1:$ffb8655f; D2:$81b9; D3:$4fce; D4:($b8,$9c,$9a,$6b,$a7,$6d,$13,$e7));

  {$EXTERNALSYM IID_IMountedVolume}
  IID_IMountedVolume: TGUID = (
    D1:$12518492; D2:$00B2; D3:$11d2; D4:($9F,$A5,$9E,$34,$20,$52,$41,$53));

  {$EXTERNALSYM CLSID_DragDropHelper}
  CLSID_DragDropHelper: TGUID = (
    D1:$4657278a; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));

  {$EXTERNALSYM IID_IDropTargetHelper}
  IID_IDropTargetHelper: TGUID = (
    D1:$4657278b; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));

  {$EXTERNALSYM IID_IDragSourceHelper}
  IID_IDragSourceHelper: TGUID = (
    D1:$de5bf786; D2:$477a; D3:$11d2; D4:($83,$9d,$00,$c0,$4f,$d9,$18,$d0));

  {$EXTERNALSYM CLSID_CAnchorBrowsePropertyPage}
  CLSID_CAnchorBrowsePropertyPage: TGUID = (
    D1:$3050f3BB; D2:$98b5; D3:$11cf; D4:($bb,$82,$00,$aa,$00,$bd,$ce,$0b));

  {$EXTERNALSYM CLSID_CImageBrowsePropertyPage}
  CLSID_CImageBrowsePropertyPage: TGUID = (
    D1:$3050f3B3; D2:$98b5; D3:$11cf; D4:($bb,$82,$00,$aa,$00,$bd,$ce,$0b));

  {$EXTERNALSYM CLSID_CDocBrowsePropertyPage}
  CLSID_CDocBrowsePropertyPage: TGUID = (
    D1:$3050f3B4; D2:$98b5; D3:$11cf; D4:($bb,$82,$00,$aa,$00,$bd,$ce,$0b));

  {$EXTERNALSYM IID_IFileSystemBindData}
  IID_IFileSystemBindData: TGUID = (
    D1:$01e18d10; D2:$4d8b; D3:$11d2; D4:($85,$5d,$00,$60,$08,$05,$93,$67));

  {$EXTERNALSYM SID_STopWindow}
  SID_STopWindow: TGUID = (
    D1:$49e1b500; D2:$4636; D3:$11d3; D4:($97,$f7,$00,$c0,$4f,$45,$d0,$b3));

  {$EXTERNALSYM SID_SGetViewFromViewDual}
  SID_SGetViewFromViewDual: TGUID = (
    D1:$889A935D; D2:$971E; D3:$4B12; D4:($B9,$0C,$24,$DF,$C9,$E1,$E5,$E8));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM CLSID_FolderItem}
  CLSID_FolderItem: TGUID = (
    D1:$fef10fa2; D2:$355e; D3:$4e06; D4:($93,$81,$9b,$24,$d7,$f7,$cc,$88));

  {$EXTERNALSYM CLSID_FolderItemsFDF}
  CLSID_FolderItemsFDF: TGUID = (
    D1:$53c74826; D2:$ab99; D3:$4d33; D4:($ac,$a4,$31,$17,$f5,$1d,$37,$88));

  {$EXTERNALSYM CLSID_NewMenu}
  CLSID_NewMenu: TGUID = (
    D1:$d969a300; D2:$e7ff; D3:$11d0; D4:($a9,$3b,$00,$a0,$c9,$0f,$27,$19));

{$IFNDEF JWA_INCLUDEMODE}    
// BHIDs for IShellItem::BindToHandler()
// BHID_LocalCopyHelper is #defined as CLSID_LocalCopyHelper, but the latter is
// not defined anywhere.
  {$EXTERNALSYM BHID_SFObject}
  BHID_SFObject: TGUID = (
    D1:$3981e224; D2:$f559; D3:$11d3; D4:($8e,$3a,$00,$c0,$4f,$68,$37,$d5));
  {$EXTERNALSYM BHID_SFUIObject}
  BHID_SFUIObject: TGUID = (
    D1:$3981e225; D2:$f559; D3:$11d3; D4:($8e,$3a,$00,$c0,$4f,$68,$37,$d5));
  {$EXTERNALSYM BHID_SFViewObject}
  BHID_SFViewObject: TGUID = (
    D1:$3981e226; D2:$f559; D3:$11d3; D4:($8e,$3a,$00,$c0,$4f,$68,$37,$d5));
  {$EXTERNALSYM BHID_Storage}
  BHID_Storage: TGUID = (
    D1:$3981e227; D2:$f559; D3:$11d3; D4:($8e,$3a,$00,$c0,$4f,$68,$37,$d5));
  {$EXTERNALSYM BHID_Stream}
  BHID_Stream: TGUID = (
    D1:$1cebb3ab; D2:$7c10; D3:$499a; D4:($a4,$17,$92,$ca,$16,$c4,$cb,$83));
  {$EXTERNALSYM BHID_LinkTargetItem}
  BHID_LinkTargetItem: TGUID = (
    D1:$3981e228; D2:$f559; D3:$11d3; D4:($8e,$3a,$00,$c0,$4f,$68,$37,$d5));
  {$EXTERNALSYM BHID_StorageEnum}
  BHID_StorageEnum: TGUID = (
    D1:$4621a4e3; D2:$f0d6; D3:$4773; D4:($8a,$9c,$46,$e7,$7b,$17,$48,$40));

  {$EXTERNALSYM SID_CtxQueryAssociations}
  SID_CtxQueryAssociations: TGUID = (
    D1:$faadfc40; D2:$b777; D3:$4b69; D4:($aa,$81,$77,$03,$5e,$f0,$e6,$e8));
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IID_IDocViewSite}
  IID_IDocViewSite: TGUID = (
    D1:$87D605E0; D2:$C511; D3:$11CF; D4:($89,$A9,$00,$A0,$C9,$05,$41,$29));

  {$EXTERNALSYM CLSID_QuickLinks}
  CLSID_QuickLinks: TGUID = (
    D1:$e5cbf21; D2:$d15f; D3:$11d0; D4:($83,$1,$00,$aa,$00,$5b,$43,$83));

// The IShellFolder band
  {$EXTERNALSYM CLSID_ISFBand}
  CLSID_ISFBand: TGUID = (
    D1:$D82BE2B0; D2:$5764; D3:$11D0; D4:($A9,$6E,$00,$C0,$4F,$D7,$05,$A2));

  {$EXTERNALSYM IID_CDefView}
  IID_CDefView: TGUID = (
    D1:$4434FF80; D2:$EF4C; D3:$11CE; D4:($AE,$65,$08,$00,$2B,$2E,$12,$62));

  {$EXTERNALSYM CLSID_ShellFldSetExt}
  CLSID_ShellFldSetExt: TGUID = (
    D1:$6D5313C0; D2:$8C62; D3:$11D1; D4:($B2,$CD,$00,$60,$97,$DF,$8C,$11));

  {$EXTERNALSYM SID_SMenuBandChild}
  SID_SMenuBandChild: TGUID = (
    D1:$ed9cc020; D2:$08b9; D3:$11d1; D4:($98,$23,$00,$c0,$4f,$d9,$19,$72));

  {$EXTERNALSYM SID_SMenuBandParent}
  SID_SMenuBandParent: TGUID = (
    D1:$8c278eec; D2:$3eab; D3:$11d1; D4:($8c,$b0,$00,$c0,$4f,$d9,$18,$d0));

  {$EXTERNALSYM SID_SMenuPopup}
  SID_SMenuPopup: TGUID = (
    D1:$D1E7AFEB; D2:$6A2E; D3:$11D0; D4:($8C,$78,$00,$C0,$4F,$D9,$18,$B4));

  {$EXTERNALSYM SID_SMenuBandBottomSelected}
  SID_SMenuBandBottomSelected: TGUID = (
    D1:$165EBAF4; D2:$6D51; D3:$11D2; D4:($83,$AD,$00,$C0,$4F,$D9,$18,$D0));

  {$EXTERNALSYM SID_SMenuBandBottom}
  SID_SMenuBandBottom: TGUID = (
    D1:$743CA664; D2:$0DEB; D3:$11D1; D4:($98,$25,$00,$C0,$4F,$D9,$19,$72));

  {$EXTERNALSYM SID_MenuShellFolder}
  SID_MenuShellFolder: TGUID = (
    D1:$A6C17EB4; D2:$2D65; D3:$11D2; D4:($83,$8F,$00,$C0,$4F,$D9,$18,$D0));

//Command Group ID for MenuDeskBar
  {$EXTERNALSYM CGID_MENUDESKBAR}
  CGID_MENUDESKBAR: TGUID = (
    D1:$5C9F0A12; D2:$959E; D3:$11D0; D4:($A3,$A4,$00,$A0,$C9,$08,$26,$36));

  {$EXTERNALSYM SID_SMenuBandTop}
  SID_SMenuBandTop: TGUID = (
    D1:$9493A810; D2:$EC38; D3:$11D0; D4:($BC,$46,$00,$AA,$00,$6C,$E2,$F5));

  {$EXTERNALSYM CLSID_MenuToolbarBase}
  CLSID_MenuToolbarBase: TGUID = (
    D1:$40B96610; D2:$B522; D3:$11D1; D4:($B3,$B4,$00,$AA,$00,$6E,$FD,$E7));

  {$EXTERNALSYM IID_IBanneredBar}
  IID_IBanneredBar: TGUID = (
    D1:$596A9A94; D2:$013E; D3:$11D1; D4:($8D,$34,$00,$A0,$C9,$0F,$27,$19));

  {$EXTERNALSYM CLSID_MenuBandSite}
  CLSID_MenuBandSite: TGUID = (
    D1:$E13EF4E4; D2:$D2F2; D3:$11D0; D4:($98,$16,$00,$C0,$4F,$D9,$19,$72));

  {$EXTERNALSYM SID_SCommDlgBrowser}
  SID_SCommDlgBrowser: TGUID = (
    D1:$80F30233; D2:$B7DF; D3:$11D2; D4:($A3,$3B,$00,$60,$97,$DF,$5B,$D4));

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

// -- shtypes.h --

//+-------------------------------------------------------------------------
//
//  Microsoft Windows
//  Copyright (c) Microsoft Corporation. All rights reserved.
//
//--------------------------------------------------------------------------
//===========================================================================
//
// Object identifiers in the explorer's name space (ItemID and IDList)
//
//  All the items that the user can browse with the explorer (such as files,
// directories, servers, work-groups, etc.) has an identifier which is unique
// among items within the parent folder. Those identifiers are called item
// IDs (SHITEMID). Since all its parent folders have their own item IDs,
// any items can be uniquely identified by a list of item IDs, which is called
// an ID list (ITEMIDLIST).
//
//  ID lists are almost always allocated by the task allocator (see some
// description below as well as OLE 2.0 SDK) and may be passed across
// some of shell interfaces (such as IShellFolder). Each item ID in an ID list
// is only meaningful to its parent folder (which has generated it), and all
// the clients must treat it as an opaque binary data except the first two
// bytes, which indicates the size of the item ID.
//
//  When a shell extension -- which implements the IShellFolder interace --
// generates an item ID, it may put any information in it, not only the data
// with that it needs to identifies the item, but also some additional
// information, which would help implementing some other functions efficiently.
// For example, the shell's IShellFolder implementation of file system items
// stores the primary (long) name of a file or a directory as the item
// identifier, but it also stores its alternative (short) name, size and date
// etc.
//
//  When an ID list is passed to one of shell APIs (such as SHGetPathFromIDList),
// it is always an absolute path -- relative from the root of the name space,
// which is the desktop folder. When an ID list is passed to one of IShellFolder
// member function, it is always a relative path from the folder (unless it
// is explicitly specified).
//
//===========================================================================
//
// SHITEMID -- Item ID  (mkid)
//     Word      cb;             // Size of the ID (including cb itself)
//     BYTE        abID[];         // The item ID (variable length)
//

type
  PSHItemID = ^TSHItemID;
  {$EXTERNALSYM _SHITEMID}
  _SHITEMID = packed record
    cb: Word;
    abID: array[0..0] of Byte;
  end;
  {$EXTERNALSYM SHITEMID}
  SHITEMID = _SHITEMID;
  TSHItemID = _SHITEMID;

//
// ITEMIDLIST -- List if item IDs (combined with 0-terminator)
//
  PItemIDList = ^TItemIDList;
  {$EXTERNALSYM _ITEMIDLIST}
  _ITEMIDLIST = packed record
    mkid: TSHItemID;
  end;
  {$EXTERNALSYM ITEMIDLIST}
  ITEMIDLIST = _ITEMIDLIST;
  TItemIDList = _ITEMIDLIST;

  {$NODEFINE PPItemIDList}
  PPItemIDList = ^PItemIDList;
  {$NODEFINE TPItemIDListArray}
  TPItemIDListArray = array[0..65535] of PItemIDList;
  {$NODEFINE PPItemIDListArray}
  PPItemIDListArray = ^TPItemIDListArray;

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

//-------------------------------------------------------------------------
//
// struct STRRET
//
// structure for returning strings from IShellFolder member functions
//
//-------------------------------------------------------------------------
//
//  uType indicate which union member to use 
//    STRRET_WSTR    Use STRRET.pOleStr     must be freed by caller of GetDisplayNameOf
//    STRRET_OFFSET  Use STRRET.uOffset     Offset into SHITEMID for ANSI string 
//    STRRET_CSTR    Use STRRET.cStr        ANSI Buffer
//
  {$EXTERNALSYM tagSTRRET_TYPE}
  tagSTRRET_TYPE = DWORD;
  {$EXTERNALSYM STRRET_TYPE}
  STRRET_TYPE = tagSTRRET_TYPE;
  TStrRetType = tagSTRRET_TYPE;

const
  {$EXTERNALSYM STRRET_WSTR}
  STRRET_WSTR   = $0;
  {$EXTERNALSYM STRRET_OFFSET}
  STRRET_OFFSET = $1;
  {$EXTERNALSYM STRRET_CSTR}
  STRRET_CSTR   = $2;

type
  PStrRet = ^TStrRet;
  {$EXTERNALSYM _STRRET}
  _STRRET = record
    case uType: UINT of
      STRRET_WSTR:
        (pOleStr: PWideChar);
      STRRET_OFFSET:
        (uOffset: UINT);
      STRRET_CSTR:
        (cStr: array[0..259] of AnsiChar);
  end;
  {$EXTERNALSYM STRRET}
  STRRET = _STRRET;
  TStrRet = _STRRET;

//-------------------------------------------------------------------------
//
// struct SHELLDETAILS
//
// structure for returning strings from IShellDetails
//
//-------------------------------------------------------------------------
//
//  fmt;            // LVCFMT_* value (header only)
//  cxChar;         // Number of 'average' characters (header only)
//  str;            // String information
//
  PShellDetails = ^TShellDetails;
  {$EXTERNALSYM _SHELLDETAILS}
  _SHELLDETAILS = packed record
    fmt: Integer;
    cxChar: Integer;
    str: TStrRet;
  end;
  {$EXTERNALSYM SHELLDETAILS}
  SHELLDETAILS = _SHELLDETAILS;
  TShellDetails = _SHELLDETAILS;

// -- shobjidl.h --

  {$EXTERNALSYM IPersistFolder}
  IPersistFolder = interface(IPersist)
  ['{000214EA-0000-0000-C000-000000000046}']
    function Initialize(pidl: PItemIDList): HResult; stdcall;
  end;

  {$EXTERNALSYM IPersistFolder2}
  IPersistFolder2 = interface(IPersistFolder)
  ['{1AC3D9F0-175C-11d1-95BE-00609797EA4F}']
    function GetCurFolder(out ppidl: PItemIDList): HResult; stdcall;
  end;

  {$EXTERNALSYM IPersistIDList}
  IPersistIDList = interface(IPersist)
  ['{1079acfc-29bd-11d3-8e0d-00c04f6837d5}']
    function SetIDList(pidl: PItemIDList): HResult; stdcall;
    function GetIDList(out ppidl: PItemIDList): HResult; stdcall;
  end;

  {$EXTERNALSYM IEnumIDList}
  IEnumIDList = interface(IUnknown)
  ['{000214F2-0000-0000-C000-000000000046}']
    function Next(celt: ULONG; out rgelt: PItemIDList; out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumIDList): HResult; stdcall;
  end;

type
  {$EXTERNALSYM tagSHGDN}
  tagSHGDN = DWORD;
  {$EXTERNALSYM SHGNO}
  SHGNO = tagSHGDN;
  TSHGNO = tagSHGDN;

const
  {$EXTERNALSYM SHGDN_NORMAL}
  SHGDN_NORMAL        = $0000;   // default (display purpose)
  {$EXTERNALSYM SHGDN_INFOLDER}
  SHGDN_INFOLDER      = $0001;   // displayed under a folder (relative)
  {$EXTERNALSYM SHGDN_FOREDITING}
  SHGDN_FOREDITING    = $1000;   // for in-place editing
  {$EXTERNALSYM SHGDN_FORADDRESSBAR}
  SHGDN_FORADDRESSBAR = $4000;   // UI friendly parsing name (remove ugly stuff)
  {$EXTERNALSYM SHGDN_FORPARSING}
  SHGDN_FORPARSING    = $8000;   // parsing name for ParseDisplayName()

type
  {$EXTERNALSYM SHGDNF}
  SHGDNF = DWORD;
  TSHGDNF = DWORD;

  {$EXTERNALSYM tagSHCONTF}
  tagSHCONTF = DWORD;
  {$EXTERNALSYM SHCONTF}
  SHCONTF = DWORD;
  TSHContF = DWORD;

const
  {$EXTERNALSYM SHCONTF_FOLDERS}
  SHCONTF_FOLDERS            = $0020;   // only want folders enumerated (SFGAO_FOLDER)
  {$EXTERNALSYM SHCONTF_NONFOLDERS}
  SHCONTF_NONFOLDERS         = $0040;   // include non folders
  {$EXTERNALSYM SHCONTF_INCLUDEHIDDEN}
  SHCONTF_INCLUDEHIDDEN      = $0080;   // show items normally hidden
  {$EXTERNALSYM SHCONTF_INIT_ON_FIRST_NEXT}
  SHCONTF_INIT_ON_FIRST_NEXT = $0100;   // allow EnumObject() to return before validating enum
  {$EXTERNALSYM SHCONTF_NETPRINTERSRCH}
  SHCONTF_NETPRINTERSRCH     = $0200;   // hint that client is looking for printers
  {$EXTERNALSYM SHCONTF_SHAREABLE}
  SHCONTF_SHAREABLE          = $0400;   // hint that client is looking sharable resources (remote shares)
  {$EXTERNALSYM SHCONTF_STORAGE}
  SHCONTF_STORAGE            = $0800;   // include all items with accessible storage and their ancestors

  {$EXTERNALSYM SHCIDS_ALLFIELDS}
  SHCIDS_ALLFIELDS        = $80000000;
  {$EXTERNALSYM SHCIDS_CANONICALONLY}
  SHCIDS_CANONICALONLY    = $10000000;
  {$EXTERNALSYM SHCIDS_BITMASK}
  SHCIDS_BITMASK          = $FFFF0000;
  {$EXTERNALSYM SHCIDS_COLUMNMASK}
  SHCIDS_COLUMNMASK       = $0000FFFF;
  {$EXTERNALSYM SFGAO_CANCOPY}
  SFGAO_CANCOPY           = DROPEFFECT_COPY; // Objects can be copied    (0x1)
  {$EXTERNALSYM SFGAO_CANMOVE}
  SFGAO_CANMOVE           = DROPEFFECT_MOVE; // Objects can be moved     (0x2)
  {$EXTERNALSYM SFGAO_CANLINK}
  SFGAO_CANLINK           = DROPEFFECT_LINK; // Objects can be linked    (0x4)
  {$EXTERNALSYM SFGAO_STORAGE}
  SFGAO_STORAGE           = $00000008;     // supports BindToObject(IID_IStorage)
  {$EXTERNALSYM SFGAO_CANRENAME}
  SFGAO_CANRENAME         = $00000010;     // Objects can be renamed
  {$EXTERNALSYM SFGAO_CANDELETE}
  SFGAO_CANDELETE         = $00000020;     // Objects can be deleted
  {$EXTERNALSYM SFGAO_HASPROPSHEET}
  SFGAO_HASPROPSHEET      = $00000040;     // Objects have property sheets
  {$EXTERNALSYM SFGAO_DROPTARGET}
  SFGAO_DROPTARGET        = $00000100;     // Objects are drop target
  {$EXTERNALSYM SFGAO_CAPABILITYMASK}
  SFGAO_CAPABILITYMASK    = $00000177;
  {$EXTERNALSYM SFGAO_ENCRYPTED}
  SFGAO_ENCRYPTED         = $00002000;     // object is encrypted (use alt color)
  {$EXTERNALSYM SFGAO_ISSLOW}
  SFGAO_ISSLOW            = $00004000;     // 'slow' object
  {$EXTERNALSYM SFGAO_GHOSTED}
  SFGAO_GHOSTED           = $00008000;     // ghosted icon
  {$EXTERNALSYM SFGAO_LINK}
  SFGAO_LINK              = $00010000;     // Shortcut (link)
  {$EXTERNALSYM SFGAO_SHARE}
  SFGAO_SHARE             = $00020000;     // shared
  {$EXTERNALSYM SFGAO_READONLY}
  SFGAO_READONLY          = $00040000;     // read-only
  {$EXTERNALSYM SFGAO_HIDDEN}
  SFGAO_HIDDEN            = $00080000;     // hidden object
  {$EXTERNALSYM SFGAO_DISPLAYATTRMASK}
  SFGAO_DISPLAYATTRMASK   = $000FC000;
  {$EXTERNALSYM SFGAO_FILESYSANCESTOR}
  SFGAO_FILESYSANCESTOR   = $10000000;     // may contain children with SFGAO_FILESYSTEM
  {$EXTERNALSYM SFGAO_FOLDER}
  SFGAO_FOLDER            = $20000000;     // support BindToObject(IID_IShellFolder)
  {$EXTERNALSYM SFGAO_FILESYSTEM}
  SFGAO_FILESYSTEM        = $40000000;     // is a win32 file system object (file/folder/root)
  {$EXTERNALSYM SFGAO_HASSUBFOLDER}
  SFGAO_HASSUBFOLDER      = $80000000;     // may contain children with SFGAO_FOLDER
  {$EXTERNALSYM SFGAO_CONTENTSMASK}
  SFGAO_CONTENTSMASK      = $80000000;
  {$EXTERNALSYM SFGAO_VALIDATE}
  SFGAO_VALIDATE          = $01000000;     // invalidate cached information
  {$EXTERNALSYM SFGAO_REMOVABLE}
  SFGAO_REMOVABLE         = $02000000;     // is this removeable media?
  {$EXTERNALSYM SFGAO_COMPRESSED}
  SFGAO_COMPRESSED        = $04000000;     // Object is compressed (use alt color)
  {$EXTERNALSYM SFGAO_BROWSABLE}
  SFGAO_BROWSABLE         = $08000000;     // supports IShellFolder, but only implements CreateViewObject() (non-folder view)
  {$EXTERNALSYM SFGAO_NONENUMERATED}
  SFGAO_NONENUMERATED     = $00100000;     // is a non-enumerated object
  {$EXTERNALSYM SFGAO_NEWCONTENT}
  SFGAO_NEWCONTENT        = $00200000;     // should show bold in explorer tree
  {$EXTERNALSYM SFGAO_CANMONIKER}
  SFGAO_CANMONIKER        = $00400000;     // defunct
  {$EXTERNALSYM SFGAO_HASSTORAGE}
  SFGAO_HASSTORAGE        = $00400000;     // defunct
  {$EXTERNALSYM SFGAO_STREAM}
  SFGAO_STREAM            = $00400000;     // supports BindToObject(IID_IStream)
  {$EXTERNALSYM SFGAO_STORAGEANCESTOR}
  SFGAO_STORAGEANCESTOR   = $00800000;     // may contain children with SFGAO_STORAGE or SFGAO_STREAM
  {$EXTERNALSYM SFGAO_STORAGECAPMASK}
  SFGAO_STORAGECAPMASK    = $70C50008;     // for determining storage capabilities, ie for open/save semantics

type
  {$EXTERNALSYM SFGAOF}
  SFGAOF = ULONG;
  TSFGAOF = ULONG;

const
  {$EXTERNALSYM STR_SKIP_BINDING_CLSID}
  STR_SKIP_BINDING_CLSID                   = 'Skip Binding CLSID';
  {$EXTERNALSYM STR_PARSE_PREFER_FOLDER_BROWSING}
  STR_PARSE_PREFER_FOLDER_BROWSING         = 'Parse Prefer Folder Browsing';
  {$EXTERNALSYM STR_DONT_PARSE_RELATIVE}
  STR_DONT_PARSE_RELATIVE                  = 'Don''t Parse Relative';
  {$EXTERNALSYM STR_PARSE_TRANSLATE_ALIASES}
  STR_PARSE_TRANSLATE_ALIASES              = 'Parse Translate Aliases';
  {$EXTERNALSYM STR_PARSE_SHELL_PROTOCOL_TO_FILE_OBJECTS}
  STR_PARSE_SHELL_PROTOCOL_TO_FILE_OBJECTS = 'Parse Shell Protocol To File Objects';

type
  {$EXTERNALSYM IShellFolder}
  IShellFolder = interface(IUnknown)
  ['{000214E6-0000-0000-C000-000000000046}']
    function ParseDisplayName(hwnd: HWND; pbc: IBindCtx;
      var pszDisplayName: POleStr; out pchEaten: ULONG; out ppidl: PItemIDList;
      var pdwAttributes: ULONG): HResult; stdcall;
    function EnumObjects(hwnd: HWND; grfFlags: TSHContF;
      out ppenumIDList: IEnumIDList): HResult; stdcall;
    function BindToObject(pidl: PItemIDList; pbc: IBindCtx;  const riid: TIID;
      out ppv): HResult; stdcall;
    function BindToStorage(pidl: PItemIDList; pbc: IBindCtx; const riid: TIID;
      out ppv): HResult; stdcall;
    function CompareIDs(ram: LPARAM;
      pidl1, pidl2: PItemIDList): HResult; stdcall;
    function CreateViewObject(hwndOwner: HWND; const riid: TIID;
      out ppv): HResult; stdcall;
    function GetAttributesOf(cidl: UINT; var apidl: PItemIDList;
      var rgfInOut: TSFGAOF): HResult; stdcall;
    function GetUIObjectOf(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList;
      const riid: TIID; rffReserved: PUINT;
      out ppv): HResult; stdcall;
    function GetDisplayNameOf(pidl: PItemIDList; uFlags: TSHGDNF;
      out pName: TStrRet): HResult; stdcall;
    function SetNameOf(hwnd: HWND; pidl: PItemIDList; pszName: POleStr;
      uFlags: TSHGDNF; out ppidlOut: PItemIDList): HResult; stdcall;
  end;

  PExtraSearch = ^TExtraSearch;
  {$EXTERNALSYM tagEXTRASEARCH}
  tagEXTRASEARCH = record
    guidSearch: TGUID;
    wszFriendlyName: array[0..79] of WideChar;
    wszUrl: array[0..2083] of WideChar;
  end;
  {$EXTERNALSYM EXTRASEARCH}
  EXTRASEARCH = tagEXTRASEARCH;
  TExtraSearch = tagEXTRASEARCH;

  {$EXTERNALSYM IEnumExtraSearch}
  IEnumExtraSearch = interface(IUnknown)
  ['{0E700BE1-9DB6-11d1-A1CE-00C04FD75D13}']
    function Next(celt: ULONG; out rgelt: TExtraSearch;
      out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumExtraSearch): HResult; stdcall;
  end;

type
  {$EXTERNALSYM SHCOLSTATE}
  SHCOLSTATE = DWORD;
  TSHColState = DWORD;

const
  {$EXTERNALSYM SHCOLSTATE_TYPE_STR}
  SHCOLSTATE_TYPE_STR      = $01;
  {$EXTERNALSYM SHCOLSTATE_TYPE_INT}
  SHCOLSTATE_TYPE_INT      = $02;
  {$EXTERNALSYM SHCOLSTATE_TYPE_DATE}
  SHCOLSTATE_TYPE_DATE     = $03;
  {$EXTERNALSYM SHCOLSTATE_TYPEMASK}
  SHCOLSTATE_TYPEMASK      = $0F;
  {$EXTERNALSYM SHCOLSTATE_ONBYDEFAULT}
  SHCOLSTATE_ONBYDEFAULT   = $10;
  {$EXTERNALSYM SHCOLSTATE_SLOW}
  SHCOLSTATE_SLOW          = $20;
  {$EXTERNALSYM SHCOLSTATE_EXTENDED}
  SHCOLSTATE_EXTENDED      = $40;
  {$EXTERNALSYM SHCOLSTATE_SECONDARYUI}
  SHCOLSTATE_SECONDARYUI   = $80;
  {$EXTERNALSYM SHCOLSTATE_HIDDEN}
  SHCOLSTATE_HIDDEN        = $100;
  {$EXTERNALSYM SHCOLSTATE_PREFER_VARCMP}
  SHCOLSTATE_PREFER_VARCMP = $200;

type
  {$EXTERNALSYM SHCOLSTATEF}
  SHCOLSTATEF = DWORD;
  TSHColStateF = DWORD;

type
  PSHColumnID = ^TSHColumnID;
  {$EXTERNALSYM SHCOLUMNID}
  SHCOLUMNID = record
    fmtid: TGUID;
    pid: DWORD;
  end;
  TSHColumnID = SHCOLUMNID;

  {$EXTERNALSYM IShellFolder2}
  IShellFolder2 = interface(IShellFolder)
  ['{93F2F68C-1D1B-11d3-A30E-00C04F79ABD1}']
    function GetDefaultSearchGUID(out pguid: TGUID): HResult; stdcall;
    function EnumSearches(out ppenum: IEnumExtraSearch): HResult; stdcall;
    function GetDefaultColumn(dwRes: DWORD;
      out pSort, pDisplay: ULONG): HResult; stdcall;
    function GetDefaultColumnState(iColumn: UINT;
      out pcsFlags: TSHColStateF): HResult; stdcall;
    function GetDetailsEx(pidl: PItemIDList; const pscid: TSHColumnID;
      pv: POleVariant): HResult; stdcall;
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      out psd: TShellDetails): HResult; stdcall;
    function MapColumnToSCID(iColumn: UINT;
      const pscid: TSHColumnID): HResult; stdcall;
  end;

  {$NODEFINE PViewSettings}
  PViewSettings = PAnsiChar;

  {$EXTERNALSYM FOLDERFLAGS}
  FOLDERFLAGS = DWORD;
  TFolderFlags = DWORD;

const
  {$EXTERNALSYM FWF_AUTOARRANGE}
  FWF_AUTOARRANGE         = $1;
  {$EXTERNALSYM FWF_ABBREVIATEDNAMES}
  FWF_ABBREVIATEDNAMES    = $2;
  {$EXTERNALSYM FWF_SNAPTOGRID}
  FWF_SNAPTOGRID          = $4;
  {$EXTERNALSYM FWF_OWNERDATA}
  FWF_OWNERDATA           = $8;
  {$EXTERNALSYM FWF_BESTFITWINDOW}
  FWF_BESTFITWINDOW       = $10;
  {$EXTERNALSYM FWF_DESKTOP}
  FWF_DESKTOP             = $20;
  {$EXTERNALSYM FWF_SINGLESEL}
  FWF_SINGLESEL           = $40;
  {$EXTERNALSYM FWF_NOSUBFOLDERS}
  FWF_NOSUBFOLDERS        = $80;
  {$EXTERNALSYM FWF_TRANSPARENT}
  FWF_TRANSPARENT         = $100;
  {$EXTERNALSYM FWF_NOCLIENTEDGE}
  FWF_NOCLIENTEDGE        = $200;
  {$EXTERNALSYM FWF_NOSCROLL}
  FWF_NOSCROLL            = $400;
  {$EXTERNALSYM FWF_ALIGNLEFT}
  FWF_ALIGNLEFT           = $800;
  {$EXTERNALSYM FWF_NOICONS}
  FWF_NOICONS             = $1000;
  {$EXTERNALSYM FWF_SHOWSELALWAYS}
  FWF_SHOWSELALWAYS       = $2000;
  {$EXTERNALSYM FWF_NOVISIBLE}
  FWF_NOVISIBLE           = $4000;
  {$EXTERNALSYM FWF_SINGLECLICKACTIVATE}
  FWF_SINGLECLICKACTIVATE = $8000;
  {$EXTERNALSYM FWF_NOWEBVIEW}
  FWF_NOWEBVIEW           = $10000;
  {$EXTERNALSYM FWF_HIDEFILENAMES}
  FWF_HIDEFILENAMES       = $20000;
  {$EXTERNALSYM FWF_CHECKSELECT}
  FWF_CHECKSELECT         = $40000;

type
  {$EXTERNALSYM FOLDERVIEWMODE}
  FOLDERVIEWMODE = DWORD;
  TFolderViewMode = DWORD;

const
  {$EXTERNALSYM FVM_FIRST}
  FVM_FIRST      = 1;
  {$EXTERNALSYM FVM_ICON}
  FVM_ICON       = 1;
  {$EXTERNALSYM FVM_SMALLICON}
  FVM_SMALLICON  = 2;
  {$EXTERNALSYM FVM_LIST}
  FVM_LIST       = 3;
  {$EXTERNALSYM FVM_DETAILS}
  FVM_DETAILS    = 4;
  {$EXTERNALSYM FVM_THUMBNAIL}
  FVM_THUMBNAIL  = 5;
  {$EXTERNALSYM FVM_TILE}
  FVM_TILE       = 6;
  {$EXTERNALSYM FVM_THUMBSTRIP}
  FVM_THUMBSTRIP = 7;
  {$EXTERNALSYM FVM_LAST}
  FVM_LAST       = 7;

type
  PFolderSettings = ^TFolderSettings;
  {$EXTERNALSYM FOLDERSETTINGS}
  FOLDERSETTINGS = record
    ViewMode: UINT;
    fFlags: UINT;
  end;
  TFolderSettings = FOLDERSETTINGS;

const
  {$EXTERNALSYM SVSI_DESELECT}
  SVSI_DESELECT       = $00000000;
  {$EXTERNALSYM SVSI_SELECT}
  SVSI_SELECT         = $00000001;
  {$EXTERNALSYM SVSI_EDIT}
  SVSI_EDIT           = $00000003;  // includes select
  {$EXTERNALSYM SVSI_DESELECTOTHERS}
  SVSI_DESELECTOTHERS = $00000004;
  {$EXTERNALSYM SVSI_ENSUREVISIBLE}
  SVSI_ENSUREVISIBLE  = $00000008;
  {$EXTERNALSYM SVSI_FOCUSED}
  SVSI_FOCUSED        = $00000010;
  {$EXTERNALSYM SVSI_TRANSLATEPT}
  SVSI_TRANSLATEPT    = $00000020;
  {$EXTERNALSYM SVSI_SELECTIONMARK}
  SVSI_SELECTIONMARK  = $00000040;
  {$EXTERNALSYM SVSI_POSITIONITEM}
  SVSI_POSITIONITEM   = $00000080;
  {$EXTERNALSYM SVSI_CHECK}
  SVSI_CHECK          = $00000100;
  {$EXTERNALSYM SVSI_NOSTATECHANGE}
  SVSI_NOSTATECHANGE  = $80000000;

type
  {$EXTERNALSYM SVSIF}
  SVSIF = UINT;
  TSVSIF = UINT;

  {$EXTERNALSYM SVGIO}
  SVGIO = DWORD;
  TSVGIO = DWORD;

const
  {$EXTERNALSYM SVGIO_BACKGROUND}
  SVGIO_BACKGROUND     = 0;
  {$EXTERNALSYM SVGIO_SELECTION}
  SVGIO_SELECTION      = $1;
  {$EXTERNALSYM SVGIO_ALLVIEW}
  SVGIO_ALLVIEW        = $2;
  {$EXTERNALSYM SVGIO_CHECKED}
  SVGIO_CHECKED        = $3;
  {$EXTERNALSYM SVGIO_TYPE_MASK}
  SVGIO_TYPE_MASK      = $f;
  {$EXTERNALSYM SVGIO_FLAG_VIEWORDER}
  SVGIO_FLAG_VIEWORDER = $80000000;

type
  {$EXTERNALSYM SVUIA_STATUS}
  SVUIA_STATUS = DWORD;
  TSVUIAStatus = DWORD;

const
  {$EXTERNALSYM SVUIA_DEACTIVATE}
  SVUIA_DEACTIVATE       = 0;
  {$EXTERNALSYM SVUIA_ACTIVATE_NOFOCUS}
  SVUIA_ACTIVATE_NOFOCUS = 1;
  {$EXTERNALSYM SVUIA_ACTIVATE_FOCUS}
  SVUIA_ACTIVATE_FOCUS   = 2;
  {$EXTERNALSYM SVUIA_INPLACEACTIVATE}
  SVUIA_INPLACEACTIVATE  = 3;

type
  {$EXTERNALSYM LPFNSVADDPROPSHEETPAGE}
  LPFNSVADDPROPSHEETPAGE = LPFNADDPROPSHEETPAGE;
  TFNVSAddPropSheetPage = LPFNADDPROPSHEETPAGE;

{ -------------------------------------------------------------------------- }

{ Interface:   IShellBrowser }

{  IShellBrowser interface is the interface that is provided by the shell }
{ explorer/folder frame window. When it creates the "contents pane" of }
{ a shell folder (which provides IShellFolder interface), it calls its }
{ CreateViewObject member function to create an IShellView object. Then, }
{ it calls its CreateViewWindow member to create the "contents pane" }
{ window. The pointer to the IShellBrowser interface is passed to }
{ the IShellView object as a parameter to this CreateViewWindow member }
{ function call. }

{    +--------------------------+  <-- Explorer window }
{    | [] Explorer              | }
{    |--------------------------+       IShellBrowser }
{    | File Edit View ..        | }
{    |--------------------------| }
{    |        |                 | }
{    |        |              <-------- Content pane }
{    |        |                 | }
{    |        |                 |       IShellView }
{    |        |                 | }
{    |        |                 | }
{    +--------------------------+ }



{ [Member functions] }


{ IShellBrowser.GetWindow(phwnd) }

{   Inherited from IOleWindow.GetWindow. }


{ IShellBrowser.ContextSensitiveHelp(fEnterMode) }

{   Inherited from IOleWindow.ContextSensitiveHelp. }


{ IShellBrowser.InsertMenusSB(hmenuShared, lpMenuWidths) }

{   Similar to the IOleInPlaceFrame.InsertMenus. The explorer will put }
{  "File" and "Edit" pulldown in the File menu group, "View" and "Tools" }
{  in the Container menu group and "Help" in the Window menu group. Each }
{  pulldown menu will have a uniqu ID, FCIDM_MENU_FILE/EDIT/VIEW/TOOLS/HELP. }
{  The view is allowed to insert menuitems into those sub-menus by those }
{  IDs must be between FCIDM_SHVIEWFIRST and FCIDM_SHVIEWLAST. }


{ IShellBrowser.SetMenuSB(hmenuShared, holemenu, hwndActiveObject) }

{   Similar to the IOleInPlaceFrame.SetMenu. The explorer ignores the }
{  holemenu parameter (reserved for future enhancement)  and performs }
{  menu-dispatch based on the menuitem IDs (see the description above). }
{  It is important to note that the explorer will add different }
{  set of menuitems depending on whether the view has a focus or not. }
{  Therefore, it is very important to call ISB.OnViewWindowActivate }
{  whenever the view window (or its children) gets the focus. }


{ IShellBrowser.RemoveMenusSB(hmenuShared) }

{   Same as the IOleInPlaceFrame.RemoveMenus. }


{ IShellBrowser.SetStatusTextSB(lpszStatusText) }

{   Same as the IOleInPlaceFrame.SetStatusText. It is also possible to }
{  send messages directly to the status window via SendControlMsg. }


{ IShellBrowser.EnableModelessSB(fEnable) }

{   Same as the IOleInPlaceFrame.EnableModeless. }

{ IShellBrowser.TranslateAcceleratorSB(lpmsg, wID) }

{   Same as the IOleInPlaceFrame.TranslateAccelerator, but will be }
{  never called because we don't support EXEs (i.e., the explorer has }
{  the message loop). This member function is defined here for possible }
{  future enhancement. }


{ IShellBrowser.BrowseObject(pidl, wFlags) }

{   The view calls this member to let shell explorer browse to another }
{  folder. The pidl and wFlags specifies the folder to be browsed. }

{  Following three flags specifies whether it creates another window or not. }
{   SBSP_SAMEBROWSER  -- Browse to another folder with the same window. }
{   SBSP_NEWBROWSER   -- Creates another window for the specified folder. }
{   SBSP_DEFBROWSER   -- Default behavior (respects the view option). }

{  Following three flags specifies open, explore, or default mode. These   . }
{  are ignored if SBSP_SAMEBROWSER or (SBSP_DEFBROWSER && (single window   . }
{  browser || explorer)).                                                  . }
{   SBSP_OPENMODE     -- Use a normal folder window }
{   SBSP_EXPLOREMODE  -- Use an explorer window }
{   SBSP_DEFMODE      -- Use the same as the current window }

{  Following three flags specifies the pidl. }
{   SBSP_ABSOLUTE -- pidl is an absolute pidl (relative from desktop) }
{   SBSP_RELATIVE -- pidl is relative from the current folder. }
{   SBSP_PARENT   -- Browse the parent folder (ignores the pidl) }
{   SBSP_NAVIGATEBACK    -- Navigate back (ignores the pidl) }
{   SBSP_NAVIGATEFORWARD -- Navigate forward (ignores the pidl) }

{  Following two flags control history manipulation as result of navigate }
{   SBSP_WRITENOHISTORY -- write no history (shell folder) entry }
{   SBSP_NOAUTOSELECT -- suppress selection in history pane }


{ IShellBrowser.GetViewStateStream(grfMode, ppstm) }

{   The browser returns an IStream interface as the storage for view }
{  specific state information. }

{   grfMode -- Specifies the read/write access (STGM_READ/WRITE/READWRITE) }
{   ppstm   -- Specifies the LPSTREAM variable to be filled. }


{ IShellBrowser.GetControlWindow(id, phwnd) }

{   The shell view may call this member function to get the window handle }
{  of Explorer controls (toolbar or status winodw -- FCW_TOOLBAR or }
{  FCW_STATUS). }


{ IShellBrowser.SendControlMsg(id, uMsg, wParam, lParam, pret) }

{   The shell view calls this member function to send control messages to }
{  one of Explorer controls (toolbar or status window -- FCW_TOOLBAR or }
{  FCW_STATUS). }


{ IShellBrowser.QueryActiveShellView(IShellView * ppshv) }

{   This member returns currently activated (displayed) shellview object. }
{  A shellview never need to call this member function. }


{ IShellBrowser.OnViewWindowActive(pshv) }

{   The shell view window calls this member function when the view window }
{  (or one of its children) got the focus. It MUST call this member before }
{  calling IShellBrowser.InsertMenus, because it will insert different }
{  set of menu items depending on whether the view has the focus or not. }


{ IShellBrowser.SetToolbarItems(lpButtons, nButtons, uFlags) }

{   The view calls this function to add toolbar items to the exporer's }
{  toolbar. "lpButtons" and "nButtons" specifies the array of toolbar }
{  items. "uFlags" must be one of FCT_MERGE, FCT_CONFIGABLE, FCT_ADDTOEND. }

{ ------------------------------------------------------------------------- }

const
  {$EXTERNALSYM SBSP_DEFBROWSER}
  SBSP_DEFBROWSER             = $0000;
  {$EXTERNALSYM SBSP_SAMEBROWSER}
  SBSP_SAMEBROWSER            = $0001;
  {$EXTERNALSYM SBSP_NEWBROWSER}
  SBSP_NEWBROWSER             = $0002;
  {$EXTERNALSYM SBSP_DEFMODE}
  SBSP_DEFMODE                = $0000;
  {$EXTERNALSYM SBSP_OPENMODE}
  SBSP_OPENMODE               = $0010;
  {$EXTERNALSYM SBSP_EXPLOREMODE}
  SBSP_EXPLOREMODE            = $0020;
  {$EXTERNALSYM SBSP_HELPMODE}
  SBSP_HELPMODE               = $0040; // IEUNIX : Help window uses this.
  {$EXTERNALSYM SBSP_NOTRANSFERHIST}
  SBSP_NOTRANSFERHIST         = $0080;
  {$EXTERNALSYM SBSP_ABSOLUTE}
  SBSP_ABSOLUTE               = $0000;
  {$EXTERNALSYM SBSP_RELATIVE}
  SBSP_RELATIVE               = $1000;
  {$EXTERNALSYM SBSP_PARENT}
  SBSP_PARENT                 = $2000;
  {$EXTERNALSYM SBSP_NAVIGATEBACK}
  SBSP_NAVIGATEBACK           = $4000;
  {$EXTERNALSYM SBSP_NAVIGATEFORWARD}
  SBSP_NAVIGATEFORWARD        = $8000;
  {$EXTERNALSYM SBSP_ALLOW_AUTONAVIGATE}
  SBSP_ALLOW_AUTONAVIGATE     = $10000;
  {$EXTERNALSYM SBSP_CALLERUNTRUSTED}
  SBSP_CALLERUNTRUSTED        = $00800000;
  {$EXTERNALSYM SBSP_TRUSTFIRSTDOWNLOAD}
  SBSP_TRUSTFIRSTDOWNLOAD     = $01000000;
  {$EXTERNALSYM SBSP_UNTRUSTEDFORDOWNLOAD}
  SBSP_UNTRUSTEDFORDOWNLOAD   = $02000000;
  {$EXTERNALSYM SBSP_NOAUTOSELECT}
  SBSP_NOAUTOSELECT           = $04000000;
  {$EXTERNALSYM SBSP_WRITENOHISTORY}
  SBSP_WRITENOHISTORY         = $08000000;
  {$EXTERNALSYM SBSP_TRUSTEDFORACTIVEX}
  SBSP_TRUSTEDFORACTIVEX      = $10000000;
  {$EXTERNALSYM SBSP_REDIRECT}
  SBSP_REDIRECT               = $40000000;
  {$EXTERNALSYM SBSP_INITIATEDBYHLINKFRAME}
  SBSP_INITIATEDBYHLINKFRAME  = $80000000;

  {$EXTERNALSYM FCW_STATUS}
  FCW_STATUS      = $0001;
  {$EXTERNALSYM FCW_TOOLBAR}
  FCW_TOOLBAR     = $0002;
  {$EXTERNALSYM FCW_TREE}
  FCW_TREE        = $0003;
  {$EXTERNALSYM FCW_INTERNETBAR}
  FCW_INTERNETBAR = $0006;
  {$EXTERNALSYM FCW_PROGRESS}
  FCW_PROGRESS    = $0008;
  {$EXTERNALSYM FCT_MERGE}
  FCT_MERGE       = $0001;
  {$EXTERNALSYM FCT_CONFIGABLE}
  FCT_CONFIGABLE  = $0002;
  {$EXTERNALSYM FCT_ADDTOEND}
  FCT_ADDTOEND    = $0004;

type
  TTBButtonSB = TTBButton;
  PTBButtonSB = PTBButton;

  {$EXTERNALSYM HOLEMENU}
  HOLEMENU = HGLOBAL;

  IShellView = interface;

  {$EXTERNALSYM IShellBrowser}
  IShellBrowser = interface(IOleWindow)
  ['{000214E2-0000-0000-C000-000000000046}']
    function InsertMenusSB(hmenuShared: HMENU;
      var lpMenuWidths: TOLEMENUGROUPWIDTHS): HResult; stdcall;
    function SetMenuSB(hmenuShared: HMENU; holemenuRes: HOLEMENU;
      hwndActiveObject: HWND): HResult; stdcall;
    function RemoveMenusSB(hmenuShared: HMENU): HResult; stdcall;
    function SetStatusTextSB(pszStatusText: POleStr): HResult; stdcall;
    function EnableModelessSB(fEnable: BOOL): HResult; stdcall;
    function TranslateAcceleratorSB(pmsg: PMSG;
      wID: Word): HResult; stdcall;
    function BrowseObject(pidl: PItemIDList; wFlags: UINT): HResult; stdcall;
    function GetViewStateStream(grfMode: DWORD;
      out ppStrm: IStream): HResult; stdcall;
    function GetControlWindow(id: UINT; out phwnd: HWND): HResult; stdcall;
    function SendControlMsg(id, uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
      var pret: LRESULT): HResult; stdcall;
    function QueryActiveShellView(out ppshv: IShellView): HResult; stdcall;
    function OnViewWindowActive(pshv: IShellView): HResult; stdcall;
    function SetToolbarItems(lpButtons: PTBButtonSB;
      nButtons, uFlags: UINT): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellView}
  IShellView = interface(IOleWindow)
  ['{000214E3-0000-0000-C000-000000000046}']
    function TranslateAccelerator(pmsg: PMSG): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
    function UIActivate(uState: UINT): HResult; stdcall;
    function Refresh: HResult; stdcall;
    function CreateViewWindow(psvPrevious: IShellView;
      var pfs: TFolderSettings; psb: IShellBrowser;
      out prcView: TRect; out phWnd: HWND): HResult; stdcall;
    function DestroyViewWindow: HResult; stdcall;
    function GetCurrentInfo(out pfs: TFolderSettings): HResult; stdcall;
    function AddPropertySheetPages(dwReserved: DWORD;
      pfn: TFNVSAddPropSheetPage; lparam: LPARAM): HResult; stdcall;
    function SaveViewState: HResult; stdcall;
    function SelectItem(pidlItem: PItemIDList;
      uFlags: SVSIF): HResult; stdcall;
    function GetItemObject(uItem: UINT; const riid: TIID;
      out ppv): HResult; stdcall;
  end;

  PShellViewID = PGUID;
  {$EXTERNALSYM SHELLVIEWID}
  SHELLVIEWID = TGUID;
  TShellViewID = TGUID;

const
  {$EXTERNALSYM SV2GV_CURRENTVIEW}
  SV2GV_CURRENTVIEW = UINT(-1);
  {$EXTERNALSYM SV2GV_DEFAULTVIEW}
  SV2GV_DEFAULTVIEW = UINT(-2);

type
  PSV2CVW2Params = ^TSV2CVW2Params;
  {$EXTERNALSYM _SV2CVW2_PARAMS}
  _SV2CVW2_PARAMS = record
    cbSize: DWORD;
    psvPrev: IShellView;
    pfs: PFolderSettings;
    psbOwner: IShellBrowser;
    prcView: PRect;
    pvid: PShellViewID;
    hwndView: HWND;
  end;
  {$EXTERNALSYM SV2CVW2_PARAMS}
  SV2CVW2_PARAMS = _SV2CVW2_PARAMS;
  TSV2CVW2Params = _SV2CVW2_PARAMS;

  {$EXTERNALSYM IShellView2}
  IShellView2 = interface(IShellView)
  ['{88E39E80-3578-11CF-AE69-08002B2E1262}']
    function GetView(pvid: PShellViewID; uView: ULONG): HResult; stdcall;
    function CreateViewWindow2(var lpParams: TSV2CVW2Params): HResult; stdcall;
    function HandleRename(pidlNew: PItemIDList): HResult; stdcall;
    function SelectAndPositionItem(pidlItem: PItemIDList;
      uFlags: UINT; var ppt: TPoint): HResult; stdcall;
  end;

  {$EXTERNALSYM IFolderView}
  IFolderView = interface(IUnknown)
  ['{cde725b0-ccc9-4519-917e-325d72fab4ce}']
    function GetCurrentViewMode(var pViewMode: UINT): HResult; stdcall;
    function SetCurrentViewMode(ViewMode: UINT): HResult; stdcall;
    function GetFolder(const riid: TIID; out ppv): HResult; stdcall;
    function Item(iItemIndex: Integer;
      out ppidl: PItemIDList): HResult; stdcall;
    function ItemCount(uFlags: UINT; out pcItems: Integer): HResult; stdcall;
    function Items(uFlags: UINT; const riid: TIID; out ppv): HResult; stdcall;
    function GetSelectionMarkedItem(out piItem: Integer): HResult; stdcall;
    function GetFocusedItem(out piItem: Integer): HResult; stdcall;
    function GetItemPosition(pidl: PItemIDList;
      out ppt: TPoint): HResult; stdcall;
    function GetSpacing(var ppt: TPoint): HResult; stdcall;
    function GetDefaultSpacing(out ppt: TPoint): HResult; stdcall;
    function GetAutoArrange: HResult; stdcall;
    function SelectItem(iItem: Integer; dwFlags: DWORD): HResult; stdcall;
    function SelectAndPositionItems(cidl: UINT; var apidl: PItemIDList;
      var apt: TPoint; dwFlags: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IFolderView}
  IID_IFolderView: TGUID = '{CDE725B0-CCC9-4519-917E-325D72FAB4CE}';
  {$EXTERNALSYM SID_SFolderView}
  SID_SFolderView: TGUID = '{CDE725B0-CCC9-4519-917E-325D72FAB4CE}';

  {$EXTERNALSYM IID_IFolderFilterSite}
  IID_IFolderFilterSite: TGUID = '{C0A651F5-B48B-11d2-B5ED-006097C686F6}';

type
  {$EXTERNALSYM IFolderFilterSite}
  IFolderFilterSite = interface(IUnknown)
  ['{C0A651F5-B48B-11d2-B5ED-006097C686F6}']
    function SetFilter(punk: IUnknown): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IFolderFilter}
  IID_IFolderFilter: TGUID = '{9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E}';

type
  {$EXTERNALSYM IFolderFilter}
  IFolderFilter = interface(IUnknown)
  ['{9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E}']
    function ShouldShow(psf: IShellFolder;
      pidlFolder, pidlItem: PItemIDList): HResult; stdcall;
    function GetEnumFlags(psf: IShellFolder; pidlFolder: PItemIDList;
      var phwnd: HWND; out pgrfFlags: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IProfferService}
  IID_IProfferService: TGUID = '{CB728B20-F786-11CE-92AD-00AA00A74CD0}';
  // nearest service that you can proffer to
  {$EXTERNALSYM SID_SProfferService}
  SID_SProfferService: TGUID = '{CB728B20-F786-11CE-92AD-00AA00A74CD0}';

type
  {$EXTERNALSYM IProfferService}
  IProfferService = interface(IUnknown)
  ['{CB728B20-F786-11CE-92AD-00AA00A74CD0}']
    function ProfferService(const rguidService: TGUID; psp: IServiceProvider;
      out pdwCookie: DWORD): HResult; stdcall;
    function RevokeService(dwCookie: DWORD): HResult; stdcall;
  end;

type
  {$EXTERNALSYM PROPERTYUI_NAME_FLAGS}
  PROPERTYUI_NAME_FLAGS = UINT;
  TPropertyUINameFlags = UINT;

const
  {$EXTERNALSYM PUIFNF_DEFAULT}
  PUIFNF_DEFAULT  = $0;
  {$EXTERNALSYM PUIFNF_MNEMONIC}
  PUIFNF_MNEMONIC = $1;

type
  {$EXTERNALSYM PROPERTYUI_FLAGS}
  PROPERTYUI_FLAGS = UINT;
  TPropertyUIFlags = UINT;

const
  {$EXTERNALSYM PUIF_DEFAULT}
  PUIF_DEFAULT          = $0;
  {$EXTERNALSYM PUIF_RIGHTALIGN}
  PUIF_RIGHTALIGN       = $1;
  {$EXTERNALSYM PUIF_NOLABELININFOTIP}
  PUIF_NOLABELININFOTIP = $2;

type
  {$EXTERNALSYM PROPERTYUI_FORMAT_FLAGS}
  PROPERTYUI_FORMAT_FLAGS = UINT;
  TPropertyUIFormatFlags = UINT;

const
  {$EXTERNALSYM PUIFFDF_DEFAULT}
  PUIFFDF_DEFAULT      = $0;
  {$EXTERNALSYM PUIFFDF_RIGHTTOLEFT}
  PUIFFDF_RIGHTTOLEFT  = $1;
  {$EXTERNALSYM PUIFFDF_SHORTFORMAT}
  PUIFFDF_SHORTFORMAT  = $2;
  {$EXTERNALSYM PUIFFDF_NOTIME}
  PUIFFDF_NOTIME       = $4;
  {$EXTERNALSYM PUIFFDF_FRIENDLYDATE}
  PUIFFDF_FRIENDLYDATE = $8;
  {$EXTERNALSYM PUIFFDF_NOUNITS}
  PUIFFDF_NOUNITS      = $10;

  {$EXTERNALSYM IID_IPropertyUI}
  IID_IPropertyUI: TIID = '{757A7D9F-919A-4118-99D7-DBB208C8CC66}';

type
  {$EXTERNALSYM IPropertyUI}
  IPropertyUI = interface(IUnknown)
  ['{757A7D9F-919A-4118-99D7-DBB208C8CC66}']
    function ParsePropertyName(pszName: PWideChar; out pfmtid: TFmtID;
      out ppid: TPropID; var pchEaten: ULONG): HResult; stdcall;
    function GetCannonicalName(fmtid: PFmtID; pid: TPropID;
      pwszText: PWideChar; cchText: DWORD): HResult; stdcall;
    function GetDisplayName(fmtid: PFmtID; pid: TPropID;
      flags: TPropertyUINameFlags; pwszText: PWideChar;
      cchText: DWORD): HResult; stdcall;
    function GetPropertyDescription(fmtid: PFmtID; pid: TPropID;
      pwszText: PWideChar; cchText: DWORD): HResult; stdcall;
    function GetDefaultWidth(fmtid: PFmtID; pid: TPropID;
      out pcxChars: ULONG): HResult; stdcall;
    function GetFlags(fmtid: PFmtID; pid: TPropID;
      out pFlags: TPropertyUIFlags): HResult; stdcall;
    function FormatForDisplay(fmtid: PFmtID; pid: TPropID; pvar: PPropVariant;
      flags: TPropertyUIFormatFlags; pwszText: PWideChar;
      cchText: DWORD): HResult; stdcall;
    function GetHelpInfo(fmtid: PFmtID; pid: TPropID; pwszHelpFile: PWideChar;
      cch: DWORD; out puHelpID: UINT): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_ICategoryProvider}
  IID_ICategoryProvider: TGUID = '{9AF64809-5864-4C26-A720-C1F78C086EE3}';

type
  {$EXTERNALSYM ICategoryProvider}
  ICategoryProvider = interface(IUnknown)
  ['{9AF64809-5864-4C26-A720-C1F78C086EE3}']
    function CanCategorizeOnSCID(const pscid: TSHColumnID): HResult; stdcall;
    function GetDefaultCategory(pguid: PGUID;
      out pscid: TSHColumnID): HResult; stdcall;
    function GetCategoryForSCID(const pscid: TSHCOLUMNID;
      out pguid: TGUID): HResult; stdcall;
    function EnumCategories(out penum: IEnumGUID): HResult; stdcall;
    function GetCategoryName(const pguid: TGUID; pszName: PWideChar;
      cch: UINT): HResult; stdcall;
    function CreateCategory(const pguid: TGUID; const riid: TIID;
      out ppv): HResult; stdcall;
  end;

  {$EXTERNALSYM CATEGORYINFO_FLAGS}
  CATEGORYINFO_FLAGS = UINT;
  TCategoryInfoFlags = UINT;

const
  {$EXTERNALSYM CATINFO_NORMAL}
  CATINFO_NORMAL    = $0;
  {$EXTERNALSYM CATINFO_COLLAPSED}
  CATINFO_COLLAPSED = $1;
  {$EXTERNALSYM CATINFO_HIDDEN}
  CATINFO_HIDDEN    = $2;

type
  {$EXTERNALSYM CATSORT_FLAGS}
  CATSORT_FLAGS = UINT;
  TCatSortFlags = UINT;

const
  {$EXTERNALSYM CATSORT_DEFAULT}
  CATSORT_DEFAULT = $0;
  {$EXTERNALSYM CATSORT_NAME}
  CATSORT_NAME    = $1;

type
  PCategoryInfo = ^TCategoryInfo;
  {$EXTERNALSYM CATEGORY_INFO}
  CATEGORY_INFO = record
    cif: TCategoryInfoFlags;
    wszName: array[0..259] of WideChar;
  end;
  TCategoryInfo = CATEGORY_INFO;

const
  IID_ICategorizer: TGUID = '{A3B14589-9174-49A8-89A3-06A1AE2B9BA7}';

type
  {$EXTERNALSYM ICategorizer}
  ICategorizer = interface(IUnknown)
  ['{A3B14589-9174-49A8-89A3-06A1AE2B9BA7}']
    function GetDescription(pszDesc: PWideChar; cch: UINT): HResult; stdcall;
    function GetCategory(cidl: UINT; apidl: PPItemIDList;
      rgCategoryIds: PDWORD): HResult; stdcall;
    function GetCategoryInfo(dwCategoryId: DWORD;
      out pci: TCategoryInfo): HResult; stdcall;
    function CompareCategory(csfFlags: TCatSortFlags;
      dwCategoryId1, dwCategoryId2: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM SLR_FLAGS}
  SLR_FLAGS = UINT;
  TSLRFlags = UINT;

const
  {$EXTERNALSYM SLR_NO_UI}
  SLR_NO_UI               = $1;
  {$EXTERNALSYM SLR_ANY_MATCH}
  SLR_ANY_MATCH           = $2;
  {$EXTERNALSYM SLR_UPDATE}
  SLR_UPDATE              = $4;
  {$EXTERNALSYM SLR_NOUPDATE}
  SLR_NOUPDATE            = $8;
  {$EXTERNALSYM SLR_NOSEARCH}
  SLR_NOSEARCH            = $10;
  {$EXTERNALSYM SLR_NOTRACK}
  SLR_NOTRACK             = $20;
  {$EXTERNALSYM SLR_NOLINKINFO}
  SLR_NOLINKINFO          = $40;
  {$EXTERNALSYM SLR_INVOKE_MSI}
  SLR_INVOKE_MSI          = $80;
  {$EXTERNALSYM SLR_NO_UI_WITH_MSG_PUMP}
  SLR_NO_UI_WITH_MSG_PUMP = $101;

type
  {$EXTERNALSYM SLGP_FLAGS}
  SLGP_FLAGS = UINT;
  TSLGPFlags = UINT;

const
  {$EXTERNALSYM SLGP_SHORTPATH}
  SLGP_SHORTPATH   = $1;
  {$EXTERNALSYM SLGP_UNCPRIORITY}
  SLGP_UNCPRIORITY = $2;
  {$EXTERNALSYM SLGP_RAWPATH}
  SLGP_RAWPATH     = $4;

type
  {$EXTERNALSYM IShellLinkA}
  IShellLinkA = interface(IUnknown)
  ['{000214EE-0000-0000-C000-000000000046}']
    function GetPath(pszFile: PAnsiChar; cch: Integer; var pfd: TWin32FindDataA;
      fFlags: DWORD): HResult; stdcall;
    function GetIDList(out ppidl: PItemIDList): HResult; stdcall;
    function SetIDList(pidl: PItemIDList): HResult; stdcall;
    function GetDescription(pszName: PAnsiChar; cch: Integer): HResult; stdcall;
    function SetDescription(pszName: PAnsiChar): HResult; stdcall;
    function GetWorkingDirectory(pszDir: PAnsiChar;
      cch: Integer): HResult; stdcall;
    function SetWorkingDirectory(pszDir: PAnsiChar): HResult; stdcall;
    function GetArguments(pszArgs: PAnsiChar; cch: Integer): HResult; stdcall;
    function SetArguments(pszArgs: PAnsiChar): HResult; stdcall;
    function GetHotkey(out pwHotkey: Word): HResult; stdcall;
    function SetHotkey(wHotkey: Word): HResult; stdcall;
    function GetShowCmd(out piShowCmd: Integer): HResult; stdcall;
    function SetShowCmd(iShowCmd: Integer): HResult; stdcall;
    function GetIconLocation(pszIconPath: PAnsiChar; cch: Integer;
      out piIcon: Integer): HResult; stdcall;
    function SetIconLocation(pszIconPath: PAnsiChar;
      iIcon: Integer): HResult; stdcall;
    function SetRelativePath(pszPathRel: PAnsiChar;
      dwReserved: DWORD): HResult; stdcall;
    function Resolve(hwnd: HWND; fFlags: DWORD): HResult; stdcall;
    function SetPath(pszFile: PAnsiChar): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellLinkW}
  IShellLinkW = interface(IUnknown)
  ['{000214F9-0000-0000-C000-000000000046}']
    function GetPath(pszFile: PWideChar; cch: Integer; var pfd: TWin32FindDataW;
      fFlags: DWORD): HResult; stdcall;
    function GetIDList(out ppidl: PItemIDList): HResult; stdcall;
    function SetIDList(pidl: PItemIDList): HResult; stdcall;
    function GetDescription(pszName: PWideChar; cch: Integer): HResult; stdcall;
    function SetDescription(pszName: PWideChar): HResult; stdcall;
    function GetWorkingDirectory(pszDir: PWideChar;
      cch: Integer): HResult; stdcall;
    function SetWorkingDirectory(pszDir: PWideChar): HResult; stdcall;
    function GetArguments(pszArgs: PWideChar; cch: Integer): HResult; stdcall;
    function SetArguments(pszArgs: PWideChar): HResult; stdcall;
    function GetHotkey(out pwHotkey: Word): HResult; stdcall;
    function SetHotkey(wHotkey: Word): HResult; stdcall;
    function GetShowCmd(out piShowCmd: Integer): HResult; stdcall;
    function SetShowCmd(iShowCmd: Integer): HResult; stdcall;
    function GetIconLocation(pszIconPath: PWideChar; cch: Integer;
      out piIcon: Integer): HResult; stdcall;
    function SetIconLocation(pszIconPath: PWideChar;
      iIcon: Integer): HResult; stdcall;
    function SetRelativePath(pszPathRel: PWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function Resolve(hwnd: HWND; fFlags: DWORD): HResult; stdcall;
    function SetPath(pszFile: PWideChar): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellLink}
  IShellLink = IShellLinkA;

{$HPPEMIT '#ifdef UNICODE'}
{$HPPEMIT '#define _di_IShellLink  _di_IShellLinkW'}
{$HPPEMIT '#else'}
{$HPPEMIT '#define _di_IShellLink  _di_IShellLinkA'}
{$HPPEMIT '#endif'}

type
  {$EXTERNALSYM SPINITF}
  SPINITF = DWORD;
  TSpinItF = DWORD;

const
  {$EXTERNALSYM SPINITF_NORMAL}
  SPINITF_NORMAL     = $0;
  {$EXTERNALSYM SPINITF_MODAL}
  SPINITF_MODAL      = $1;
  {$EXTERNALSYM SPINITF_NOMINIMIZE}
  SPINITF_NOMINIMIZE = $8;

  {$EXTERNALSYM IID_IActionProgressDialog}
  IID_IActionProgressDialog: TGUID = '{49FF1172-EADC-446D-9285-156453A6431C}';

type
  {$EXTERNALSYM IActionProgressDialog}
  IActionProgressDialog = interface(IUnknown)
  ['{49FF1172-EADC-446D-9285-156453A6431C}']
    function Initialize(flags: SPINITF;
      pszTitle, pszCancel: PWideChar): HResult; stdcall;
    function Stop: HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IHWEventHandler}
  IID_IHWEventHandler: TGUID = '{C1FB73D0-EC3A-4BA2-B512-8CDB9187B6D1}';

type
  {$EXTERNALSYM IHWEventHandler}
  IHWEventHandler = interface(IUnknown)
  ['{C1FB73D0-EC3A-4BA2-B512-8CDB9187B6D1}']
    function Initialize(pszParams: PWideChar): HResult; stdcall;
    function HandleEvent(
      pszDeviceID, pszAltDeviceID, pszEventType: PWideChar): HResult; stdcall;
    function HandleEventWithContent(
      pszDeviceID, pszAltDeviceID, pszEventType, pszContentTypeHandler: PWideChar;
      pdataobject: IDataObject): HResult; stdcall;
  end;

const
  {$EXTERNALSYM ARCONTENT_AUTORUNINF}
  ARCONTENT_AUTORUNINF      = $00000002;
  {$EXTERNALSYM ARCONTENT_AUDIOCD}
  ARCONTENT_AUDIOCD         = $00000004;
  {$EXTERNALSYM ARCONTENT_DVDMOVIE}
  ARCONTENT_DVDMOVIE        = $00000008;
  {$EXTERNALSYM ARCONTENT_BLANKCD}
  ARCONTENT_BLANKCD         = $00000010;
  {$EXTERNALSYM ARCONTENT_BLANKDVD}
  ARCONTENT_BLANKDVD        = $00000020;
  {$EXTERNALSYM ARCONTENT_UNKNOWNCONTENT}
  ARCONTENT_UNKNOWNCONTENT  = $00000040;
  {$EXTERNALSYM ARCONTENT_AUTOPLAYPIX}
  ARCONTENT_AUTOPLAYPIX     = $00000080;
  {$EXTERNALSYM ARCONTENT_AUTOPLAYMUSIC}
  ARCONTENT_AUTOPLAYMUSIC   = $00000100;
  {$EXTERNALSYM ARCONTENT_AUTOPLAYVIDEO}
  ARCONTENT_AUTOPLAYVIDEO   = $00000200;

  {$EXTERNALSYM IID_IQueryCancelAutoPlay}
  IID_IQueryCancelAutoPlay: TGUID = '{DDEFE873-6997-4E68-BE26-39B633ADBE12}';

type
  {$EXTERNALSYM IQueryCancelAutoPlay}
  IQueryCancelAutoPlay = interface(IUnknown)
  ['{DDEFE873-6997-4E68-BE26-39B633ADBE12}']
    function AllowAutoPlay(pszPath: PWideChar; dwContentType: DWORD;
      pszLabel: PWideChar; dwSerialNumber: DWORD): HResult; stdcall;
  end;

type
  {$EXTERNALSYM SPBEGINF}
  SPBEGINF = DWORD;
  TSPBeginF = DWORD;

const
  {$EXTERNALSYM SPBEGINF_NORMAL}
  SPBEGINF_NORMAL          = $00;
  {$EXTERNALSYM SPBEGINF_AUTOTIME}
  SPBEGINF_AUTOTIME        = $02;
  {$EXTERNALSYM SPBEGINF_NOPROGRESSBAR}
  SPBEGINF_NOPROGRESSBAR   = $10;
  {$EXTERNALSYM SPBEGINF_MARQUEEPROGRESS}
  SPBEGINF_MARQUEEPROGRESS = $20;

type
  {$EXTERNALSYM _SPACTION}
  _SPACTION = DWORD;
  {$EXTERNALSYM SPACTION}
  SPACTION = _SPACTION;
  TSPAction = _SPACTION;

const
  {$EXTERNALSYM SPACTION_NONE}
  SPACTION_NONE               = 0;
  {$EXTERNALSYM SPACTION_MOVING}
  SPACTION_MOVING             = SPACTION_NONE + 1;
  {$EXTERNALSYM SPACTION_COPYING}
  SPACTION_COPYING            = SPACTION_MOVING + 1;
  {$EXTERNALSYM SPACTION_RECYCLING}
  SPACTION_RECYCLING          = SPACTION_COPYING + 1;
  {$EXTERNALSYM SPACTION_APPLYINGATTRIBS}
  SPACTION_APPLYINGATTRIBS    = SPACTION_RECYCLING + 1;
  {$EXTERNALSYM SPACTION_DOWNLOADING}
  SPACTION_DOWNLOADING        = SPACTION_APPLYINGATTRIBS + 1;
  {$EXTERNALSYM SPACTION_SEARCHING_INTERNET}
  SPACTION_SEARCHING_INTERNET = SPACTION_DOWNLOADING + 1;
  {$EXTERNALSYM SPACTION_CALCULATING}
  SPACTION_CALCULATING        = SPACTION_SEARCHING_INTERNET + 1;
  {$EXTERNALSYM SPACTION_UPLOADING}
  SPACTION_UPLOADING          = SPACTION_CALCULATING + 1;
  {$EXTERNALSYM SPACTION_SEARCHING_FILES}
  SPACTION_SEARCHING_FILES    = SPACTION_UPLOADING + 1;

type
  {$EXTERNALSYM _SPTEXT}
  _SPTEXT = DWORD;
  {$EXTERNALSYM SPTEXT}
  SPTEXT = _SPTEXT;
  TSPText = _SPTEXT;

const
  {$EXTERNALSYM SPTEXT_ACTIONDESCRIPTION}
  SPTEXT_ACTIONDESCRIPTION = 1;
  {$EXTERNALSYM SPTEXT_ACTIONDETAIL}
  SPTEXT_ACTIONDETAIL      = SPTEXT_ACTIONDESCRIPTION + 1;

  {$EXTERNALSYM IID_IActionProgress}
  IID_IActionProgress: TGUID = '{49FF1173-EADC-446D-9285-156453A6431C}';

type
  {$EXTERNALSYM IActionProgress}
  IActionProgress = interface(IUnknown)
  ['{49FF1173-EADC-446D-9285-156453A6431C}']
    function _Begin(action: TSPAction; flags: TSPBeginF): HResult; stdcall;
    function UpdateProgress(ulCompleted, ulTotal: Int64): HResult; stdcall;
    function UpdateText(sptext: TSPText; pszText: PWideChar;
      fMayCompact: BOOL): HResult; stdcall;
    function QueryCancel(out pfCancelled: BOOL): HResult; stdcall;
    function ResetCancel: HResult; stdcall;
    function _End: HResult; stdcall;
  end;

  {$EXTERNALSYM IShellExtInit}
  IShellExtInit = interface(IUnknown)
  ['{000214E8-0000-0000-C000-000000000046}']
    function Initialize(pidlFolder: PItemIDList; pdtobj: IDataObject;
      hkeyProgID: HKEY): HResult; stdcall;
  end;

const
  {$EXTERNALSYM EXPPS_FILETYPES}
  EXPPS_FILETYPES = $1;

type
  {$EXTERNALSYM EXPPS}
  EXPPS = UINT;
  TExPPS = UINT;

  {$EXTERNALSYM IShellPropSheetExt}
  IShellPropSheetExt = interface(IUnknown)
  ['{000214E9-0000-0000-C000-000000000046}']
    function AddPages(pfnAddPage: TFNAddPropSheetPage;
      lParam: LPARAM): HResult; stdcall;
    function ReplacePage(uPageID: TExPPS; pfnReplaceWith: TFNAddPropSheetPage;
      lParam: LPARAM): HResult; stdcall;
  end;

  {$EXTERNALSYM IRemoteComputer}
  IRemoteComputer = interface(IUnknown)
  ['{000214FE-0000-0000-C000-000000000046}']
    function Initialize(pszMachine: PWideChar;
      bEnumerating: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IQueryContinue}
  IID_IQueryContinue: TGUID = '{7307055C-B24A-486B-9F25-163E597A28A9}';

type
  IQueryContinue = interface(IUnknown)
  ['{7307055C-B24A-486B-9F25-163E597A28A9}']
    function QueryContinue: HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IUserNotification}
  IID_IUserNotification: TGUID = '{BA9711BA-5893-4787-A7E1-41277151550B}';

type
  {$EXTERNALSYM IUserNotification}
  IUserNotification = interface(IUnknown)
  ['{BA9711BA-5893-4787-A7E1-41277151550B}']
    function SetBalloonInfo(pszTitle, pszText: PWideChar;
      dwInfoFlags: DWORD): HResult; stdcall;
    function SetBalloonRetry(dwShowTime, dwInterval: DWORD;
      cRetryCount: UINT): HResult; stdcall;
    function SetIconInfo(hIcon: HICON; pszToolTip: PWideChar): HResult; stdcall;
    function Show(pqc: IQueryContinue;
      dwContinuePollInterval: DWORD): HResult; stdcall;
    function PlaySound(pszSoundName: PWideChar): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IItemNameLimits}
  IID_IItemNameLimits: TGUID = '{1DF0D7F1-B267-4D28-8B10-12E23202A5C4}';

type
  {$EXTERNALSYM IItemNameLimits}
  IItemNameLimits = interface(IUnknown)
  ['{1DF0D7F1-B267-4D28-8B10-12E23202A5C4}']
    function GetValidCharacters(
      out ppwszValidChars, ppwszInvalidChars: PWideChar): HResult; stdcall;
    function GetMaxLength(pszName: PWideChar;
      out piMaxNameLen: Integer): HResult; stdcall;
  end;

const
  {$EXTERNALSYM SNCF_REFRESHLIST}
  SNCF_REFRESHLIST = $00000001;  // refresh the list (eg. from F5 or opening a folder)

  {$EXTERNALSYM IID_INetCrawler}
  IID_INetCrawler: TGUID = '{49C929EE-A1B7-4C58-B539-E63BE392B6F3}';

type
  {$EXTERNALSYM INetCrawler}
  INetCrawler = interface(IUnknown)
  ['{49C929EE-A1B7-4C58-B539-E63BE392B6F3}']
    function Update(dwFlags: DWORD): HResult; stdcall;
  end;

// Values for IShellTaskScheduler, moved here so the IEI_ values can be defined.

// Task priorities
// ---------------
// This depends on the cooperation of tasks currently under execution. New tasks will
// be inserted in the queue in priority order. If a task of a low priority is currently
// under execution when a higher priority task is added, the scheduler will attempt
// to suspend the task currently under execution. It will be resumed when the other tasks
// have been completed.
const
  {$EXTERNALSYM ITSAT_DEFAULT_PRIORITY}
  ITSAT_DEFAULT_PRIORITY = $10000000;
  {$EXTERNALSYM ITSAT_MAX_PRIORITY}
  ITSAT_MAX_PRIORITY     = $7FFFFFFF;
  {$EXTERNALSYM ITSAT_MIN_PRIORITY}
  ITSAT_MIN_PRIORITY     = $00000000;

  {$EXTERNALSYM IEI_PRIORITY_MAX}
  IEI_PRIORITY_MAX     = ITSAT_MAX_PRIORITY;
  {$EXTERNALSYM IEI_PRIORITY_MIN}
  IEI_PRIORITY_MIN     = ITSAT_MIN_PRIORITY;
  {$EXTERNALSYM IEIT_PRIORITY_NORMAL}
  IEIT_PRIORITY_NORMAL = ITSAT_DEFAULT_PRIORITY;

  {$EXTERNALSYM IEIFLAG_ASYNC}
  IEIFLAG_ASYNC     = $0001;     // ask the extractor if it supports ASYNC extract (free threaded)
  {$EXTERNALSYM IEIFLAG_CACHE}
  IEIFLAG_CACHE     = $0002;     // returned from the extractor if it does NOT cache the thumbnail
  {$EXTERNALSYM IEIFLAG_ASPECT}
  IEIFLAG_ASPECT    = $0004;     // passed to the extractor to beg it to render to the aspect ratio of the supplied rect
  {$EXTERNALSYM IEIFLAG_OFFLINE}
  IEIFLAG_OFFLINE   = $0008;     // if the extractor shouldn't hit the net to get any content neede for the rendering
  {$EXTERNALSYM IEIFLAG_GLEAM}
  IEIFLAG_GLEAM     = $0010;     // does the image have a gleam ? this will be returned if it does
  {$EXTERNALSYM IEIFLAG_SCREEN}
  IEIFLAG_SCREEN    = $0020;     // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )
  {$EXTERNALSYM IEIFLAG_ORIGSIZE}
  IEIFLAG_ORIGSIZE  = $0040;     // render to the approx size passed, but crop if neccessary
  {$EXTERNALSYM IEIFLAG_NOSTAMP}
  IEIFLAG_NOSTAMP   = $0080;     // returned from the extractor if it does NOT want an icon stamp on the thumbnail
  {$EXTERNALSYM IEIFLAG_NOBORDER}
  IEIFLAG_NOBORDER  = $0100;     // returned from the extractor if it does NOT want an a border around the thumbnail
  {$EXTERNALSYM IEIFLAG_QUALITY}
  IEIFLAG_QUALITY   = $0200;     // passed to the Extract method to indicate that a slower, higher quality image is desired, re-compute the thumbnail
  {$EXTERNALSYM IEIFLAG_REFRESH}
  IEIFLAG_REFRESH   = $0400;     // returned from the extractor if it would like to have Refresh Thumbnail available

  {$EXTERNALSYM IID_IExtractImage}
  IID_IExtractImage: TGUID = '{BB2E617C-0920-11D1-9A0B-00C04FC2D6C1}';

type
  {$EXTERNALSYM IExtractImage}
  IExtractImage = interface(IUnknown)
  ['{BB2E617C-0920-11D1-9A0B-00C04FC2D6C1}']
    function GetLocation(pszPathBuffer: PWideChar; cch: DWORD;
      var pdwPriority: DWORD; var prgSize: SIZE; dwRecClrDepth: DWORD;
      var pdwFlags: DWORD): HResult; stdcall;
    function Extract(out phBmpThumbnail: HBITMAP): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IExtractImage2}
  IID_IExtractImage2: TGUID = '{953BB1EE-93B4-11d1-98A3-00C04FB687DA}';

type
  {$EXTERNALSYM IExtractImage2}
  IExtractImage2 = interface(IExtractImage)
  ['{953BB1EE-93B4-11d1-98A3-00C04FB687DA}']
    function GetDateStamp(out pDateStamp: TFileTime): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IUserEventTimerCallback}
  IID_IUserEventTimerCallback: TGUID = '{e9ead8e6-2a25-410e-9b58-a9fbef1dd1a2}';

type
  {$EXTERNALSYM IUserEventTimerCallback}
  IUserEventTimerCallback = interface(IUnknown)
  ['{e9ead8e6-2a25-410e-9b58-a9fbef1dd1a2}']
    function UserEventTimerProc(uUserEventTimerID: ULONG;
      uTimerElapse: UINT): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IUserEventTimer}
  IID_IUserEventTimer: TGUID = '{0F504B94-6E42-42E6-99E0-E20FAFE52AB4}';

type
  {$EXTERNALSYM IUserEventTimer}
  IUserEventTimer = interface(IUnknown)
  ['{0F504B94-6E42-42E6-99E0-E20FAFE52AB4}']
    function SetUserEventTimer(hWnd: HWND;
      uCallbackMessage, uTimerElapse: UINT;
      pUserEventTimerCallback: IUserEventTimerCallback;
      var puUserEventTimerID: ULONG): HResult; stdcall;
    function KillUserEventTimer(hWnd: HWND;
      uUserEventTimerID: ULONG): HResult; stdcall;
    function GetUserEventTimerElapsed(hWnd: HWND; uUserEventTimerID: ULONG;
      out puTimerElapsed: UINT): HResult; stdcall;
    function InitTimerTickInterval(
      uTimerTickIntervalMs: UINT): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IDockingWindow}
  IID_IDockingWindow: TGUID = '{012dd920-7b26-11d0-8ca9-00a0c92dbfe8}';

type
  {$EXTERNALSYM IDockingWindow}
  IDockingWindow = interface(IOleWindow)
  ['{012dd920-7b26-11d0-8ca9-00a0c92dbfe8}']
    function ShowDW(fShow: BOOL): HResult; stdcall;
    function CloseDW(dwReserved: DWORD): HResult; stdcall;
    function ResizeBorderDW(const prcBorder: TRect;
      punkToolbarSite: IUnknown; fReserved: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM DBIM_MINSIZE}
  DBIM_MINSIZE   = $0001;
  {$EXTERNALSYM DBIM_MAXSIZE}
  DBIM_MAXSIZE   = $0002;
  {$EXTERNALSYM DBIM_INTEGRAL}
  DBIM_INTEGRAL  = $0004;
  {$EXTERNALSYM DBIM_ACTUAL}
  DBIM_ACTUAL    = $0008;
  {$EXTERNALSYM DBIM_TITLE}
  DBIM_TITLE     = $0010;
  {$EXTERNALSYM DBIM_MODEFLAGS}
  DBIM_MODEFLAGS = $0020;
  {$EXTERNALSYM DBIM_BKCOLOR}
  DBIM_BKCOLOR   = $0040;

type
  PDeskBandInfo = ^TDeskBandInfo;
  {$EXTERNALSYM DESKBANDINFO}
  DESKBANDINFO = record
    dwMask: DWORD;
    ptMinSize: TPointL;
    ptMaxSize: TPointL;
    ptIntegral: TPointL;
    ptActual: TPointL;
    wszTitle: array[0..255] of WideChar;
    dwModeFlags: DWORD;
    crBkgnd: TColorRef;
  end;
  TDeskBandInfo = DESKBANDINFO;

const
  {$EXTERNALSYM DBIMF_NORMAL}
  DBIMF_NORMAL              = $0000;
  {$EXTERNALSYM DBIMF_FIXED}
  DBIMF_FIXED               = $0001;
  {$EXTERNALSYM DBIMF_FIXEDBMP}
  DBIMF_FIXEDBMP            = $0004;   // a fixed background bitmap (if supported)
  {$EXTERNALSYM DBIMF_VARIABLEHEIGHT}
  DBIMF_VARIABLEHEIGHT      = $0008;
  {$EXTERNALSYM DBIMF_UNDELETEABLE}
  DBIMF_UNDELETEABLE        = $0010;
  {$EXTERNALSYM DBIMF_DEBOSSED}
  DBIMF_DEBOSSED            = $0020;
  {$EXTERNALSYM DBIMF_BKCOLOR}
  DBIMF_BKCOLOR             = $0040;
  {$EXTERNALSYM DBIMF_USECHEVRON}
  DBIMF_USECHEVRON          = $0080;
  {$EXTERNALSYM DBIMF_BREAK}
  DBIMF_BREAK               = $0100;
  {$EXTERNALSYM DBIMF_ADDTOFRONT}
  DBIMF_ADDTOFRONT          = $0200;
  {$EXTERNALSYM DBIMF_TOPALIGN}
  DBIMF_TOPALIGN            = $0400;
  {$EXTERNALSYM DBIF_VIEWMODE_NORMAL}
  DBIF_VIEWMODE_NORMAL      = $0000;
  {$EXTERNALSYM DBIF_VIEWMODE_VERTICAL}
  DBIF_VIEWMODE_VERTICAL    = $0001;
  {$EXTERNALSYM DBIF_VIEWMODE_FLOATING}
  DBIF_VIEWMODE_FLOATING    = $0002;
  {$EXTERNALSYM DBIF_VIEWMODE_TRANSPARENT}
  DBIF_VIEWMODE_TRANSPARENT = $0004;

  {$EXTERNALSYM DBID_BANDINFOCHANGED}
  DBID_BANDINFOCHANGED = 0;
  {$EXTERNALSYM DBID_SHOWONLY}
  DBID_SHOWONLY        = 1;
  {$EXTERNALSYM DBID_MAXIMIZEBAND}
  DBID_MAXIMIZEBAND    = 2;
  {$EXTERNALSYM DBID_PUSHCHEVRON}
  DBID_PUSHCHEVRON     = 3;
  {$EXTERNALSYM DBID_DELAYINIT}
  DBID_DELAYINIT       = 4;
  {$EXTERNALSYM DBID_FINISHINIT}
  DBID_FINISHINIT      = 5;
  {$EXTERNALSYM DBID_SETWINDOWTHEME}
  DBID_SETWINDOWTHEME  = 6;
  {$EXTERNALSYM DBID_PERMITAUTOHIDE}
  DBID_PERMITAUTOHIDE  = 7;

  {$EXTERNALSYM DBPC_SELECTFIRST}
  DBPC_SELECTFIRST     = DWORD(-1);
  {$EXTERNALSYM DBPC_SELECTLAST}
  DBPC_SELECTLAST      = DWORD(-2);

  {$EXTERNALSYM IID_IDeskBand}
  IID_IDeskBand: TGUID = '{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}';
  {$EXTERNALSYM CGID_DeskBand}
  CGID_DeskBand: TGUID = '{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}';

type
  {$EXTERNALSYM IDeskBand}
  IDeskBand = interface(IDockingWindow)
  ['{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}']
    function GetBandInfo(dwBandID, dwViewMode: DWORD;
      out pdbi: TDeskBandInfo): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_ITaskbarList}
  IID_ITaskbarList: TGUID = '{56FDF342-FD6D-11d0-958A-006097C9A090}';

type
  {$EXTERNALSYM ITaskbarList}
  ITaskbarList = interface(IUnknown)
  ['{56FDF342-FD6D-11d0-958A-006097C9A090}']
    function HrInit: HResult; stdcall;
    function AddTab(hwnd: HWND): HResult; stdcall;
    function DeleteTab(hwnd: HWND): HResult; stdcall;
    function ActivateTab(hwnd: HWND): HResult; stdcall;
    function SetActiveAlt(hwnd: HWND): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_ITaskbarList2}
  IID_ITaskbarList2: TGUID = '{602D4995-B13A-429b-A66E-1935E44F4317}';

type
  {$EXTERNALSYM ITaskbarList2}
  ITaskbarList2 = interface(ITaskbarList)
  ['{602D4995-B13A-429b-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: HWND;
      fFullscreen: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_ICDBurn}
  IID_ICDBurn: TGUID = '{3d73a659-e5d0-4d42-afc0-5121ba425c8d}';

type
  {$EXTERNALSYM ICDBurn}
  ICDBurn = interface(IUnknown)
  ['{3d73a659-e5d0-4d42-afc0-5121ba425c8d}']
    function GetRecorderDriveLetter(pszDrive: PWideChar;
      cch: UINT): HResult; stdcall;
    function Burn(hwnd: HWND): HResult; stdcall;
    function HasRecordableDrive(out pfHasRecorder: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IDD_WIZEXTN_FIRST}
  IDD_WIZEXTN_FIRST    = $5000;
  {$EXTERNALSYM IDD_WIZEXTN_LAST}
  IDD_WIZEXTN_LAST     = $5100;

const
  {$EXTERNALSYM IID_IWizardSite}
  IID_IWizardSite: TGUID = '{88960f5b-422f-4e7b-8013-73415381c3c3}';

type
  {$EXTERNALSYM IWizardSite}
  IWizardSite = interface(IUnknown)
  ['{88960f5b-422f-4e7b-8013-73415381c3c3}']
    function GetPreviousPage(out phpage: HPROPSHEETPAGE): HResult; stdcall;
    function GetNextPage(out phpage: HPROPSHEETPAGE): HResult; stdcall;
    function GetCancelledPage(out phpage: HPROPSHEETPAGE): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IWizardExtension}
  IID_IWizardExtension: TGUID = '{c02ea696-86cc-491e-9b23-74394a0444a8}';

type
  {$NODEFINE PHPROPSHEETPAGEArray}
  PHPROPSHEETPAGEArray = ^THPROPSHEETPAGEArray;
  {$NODEFINE THPROPSHEETPAGEArray}
  THPROPSHEETPAGEArray = array[0..65535] of HPROPSHEETPAGE;

  {$EXTERNALSYM IWizardExtension}
  IWizardExtension = interface(IUnknown)
  ['{c02ea696-86cc-491e-9b23-74394a0444a8}']
    function AddPages(aPages: PHPROPSHEETPAGEArray; cPages: UINT;
      out pnPagesAdded: UINT): HResult; stdcall;
    function GetFirstPage(out phpage: HPROPSHEETPAGE): HResult; stdcall;
    function GetLastPage(out phpage: HPROPSHEETPAGE): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IWebWizardExtension}
  IID_IWebWizardExtension: TGUID = '{0e6b3f66-98d1-48c0-a222-fbde74e2fbc5}';

type
  {$EXTERNALSYM IWebWizardExtension}
  IWebWizardExtension = interface(IWizardExtension)
  ['{0e6b3f66-98d1-48c0-a222-fbde74e2fbc5}']
    function SetInitialURL(pszURL: PWideChar): HResult; stdcall;
    function SetErrorURL(pszErrorURL: PWideChar): HResult; stdcall;
  end;

const
  {$EXTERNALSYM SID_WebWizardHost}
  SID_WebWizardHost: TGUID = '{0e6b3f66-98d1-48c0-a222-fbde74e2fbc5}';

  {$EXTERNALSYM SHPWHF_NORECOMPRESS}
  SHPWHF_NORECOMPRESS             = $00000001;  // don't allow/prompt for recompress of streams
  {$EXTERNALSYM SHPWHF_NONETPLACECREATE}
  SHPWHF_NONETPLACECREATE         = $00000002;  // don't create a network place when transfer is complete
  {$EXTERNALSYM SHPWHF_NOFILESELECTOR}
  SHPWHF_NOFILESELECTOR           = $00000004;  // don't show the file selector
  {$EXTERNALSYM SHPWHF_VALIDATEVIAWEBFOLDERS}
  SHPWHF_VALIDATEVIAWEBFOLDERS    = $00010000;  // enable web folders to validate network places (ANP support)

  {$EXTERNALSYM IID_IPublishingWizard}
  IID_IPublishingWizard: TGUID = '{aa9198bb-ccec-472d-beed-19a4f6733f7a}';

type
  {$EXTERNALSYM IPublishingWizard}
  IPublishingWizard = interface(IWizardExtension)
  ['{aa9198bb-ccec-472d-beed-19a4f6733f7a}']
    function Initialize(pdo: IDataObject; dwOptions: DWORD;
      pszServiceProvider: PWideChar): HResult; stdcall;
    function GetTransferManifest(out phrFromTransfer: HRESULT;
      out pdocManifest: {$IFDEF DELPHI5}Pointer{$ELSE}IXMLDOMDocument{$ENDIF}): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IFolderViewHost}
  IID_IFolderViewHost: TGUID = '{1ea58f02-d55a-411d-b09e-9e65ac21605b}';

type
  {$EXTERNALSYM IFolderViewHost}
  IFolderViewHost = interface(IUnknown)
  ['{1ea58f02-d55a-411d-b09e-9e65ac21605b}']
    function Initialize(hwndParent: HWND; pdo: IDataObject;
      var prc: TRect): HResult; stdcall;
  end;

const
  {$EXTERNALSYM ACDD_VISIBLE}
  ACDD_VISIBLE = $0001;

  {$EXTERNALSYM IID_IAutoCompleteDropDown}
  IID_IAutoCompleteDropDown: TGUID = '{3CD141F4-3C6A-11d2-BCAA-00C04FD929DB}';

type
  {$EXTERNALSYM IAutoCompleteDropDown}
  IAutoCompleteDropDown = interface(IUnknown)
  ['{3CD141F4-3C6A-11d2-BCAA-00C04FD929DB}']
    function GetDropDownStatus(out pdwFlags: DWORD;
      out ppwszString: PWideChar): HResult; stdcall;
    function ResetEnumerator: HResult; stdcall;
  end;

const
  // The wizard was launch explicitly by the user, not on demand by the key manager
  {$EXTERNALSYM PPW_LAUNCHEDBYUSER}
  PPW_LAUNCHEDBYUSER = $00000001;      

const
  {$EXTERNALSYM IID_IModalWindow}
  IID_IModalWindow: TGUID = '{b4db1657-70d7-485e-8e3e-6fcb5a5c1802}';

type
  {$EXTERNALSYM IModalWindow}
  IModalWindow = interface(IUnknown)
  ['{b4db1657-70d7-485e-8e3e-6fcb5a5c1802}']
    function Show(hwndParent: HWND): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IPassportWizard}
  IID_IPassportWizard: TGUID = '{a09db586-9180-41ac-9114-460a7f362b76}';

type
  IPassportWizard = interface(IModalWindow)
  ['{a09db586-9180-41ac-9114-460a7f362b76}']
    function SetOptions(dwOptions: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM PROPSTR_EXTENSIONCOMPLETIONSTATE}
  PROPSTR_EXTENSIONCOMPLETIONSTATE = 'ExtensionCompletionState';

  {$EXTERNALSYM CDBE_RET_DEFAULT}
  CDBE_RET_DEFAULT          = $0;
  {$EXTERNALSYM CDBE_RET_DONTRUNOTHEREXTS}
  CDBE_RET_DONTRUNOTHEREXTS = $1;
  {$EXTERNALSYM CDBE_RET_STOPWIZARD}
  CDBE_RET_STOPWIZARD       = $2;

  {$EXTERNALSYM CDBE_TYPE_MUSIC}
  CDBE_TYPE_MUSIC = $1;
  {$EXTERNALSYM CDBE_TYPE_DATA}
  CDBE_TYPE_DATA  = $2;
  {$EXTERNALSYM CDBE_TYPE_ALL}
  CDBE_TYPE_ALL   = $FFFFFFFF;

  {$EXTERNALSYM IID_ICDBurnExt}
  IID_ICDBurnExt: TGUID = '{2271dcca-74fc-4414-8fb7-c56b05ace2d7}';
  {$EXTERNALSYM SID_CDWizardHost}
  SID_CDWizardHost: TGUID = '{2271dcca-74fc-4414-8fb7-c56b05ace2d7}';

type
  {$EXTERNALSYM ICDBurnExt}
  ICDBurnExt = interface(IUnknown)
  ['{2271dcca-74fc-4414-8fb7-c56b05ace2d7}']
    function GetSupportedActionTypes(pdwActions: PDWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM PFDVENUMREADYBALLBACK}
  PFDVENUMREADYBALLBACK = procedure(pvData: Pointer) cdecl;
  TFDVEnumReadyCallback = PFDVENUMREADYBALLBACK;

const
  {$EXTERNALSYM IID_IDVGetEnum}
  IID_IDVGetEnum: TGUID = '{70F55181-5FEA-4900-B6B8-7343CB0A348C}';

type
  {$EXTERNALSYM IDVGetEnum}
  IDVGetEnum = interface(IUnknown)
  ['{70F55181-5FEA-4900-B6B8-7343CB0A348C}']
    function SetEnumReadyCallback(pfn: TFDVEnumReadyCallback;
      pvData: Pointer): HResult; stdcall;
    function CreateEnumIDListFromContents(pidlFolder: PItemIDList;
      dwEnumFlags: DWORD; out ppEnumIDList: IEnumIDList): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IInsertItem}
  IID_IInsertItem: TGUID = '{D2B57227-3D23-4b95-93C0-492BD454C356}';

type
  {$EXTERNALSYM IInsertItem}
  IInsertItem = interface(IUnknown)
  ['{D2B57227-3D23-4b95-93C0-492BD454C356}']
    function InsertItem(pidl: PItemIDList): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IDeskBar}
  IID_IDeskBar: TGUID = '{EB0FE173-1A3A-11D0-89B3-00A0C90A90AC}';

type
  {$EXTERNALSYM IDeskBar}
  IDeskBar = interface(IOleWindow)
  ['{EB0FE173-1A3A-11D0-89B3-00A0C90A90AC}']
    function SetClient(punkClient: IUnknown): HResult; stdcall;
    function GetClient(out ppunkClient: IUnknown): HResult; stdcall;
    function OnPosRectChangeDB(var prc: TRect): HResult; stdcall;
  end;

const
  {$EXTERNALSYM MBHANDCID_PIDLSELECT}
  MBHANDCID_PIDLSELECT = 0;

  {$EXTERNALSYM IID_IMenuBand}
  IID_IMenuBand: TGUID = '{568804CD-CBD7-11d0-9816-00C04FD91972}';

type
  {$EXTERNALSYM IMenuBand}
  IMenuBand = interface(IUnknown)
  ['{568804CD-CBD7-11d0-9816-00C04FD91972}']
    function IsMenuMessage(var pmsg: TMSG): HResult; stdcall;
    function TranslateMenuMessage(var pmsg: TMSG;
      out plRet: LRESULT): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IFolderBandPriv}
  IID_IFolderBandPriv: TGUID = '{47c01f95-e185-412c-b5c5-4f27df965aea}';

type
  {$EXTERNALSYM IFolderBandPriv}
  IFolderBandPriv = interface(IUnknown)
  ['{47c01f95-e185-412c-b5c5-4f27df965aea}']
    function SetCascade(f: BOOL): HResult; stdcall;
    function SetAccelerators(f: BOOL): HResult; stdcall;
    function SetNoIcons(f: BOOL): HResult; stdcall;
    function SetNoText(f: BOOL): HResult; stdcall;
  end;

  PBandSiteInfo = ^TBandSiteInfo;
  {$EXTERNALSYM tagBANDSITEINFO}
  tagBANDSITEINFO = record
    dwMask: DWORD;
    dwState: DWORD;
    dwStyle: DWORD;
  end;
  {$EXTERNALSYM BANDSITEINFO}
  BANDSITEINFO = tagBANDSITEINFO;
  TBandSiteInfo = tagBANDSITEINFO;

const
  {$EXTERNALSYM BSID_BANDADDED}
  BSID_BANDADDED   = 0;
  {$EXTERNALSYM BSID_BANDREMOVED}
  BSID_BANDREMOVED = BSID_BANDADDED + 1;

  BSIM_STATE          = $00000001;
  BSIM_STYLE          = $00000002;
  BSSF_VISIBLE        = $00000001;
  BSSF_NOTITLE        = $00000002;
  BSSF_UNDELETEABLE   = $00001000;
  BSIS_AUTOGRIPPER    = $00000000;
  BSIS_NOGRIPPER      = $00000001;
  BSIS_ALWAYSGRIPPER  = $00000002;
  BSIS_LEFTALIGN      = $00000004;
  BSIS_SINGLECLICK    = $00000008;
  BSIS_NOCONTEXTMENU  = $00000010;
  BSIS_NODROPTARGET   = $00000020;
  BSIS_NOCAPTION      = $00000040;
  BSIS_PREFERNOLINEBREAK   = $00000080;
  BSIS_LOCKED         = $00000100;

const
  {$EXTERNALSYM IID_IBandSite}
  IID_IBandSite: TGUID = '{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}';
  {$EXTERNALSYM SID_SBandSite}
  SID_SBandSite: TGUID = '{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}';
  {$EXTERNALSYM CGID_BandSite}
  CGID_BandSite: TGUID = '{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}';

type
  {$EXTERNALSYM IBandSite}
  IBandSite = interface(IUnknown)
  ['{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}']
    function AddBand(punk: IUnknown): HResult; stdcall;
    function EnumBands(uBand: UINT; out pdwBandID: DWORD): HResult; stdcall;
    function QueryBand(dwBandID: DWORD; out ppstb: IDeskBand;
      out pdwState: DWORD; pszName: PWideChar;
      cchName: Integer): HResult; stdcall;
    function SetBandState(dwBandID, dwMask, dwState: DWORD): HResult; stdcall;
    function RemoveBand(dwBandID: DWORD): HResult; stdcall;
    function GetBandObject(dwBandID: DWORD; const riid: TIID;
      out ppv): HResult; stdcall;
    function SetBandSiteInfo(var pbsinfo: TBandSiteInfo): HResult; stdcall;
    function GetBandSiteInfo(var pbsinfo: TBandSiteInfo): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_INamespaceWalkCB}
  IID_INamespaceWalkCB: TGUID = '{d92995f8-cf5e-4a76-bf59-ead39ea2b97e}';

type
  {$EXTERNALSYM INamespaceWalkCB}
  INamespaceWalkCB = interface(IUnknown)
  ['{d92995f8-cf5e-4a76-bf59-ead39ea2b97e}']
    function FoundItem(psf: IShellFolder; pidl: PItemIDList): HResult; stdcall;
    function EnterFolder(psf: IShellFolder;
      pidl: PItemIDList): HResult; stdcall;
    function LeaveFolder(psf: IShellFolder;
      pidl: PItemIDList): HResult; stdcall;
    function InitializeProgressDialog(
      out ppszTitle, ppszCancel: PWideChar): HResult; stdcall;
  end;

const
  {$EXTERNALSYM NSWF_NONE_IMPLIES_ALL}
  NSWF_NONE_IMPLIES_ALL          = $1;
  {$EXTERNALSYM NSWF_ONE_IMPLIES_ALL}
  NSWF_ONE_IMPLIES_ALL           = $2;
  {$EXTERNALSYM NSWF_DONT_TRAVERSE_LINKS}
  NSWF_DONT_TRAVERSE_LINKS       = $4;
  {$EXTERNALSYM NSWF_DONT_ACCUMULATE_RESULT}
  NSWF_DONT_ACCUMULATE_RESULT    = $8;
  {$EXTERNALSYM NSWF_TRAVERSE_STREAM_JUNCTIONS}
  NSWF_TRAVERSE_STREAM_JUNCTIONS = $10;
  {$EXTERNALSYM NSWF_FILESYSTEM_ONLY}
  NSWF_FILESYSTEM_ONLY           = $20;
  {$EXTERNALSYM NSWF_SHOW_PROGRESS}
  NSWF_SHOW_PROGRESS             = $40;
  {$EXTERNALSYM NSWF_FLAG_VIEWORDER}
  NSWF_FLAG_VIEWORDER            = $80;
  {$EXTERNALSYM NSWF_IGNORE_AUTOPLAY_HIDA}
  NSWF_IGNORE_AUTOPLAY_HIDA      = $100;

const
  {$EXTERNALSYM IID_INamespaceWalk}
  IID_INamespaceWalk: TGUID = '{57ced8a7-3f4a-432c-9350-30f24483f74f}';

type
  {$EXTERNALSYM INamespaceWalk}
  INamespaceWalk = interface(IUnknown)
  ['{57ced8a7-3f4a-432c-9350-30f24483f74f}']
    function Walk(punkToWalk: IUnknown; dwFlags: DWORD; cDepth: Integer;
      pnswcb: INamespaceWalkCB): HResult; stdcall;
    function GetIDArrayResult(out pcItems: UINT;
      out pppidl: PPItemIDListArray): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IRegTreeItem}
  IID_IRegTreeItem: TGUID = '{A9521922-0812-4d44-9EC3-7FD38C726F3D}';

type
  {$EXTERNALSYM IRegTreeItem}
  IRegTreeItem = interface(IUnknown)
  ['{A9521922-0812-4d44-9EC3-7FD38C726F3D}']
    function GetCheckState(out pbCheck: BOOL): HResult; stdcall;
    function SetCheckState(bCheck: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM MPOS_EXECUTE}
  MPOS_EXECUTE       = 0;
  {$EXTERNALSYM MPOS_FULLCANCEL}
  MPOS_FULLCANCEL    = MPOS_EXECUTE + 1;
  {$EXTERNALSYM MPOS_CANCELLEVEL}
  MPOS_CANCELLEVEL   = MPOS_FULLCANCEL + 1;
  {$EXTERNALSYM MPOS_SELECTLEFT}
  MPOS_SELECTLEFT    = MPOS_CANCELLEVEL + 1;
  {$EXTERNALSYM MPOS_SELECTRIGHT}
  MPOS_SELECTRIGHT   = MPOS_SELECTLEFT + 1;
  {$EXTERNALSYM MPOS_CHILDTRACKING}
  MPOS_CHILDTRACKING = MPOS_SELECTRIGHT + 1;

  {$EXTERNALSYM MPPF_SETFOCUS}
  MPPF_SETFOCUS      = $1;
  {$EXTERNALSYM MPPF_INITIALSELECT}
  MPPF_INITIALSELECT = $2;
  {$EXTERNALSYM MPPF_NOANIMATE}
  MPPF_NOANIMATE     = $4;
  {$EXTERNALSYM MPPF_KEYBOARD}
  MPPF_KEYBOARD      = $10;
  {$EXTERNALSYM MPPF_REPOSITION}
  MPPF_REPOSITION    = $20;
  {$EXTERNALSYM MPPF_FORCEZORDER}
  MPPF_FORCEZORDER   = $40;
  {$EXTERNALSYM MPPF_FINALSELECT}
  MPPF_FINALSELECT   = $80;
  {$EXTERNALSYM MPPF_TOP}
  MPPF_TOP           = $20000000;
  {$EXTERNALSYM MPPF_LEFT}
  MPPF_LEFT          = $40000000;
  {$EXTERNALSYM MPPF_RIGHT}
  MPPF_RIGHT         = $60000000;
  {$EXTERNALSYM MPPF_BOTTOM}
  MPPF_BOTTOM        = $80000000;
  {$EXTERNALSYM MPPF_POS_MASK}
  MPPF_POS_MASK      = $e0000000;

  {$EXTERNALSYM IID_IMenuPopup}
  IID_IMenuPopup: TGUID = '{D1E7AFEB-6A2E-11d0-8C78-00C04FD918B4}';

type
{$IFNDEF JWA_INCLUDEMODE}
  {$NODEFINE TRectL}
  TRectL = TRect;
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM IMenuPopup}
  IMenuPopup = interface(IDeskBar)
  ['{D1E7AFEB-6A2E-11d0-8C78-00C04FD918B4}']
    function Popup(var ppt: TPointL; var prcExclude: TRectL;
      dwFlags: DWORD): HResult; stdcall;
    function OnSelect(dwSelectType: DWORD): HResult; stdcall;
    function SetSubMenu(pmp: IMenuPopup; fSet: BOOL): HResult; stdcall;
  end;

type
  {$EXTERNALSYM SIGDN}
  SIGDN = DWORD; // ShellItem GetDisplayName
  TSIGDN = DWORD;

const
  {$EXTERNALSYM SIGDN_NORMALDISPLAY}
  SIGDN_NORMALDISPLAY               = 0;
  {$EXTERNALSYM SIGDN_PARENTRELATIVEPARSING}
  SIGDN_PARENTRELATIVEPARSING       = $80018001;
  {$EXTERNALSYM SIGDN_PARENTRELATIVEFORADDRESSBAR}
  SIGDN_PARENTRELATIVEFORADDRESSBAR = $8001c001;
  {$EXTERNALSYM SIGDN_DESKTOPABSOLUTEPARSING}
  SIGDN_DESKTOPABSOLUTEPARSING      = $80028000;
  {$EXTERNALSYM SIGDN_PARENTRELATIVEEDITING}
  SIGDN_PARENTRELATIVEEDITING       = $80031001;
  {$EXTERNALSYM SIGDN_DESKTOPABSOLUTEEDITING}
  SIGDN_DESKTOPABSOLUTEEDITING      = $8004c000;
  {$EXTERNALSYM SIGDN_FILESYSPATH}
  SIGDN_FILESYSPATH                 = $80058000;
  {$EXTERNALSYM SIGDN_URL}
  SIGDN_URL                         = $80068000;

type
  {$EXTERNALSYM SICHINTF}
  SICHINTF = DWORD; // ShellItem Compare Hint Flag
  TSICHintF = DWORD;

const
  {$EXTERNALSYM SICHINT_DISPLAY}
  SICHINT_DISPLAY   = 0;
  {$EXTERNALSYM SICHINT_ALLFIELDS}
  SICHINT_ALLFIELDS = $80000000;
  {$EXTERNALSYM SICHINT_CANONICAL}
  SICHINT_CANONICAL = $10000000;

  {$EXTERNALSYM IID_IShellItem}
  IID_IShellItem: TGUID = '{43826d1e-e718-42ee-bc55-a1e261c37bfe}';

type
  {$EXTERNALSYM IShellItem}
  IShellItem = interface(IUnknown)
  ['{43826d1e-e718-42ee-bc55-a1e261c37bfe}']
    function BindToHandler(pbc: IBindCtx; const rbhid: TGUID; const riid: TIID;
      out ppvOut): HResult; stdcall;
    function GetParent(out ppsi: IShellItem): HResult; stdcall;
    function GetDisplayName(sigdnName: SIGDN;
      out ppszName: POleStr): HResult; stdcall;
    function GetAttributes(sfgaoMask: TSFGAOF;
      out psfgaoAttribs: TSFGAOF): HResult; stdcall;
    function Compare(psi: IShellItem; hint: SICHINTF;
      out piOrder: Integer): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IImageRecompress}
  IID_IImageRecompress: TGUID = '{505f1513-6b3e-4892-a272-59f8889a4d3e}';

type
  {$EXTERNALSYM IImageRecompress}
  IImageRecompress = interface(IUnknown)
  ['{505f1513-6b3e-4892-a272-59f8889a4d3e}']
    function RecompressImage(psi: IShellItem; cx, cy, iQuality: Integer;
      pstg: IStorage; out ppstrmOut: IStream): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IDefViewSafety}
  IID_IDefViewSafety: TGUID = '{9A93B3FB-4E75-4c74-871A-2CDA667F39A5}';

type
  {$EXTERNALSYM IDefViewSafety}
  IDefViewSafety = interface(IUnknown)
  ['{9A93B3FB-4E75-4c74-871A-2CDA667F39A5}']
    function IsSafePage: HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_IContextMenuSite}
  IID_IContextMenuSite: TGUID = '{0811AEBE-0B87-4C54-9E72-548CF649016B}';

type
  {$EXTERNALSYM IContextMenuSite}
  IContextMenuSite = interface(IUnknown)
  ['{0811AEBE-0B87-4C54-9E72-548CF649016B}']
    function DoContextMenuPopup(punkContextMenu: IUnknown; fFlags: UINT;
      pt: TPoint): HResult; stdcall;
  end;

  PDelegateItemID = ^TDelegateItemID;
  {$EXTERNALSYM tagDELEGATEITEMID}
  tagDELEGATEITEMID = packed record
    cbSize: Word;
    wOuter: Word;
    cbInner: Word;
    rgb: array[0..0] of Byte;
  end;
  {$EXTERNALSYM DELEGATEITEMID}
  DELEGATEITEMID = tagDELEGATEITEMID;
  TDelegateItemID = tagDELEGATEITEMID;

{$IFNDEF JWA_INCLUDEMODE}
const
  {$EXTERNALSYM IID_IDelegateFolder}
  IID_IDelegateFolder: TGUID = '{ADD8BA80-002B-11D0-8F0F-00C04FD7D062}';
{$ENDIF JWA_INCLUDEMODE}

type
  {$EXTERNALSYM IDelegateFolder}
  IDelegateFolder = interface(IUnknown)
  ['{ADD8BA80-002B-11D0-8F0F-00C04FD7D062}']
    function SetItemAlloc(pmalloc: IMalloc): HResult; stdcall;
  end;

// INTERFACE: IBrowserFrameOptions
//
// This interface was implemented so a browser or host can ask a ShellView/ShellNameSpace what
// kind of 'Behavior' is appropriate for that view.
//
//    IBrowserFrameOptions::GetBrowserOptions()
//       dwMask is the logical OR of bits to look for.  pdwOptions is not optional and
//       it's return value will always equal or will be a subset of dwMask.
//       If the function succeeds, the return value must be S_OK and pdwOptions needs to be filled in.
//       If the function fails, pdwOptions needs to be filled in with BFO_NONE.
//

const
  {$EXTERNALSYM BFO_NONE}
  BFO_NONE                             = 0;
  {$EXTERNALSYM BFO_BROWSER_PERSIST_SETTINGS}
  BFO_BROWSER_PERSIST_SETTINGS         = $1;
  {$EXTERNALSYM BFO_RENAME_FOLDER_OPTIONS_TOINTERNET}
  BFO_RENAME_FOLDER_OPTIONS_TOINTERNET = $2;
  {$EXTERNALSYM BFO_BOTH_OPTIONS}
  BFO_BOTH_OPTIONS                     = $4;
  {$EXTERNALSYM BIF_PREFER_INTERNET_SHORTCUT}
  BIF_PREFER_INTERNET_SHORTCUT         = $8;
  {$EXTERNALSYM BFO_BROWSE_NO_IN_NEW_PROCESS}
  BFO_BROWSE_NO_IN_NEW_PROCESS         = $10;
  {$EXTERNALSYM BFO_ENABLE_HYPERLINK_TRACKING}
  BFO_ENABLE_HYPERLINK_TRACKING        = $20;
  {$EXTERNALSYM BFO_USE_IE_OFFLINE_SUPPORT}
  BFO_USE_IE_OFFLINE_SUPPORT           = $40;
  {$EXTERNALSYM BFO_SUBSTITUE_INTERNET_START_PAGE}
  BFO_SUBSTITUE_INTERNET_START_PAGE    = $80;
  {$EXTERNALSYM BFO_USE_IE_LOGOBANDING}
  BFO_USE_IE_LOGOBANDING               = $100;
  {$EXTERNALSYM BFO_ADD_IE_TOCAPTIONBAR}
  BFO_ADD_IE_TOCAPTIONBAR              = $200;
  {$EXTERNALSYM BFO_USE_DIALUP_REF}
  BFO_USE_DIALUP_REF                   = $400;
  {$EXTERNALSYM BFO_USE_IE_TOOLBAR}
  BFO_USE_IE_TOOLBAR                   = $800;
  {$EXTERNALSYM BFO_NO_PARENT_FOLDER_SUPPORT}
  BFO_NO_PARENT_FOLDER_SUPPORT         = $1000;
  {$EXTERNALSYM BFO_NO_REOPEN_NEXT_RESTART}
  BFO_NO_REOPEN_NEXT_RESTART           = $2000;
  {$EXTERNALSYM BFO_GO_HOME_PAGE}
  BFO_GO_HOME_PAGE                     = $4000;
  {$EXTERNALSYM BFO_PREFER_IEPROCESS}
  BFO_PREFER_IEPROCESS                 = $8000;
  {$EXTERNALSYM BFO_SHOW_NAVIGATION_CANCELLED}
  BFO_SHOW_NAVIGATION_CANCELLED        = $10000;
  {$EXTERNALSYM BFO_USE_IE_STATUSBAR}
  BFO_USE_IE_STATUSBAR                 = $20000;
  {$EXTERNALSYM BFO_QUERY_ALL}
  BFO_QUERY_ALL                        = $FFFFFFFF;

type
  {$EXTERNALSYM BROWSERFRAMEOPTIONS}
  BROWSERFRAMEOPTIONS = DWORD;
  TBrowserFrameOptions = DWORD;

const
  {$EXTERNALSYM IID_IBrowserFrameOptions}
  IID_IBrowserFrameOptions: TGUID = '{10DF43C8-1DBE-11d3-8B34-006097DF5BD4}';

type
  {$EXTERNALSYM IBrowserFrameOptions}
  IBrowserFrameOptions = interface(IUnknown)
  ['{10DF43C8-1DBE-11d3-8B34-006097DF5BD4}']
    function GetFrameOptions(dwMask: TBrowserFrameOptions;
      out pdwOptions: TBrowserFrameOptions): HResult; stdcall;
  end;

type
  {$EXTERNALSYM tagNWMF}
  tagNWMF = DWORD;
  {$EXTERNALSYM NWMF}
  NWMF = tagNWMF;
  TNWMF = tagNWMF;

const
  {$EXTERNALSYM NWMF_UNLOADING}
  NWMF_UNLOADING       = $1;
  {$EXTERNALSYM NWMF_USERINITED}
  NWMF_USERINITED      = $2;
  {$EXTERNALSYM NWMF_FIRST}
  NWMF_FIRST           = $4;
  {$EXTERNALSYM NWMF_OVERRIDEKEY}
  NWMF_OVERRIDEKEY     = $8;
  {$EXTERNALSYM NWMF_SHOWHELP}
  NWMF_SHOWHELP        = $10;
  {$EXTERNALSYM NWMF_HTMLDIALOG}
  NWMF_HTMLDIALOG      = $20;
  {$EXTERNALSYM NWMF_FROMDIALOGCHILD}
  NWMF_FROMDIALOGCHILD = $40;
  {$EXTERNALSYM NWMF_USERREQUESTED}
  NWMF_USERREQUESTED   = $80;
  {$EXTERNALSYM NWMF_USERALLOWED}
  NWMF_USERALLOWED     = $100;

  {$EXTERNALSYM IID_INewWindowManager}
  IID_INewWindowManager: TGUID = '{D2BC4C84-3F72-4a52-A604-7BCBF3982CBB}';
  {$EXTERNALSYM SID_SNewWindowManager}
  SID_SNewWindowManager: TGUID = '{D2BC4C84-3F72-4a52-A604-7BCBF3982CBB}';

type
  {$EXTERNALSYM INewWindowManager}
  INewWindowManager = interface(IUnknown)
  ['{D2BC4C84-3F72-4a52-A604-7BCBF3982CBB}']
    function EvaluateNewWindow(
      pszUrl, pszName, pszUrlContext, pszFeatures: PWideChar; fReplace: BOOL;
      dwFlags, dwUserActionTime: DWORD): HResult; stdcall;
  end;

  PSMData = ^TSMData;
  {$EXTERNALSYM tagSMDATA}
  tagSMDATA = record
    dwMask: DWORD;
    dwFlags: DWORD;
    hmenu: HMENU;
    hwnd: HWND;
    uId: UINT;
    uIdParent: UINT;
    uIdAncestor: UINT;
    punk: IUnknown;
    pidlFolder: PItemIDList;
    pidlItem: PItemIDList;
    psf: IShellFolder;
    pvUserData: Pointer;
  end;
  {$EXTERNALSYM SMDATA}
  SMDATA = tagSMDATA;
  TSMData = tagSMDATA;

const
// Mask
  {$EXTERNALSYM SMDM_SHELLFOLDER}
  SMDM_SHELLFOLDER = $00000001;  // This is for an item in the band
  {$EXTERNALSYM SMDM_HMENU}
  SMDM_HMENU       = $00000002;  // This is for the Band itself
  {$EXTERNALSYM SMDM_TOOLBAR}
  SMDM_TOOLBAR     = $00000004;  // Plain toolbar, not associated with a shell folder or hmenu

type
// Flags (bitmask)
  PSMInfo = ^TSMInfo;
  {$EXTERNALSYM tagSMINFO}
  tagSMINFO = record
    dwMask: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    iIcon: Integer;
  end;
  {$EXTERNALSYM SMINFO}
  SMINFO = tagSMINFO;
  TSMInfo = tagSMINFO;

  PSMCSHChangeNotifyStruct = ^TSMCSHChangeNotifyStruct;
  {$EXTERNALSYM tagSHCSCHANGENOTIFYSTRUCT}
  tagSHCSCHANGENOTIFYSTRUCT = record
    lEvent: Longint;
    pidl1: PItemIDList;
    pidl2: PItemIDList;
  end;
  {$EXTERNALSYM SMCSHCHANGENOTIFYSTRUCT}
  SMCSHCHANGENOTIFYSTRUCT = tagSHCSCHANGENOTIFYSTRUCT;
  TSMCSHChangeNotifyStruct = tagSHCSCHANGENOTIFYSTRUCT;

const
  {$EXTERNALSYM SMIM_TYPE}
  SMIM_TYPE  = $1;
  {$EXTERNALSYM SMIM_FLAGS}
  SMIM_FLAGS = $2;
  {$EXTERNALSYM SMIM_ICON}
  SMIM_ICON  = $4;

  {$EXTERNALSYM SMIT_SEPARATOR}
  SMIT_SEPARATOR = $1;
  {$EXTERNALSYM SMIT_STRING}
  SMIT_STRING    = $2;

  {$EXTERNALSYM SMIF_ICON}
  SMIF_ICON        = $1;
  {$EXTERNALSYM SMIF_ACCELERATOR}
  SMIF_ACCELERATOR = $2;
  {$EXTERNALSYM SMIF_DROPTARGET}
  SMIF_DROPTARGET  = $4;
  {$EXTERNALSYM SMIF_SUBMENU}
  SMIF_SUBMENU     = $8;
  {$EXTERNALSYM SMIF_CHECKED}
  SMIF_CHECKED     = $20;
  {$EXTERNALSYM SMIF_DROPCASCADE}
  SMIF_DROPCASCADE = $40;
  {$EXTERNALSYM SMIF_HIDDEN}
  SMIF_HIDDEN      = $80;
  {$EXTERNALSYM SMIF_DISABLED}
  SMIF_DISABLED    = $100;
  {$EXTERNALSYM SMIF_TRACKPOPUP}
  SMIF_TRACKPOPUP  = $200;
  {$EXTERNALSYM SMIF_DEMOTED}
  SMIF_DEMOTED     = $400;
  {$EXTERNALSYM SMIF_ALTSTATE}
  SMIF_ALTSTATE    = $800;
  {$EXTERNALSYM SMIF_DRAGNDROP}
  SMIF_DRAGNDROP   = $1000;
  {$EXTERNALSYM SMIF_NEW}
  SMIF_NEW         = $2000;

  {$EXTERNALSYM SMC_INITMENU}
  SMC_INITMENU            = $00000001;  // The callback is called to init a menuband
  {$EXTERNALSYM SMC_CREATE}
  SMC_CREATE              = $00000002;
  {$EXTERNALSYM SMC_EXITMENU}
  SMC_EXITMENU            = $00000003;  // The callback is called when menu is collapsing
  {$EXTERNALSYM SMC_GETINFO}
  SMC_GETINFO             = $00000005;  // The callback is called to return DWORD values
  {$EXTERNALSYM SMC_GETSFINFO}
  SMC_GETSFINFO           = $00000006;  // The callback is called to return DWORD values
  {$EXTERNALSYM SMC_GETOBJECT}
  SMC_GETOBJECT           = $00000007;  // The callback is called to get some object
  {$EXTERNALSYM SMC_GETSFOBJECT}
  SMC_GETSFOBJECT         = $00000008;  // The callback is called to get some object
  {$EXTERNALSYM SMC_SFEXEC}
  SMC_SFEXEC              = $00000009;  // The callback is called to execute an shell folder item
  {$EXTERNALSYM SMC_SFSELECTITEM}
  SMC_SFSELECTITEM        = $0000000A;  // The callback is called when an item is selected
  {$EXTERNALSYM SMC_REFRESH}
  SMC_REFRESH             = $00000010;  // Menus have completely refreshed. Reset your state.
  {$EXTERNALSYM SMC_DEMOTE}
  SMC_DEMOTE              = $00000011;  // Demote an item
  {$EXTERNALSYM SMC_PROMOTE}
  SMC_PROMOTE             = $00000012;  // Promote an item, wParam = SMINV_* flag
  {$EXTERNALSYM SMC_DEFAULTICON}
  SMC_DEFAULTICON         = $00000016;  // Returns Default icon location in wParam, index in lParam
  {$EXTERNALSYM SMC_NEWITEM}
  SMC_NEWITEM             = $00000017;  // Notifies item is not in the order stream.
  {$EXTERNALSYM SMC_CHEVRONEXPAND}
  SMC_CHEVRONEXPAND       = $00000019;  // Notifies of a expansion via the chevron
  {$EXTERNALSYM SMC_DISPLAYCHEVRONTIP}
  SMC_DISPLAYCHEVRONTIP   = $0000002A;  // S_OK display, S_FALSE not.
  {$EXTERNALSYM SMC_SETSFOBJECT}
  SMC_SETSFOBJECT         = $0000002D;  // Called to save the passed object
  {$EXTERNALSYM SMC_SHCHANGENOTIFY}
  SMC_SHCHANGENOTIFY      = $0000002E;  // Called when a Change notify is received. lParam points to SMCSHCHANGENOTIFYSTRUCT
  {$EXTERNALSYM SMC_CHEVRONGETTIP}
  SMC_CHEVRONGETTIP       = $0000002F;  // Called to get the chevron tip text. wParam = Tip title, Lparam = TipText Both MAX_PATH
  {$EXTERNALSYM SMC_SFDDRESTRICTED}
  SMC_SFDDRESTRICTED      = $00000030;  // Called requesting if it's ok to drop. wParam = IDropTarget.


const
  {$EXTERNALSYM IID_IShellMenuCallback}
  IID_IShellMenuCallback: TGUID = '{4CA300A1-9B8D-11d1-8B22-00C04FD918D0}';

type
  {$EXTERNALSYM IShellMenuCallback}
  IShellMenuCallback = interface(IUnknown)
  ['{4CA300A1-9B8D-11d1-8B22-00C04FD918D0}']
    function CallbackSM(var psmd: TSMData; uMsg: UINT; wParam: WPARAM;
      lParam: LPARAM): HResult; stdcall;
  end;

type
  {$EXTERNALSYM tagATTACHMENT_PROMPT}
  tagATTACHMENT_PROMPT = DWORD;
  {$EXTERNALSYM ATTACHMENT_PROMPT}
  ATTACHMENT_PROMPT = tagATTACHMENT_PROMPT;
  TAttachmentPrompt = tagATTACHMENT_PROMPT;

const
  {$EXTERNALSYM ATTACHMENT_PROMPT_NONE}
  ATTACHMENT_PROMPT_NONE         = $0;
  {$EXTERNALSYM ATTACHMENT_PROMPT_SAVE}
  ATTACHMENT_PROMPT_SAVE         = $1;
  {$EXTERNALSYM ATTACHMENT_PROMPT_EXEC}
  ATTACHMENT_PROMPT_EXEC         = $2;
  {$EXTERNALSYM ATTACHMENT_PROMPT_EXEC_OR_SAVE}
  ATTACHMENT_PROMPT_EXEC_OR_SAVE = $3;

type
  {$EXTERNALSYM tagATTACHMENT_ACTION}
  tagATTACHMENT_ACTION = DWORD;
  {$EXTERNALSYM ATTACHMENT_ACTION}
  ATTACHMENT_ACTION = tagATTACHMENT_ACTION;
  TAttachmentAction = tagATTACHMENT_ACTION;

const
  {$EXTERNALSYM ATTACHMENT_ACTION_CANCEL}
  ATTACHMENT_ACTION_CANCEL = $0;
  {$EXTERNALSYM ATTACHMENT_ACTION_SAVE}
  ATTACHMENT_ACTION_SAVE   = $1;
  {$EXTERNALSYM ATTACHMENT_ACTION_EXEC}
  ATTACHMENT_ACTION_EXEC   = $2;

  {$EXTERNALSYM IID_IAttachmentExecute}
  IID_IAttachmentExecute: TGUID = '{73db1241-1e85-4581-8e4f-a81e1d0f8c57}';

type
  {$EXTERNALSYM IAttachmentExecute}
  IAttachmentExecute = interface(IUnknown)
  ['{73db1241-1e85-4581-8e4f-a81e1d0f8c57}']
    function SetClientTitle(pszTitle: PWideChar): HResult; stdcall;
    function SetClientGuid(const guid: TGUID): HResult; stdcall;
    function SetLocalPath(pszLocalPath: PWideChar): HResult; stdcall;
    function SetFileName(pszFileName: PWideChar): HResult; stdcall;
    function SetSource(pszSource: PWideChar): HResult; stdcall;
    function SetReferrer(pszReferrer: PWideChar): HResult; stdcall;
    function CheckPolicy: HResult; stdcall;
    function Prompt(hwnd: HWND; prompt: TAttachmentPrompt;
      out paction: TAttachmentAction): HResult; stdcall;
    function Save: HResult; stdcall;
    function Execute(hwnd: HWND; pszVerb: PWideChar;
      var phProcess: THandle): HResult; stdcall;
    function SaveWithUI(hwnd: HWND): HResult; stdcall;
    function ClearClientState: HResult; stdcall;
  end;

const
  {$EXTERNALSYM SMINIT_DEFAULT}
  SMINIT_DEFAULT              = $00000000;  // No Options
  {$EXTERNALSYM SMINIT_RESTRICT_DRAGDROP}
  SMINIT_RESTRICT_DRAGDROP    = $00000002;  // Don't allow Drag and Drop
  {$EXTERNALSYM SMINIT_TOPLEVEL}
  SMINIT_TOPLEVEL             = $00000004;  // This is the top band.
  {$EXTERNALSYM SMINIT_CACHED}
  SMINIT_CACHED               = $00000010;
  {$EXTERNALSYM SMINIT_VERTICAL}
  SMINIT_VERTICAL             = $10000000;  // This is a vertical menu
  {$EXTERNALSYM SMINIT_HORIZONTAL}
  SMINIT_HORIZONTAL           = $20000000;  // This is a horizontal menu    (does not inherit)

  {$EXTERNALSYM ANCESTORDEFAULT}
  ANCESTORDEFAULT             = UINT(-1);

  {$EXTERNALSYM SMSET_TOP}
  SMSET_TOP                   = $10000000;    // Bias this namespace to the top of the menu
  {$EXTERNALSYM SMSET_BOTTOM}
  SMSET_BOTTOM                = $20000000;    // Bias this namespace to the bottom of the menu
  {$EXTERNALSYM SMSET_DONTOWN}
  SMSET_DONTOWN               = $00000001;    // The Menuband doesn't own the non-ref counted object

  {$EXTERNALSYM SMINV_REFRESH}
  SMINV_REFRESH               = $00000001;
  {$EXTERNALSYM SMINV_ID}
  SMINV_ID                    = $00000008;

  {$EXTERNALSYM IID_IShellMenu}
  IID_IShellMenu: TGUID = '{EE1F7637-E138-11d1-8379-00C04FD918D0}';

type
  {$EXTERNALSYM IShellMenu}
  IShellMenu = interface(IUnknown)
  ['{EE1F7637-E138-11d1-8379-00C04FD918D0}']
    function Initialize(psmc: IShellMenuCallback; uId, uIdAncestor: UINT;
      dwFlags: DWORD): HResult; stdcall;
    function GetMenuInfo(out ppsmc: IShellMenuCallback;
      out puId, puIdAncestor: UINT; out pdwFlags: DWORD): HResult; stdcall;
    function SetShellFolder(psf: IShellFolder; pidlFolder: PItemIDList;
      hKey: HKEY; dwFlags: DWORD): HResult; stdcall;
    function GetShellFolder(out pdwFlags: DWORD; ppidl: PPItemIDList;
      const riid: TIID; out ppv): HResult; stdcall;
    function SetMenu(hmenu: HMENU; hwnd: HWND; dwFlags: DWORD): HResult; stdcall;
    function GetMenu(out phmenu: HMENU; out phwnd: HWND;
      out pdwFlags: DWORD): HResult; stdcall;
    function InvalidateItem(var psmd: TSMData;
      dwFlags: DWORD): HResult; stdcall;
    function GetState(out psmd: TSMData): HResult; stdcall;
    function SetMenuToolbar(punk: IUnknown; dwFlags: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM LIBID_ShellObjects}
  LIBID_ShellObjects: TGUID = '{50A7E9B0-70EF-11D1-B75A-00A0C90564FE}';

  {$EXTERNALSYM CLSID_QueryCancelAutoPlay}
  CLSID_QueryCancelAutoPlay: TGUID = '{331F1768-05A9-4ddd-B86E-DAE34DDC998A}';
  {$EXTERNALSYM CLSID_DriveSizeCategorizer}
  CLSID_DriveSizeCategorizer: TGUID = '{94357B53-CA29-4b78-83AE-E8FE7409134F}';
  {$EXTERNALSYM CLSID_DriveTypeCategorizer}
  CLSID_DriveTypeCategorizer: TGUID = '{B0A8F3CF-4333-4bab-8873-1CCB1CADA48B}';
  {$EXTERNALSYM CLSID_FreeSpaceCategorizer}
  CLSID_FreeSpaceCategorizer: TGUID = '{B5607793-24AC-44c7-82E2-831726AA6CB7}';
  {$EXTERNALSYM CLSID_TimeCategorizer}
  CLSID_TimeCategorizer: TGUID = '{3bb4118f-ddfd-4d30-a348-9fb5d6bf1afe}';
  {$EXTERNALSYM CLSID_SizeCategorizer}
  CLSID_SizeCategorizer: TGUID = '{55d7b852-f6d1-42f2-aa75-8728a1b2d264}';
  {$EXTERNALSYM CLSID_AlphabeticalCategorizer}
  CLSID_AlphabeticalCategorizer: TGUID = '{3c2654c6-7372-4f6b-b310-55d6128f49d2}';
  {$EXTERNALSYM CLSID_MergedCategorizer}
  CLSID_MergedCategorizer: TGUID = '{8e827c11-33e7-4bc1-b242-8cd9a1c2b304}';
  {$EXTERNALSYM CLSID_ImageProperties}
  CLSID_ImageProperties: TGUID = '{7ab770c7-0e23-4d7a-8aa2-19bfad479829}';
  {$EXTERNALSYM CLSID_PropertiesUI}
  CLSID_PropertiesUI: TGUID = '{d912f8cf-0396-4915-884e-fb425d32943b}';
  {$EXTERNALSYM CLSID_UserNotification}
  CLSID_UserNotification: TGUID = '{0010890e-8789-413c-adbc-48f5b511b3af}';
  {$EXTERNALSYM CLSID_UserEventTimerCallback}
  CLSID_UserEventTimerCallback: TGUID = '{15fffd13-5140-41b8-b89a-c8d5759cd2b2}';
  {$EXTERNALSYM CLSID_UserEventTimer}
  CLSID_UserEventTimer: TGUID = '{864A1288-354C-4D19-9D68-C2742BB14997}';
  {$EXTERNALSYM CLSID_NetCrawler}
  CLSID_NetCrawler: TGUID = '{601ac3dc-786a-4eb0-bf40-ee3521e70bfb}';
  {$EXTERNALSYM CLSID_CDBurn}
  CLSID_CDBurn: TGUID = '{fbeb8a05-beee-4442-804e-409d6c4515e9}';
  {$EXTERNALSYM CLSID_TaskbarList}
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';
  {$EXTERNALSYM CLSID_WebWizardHost}
  CLSID_WebWizardHost: TGUID = '{c827f149-55c1-4d28-935e-57e47caed973}';
  {$EXTERNALSYM CLSID_PublishDropTarget}
  CLSID_PublishDropTarget: TGUID = '{CC6EEFFB-43F6-46c5-9619-51D571967F7D}';
  {$EXTERNALSYM CLSID_PublishingWizard}
  CLSID_PublishingWizard: TGUID = '{6b33163c-76a5-4b6c-bf21-45de9cd503a1}';
  {$EXTERNALSYM SID_PublishingWizard}
  SID_PublishingWizard: TGUID = '{6b33163c-76a5-4b6c-bf21-45de9cd503a1}';
  {$EXTERNALSYM CLSID_InternetPrintOrdering}
  CLSID_InternetPrintOrdering: TGUID = '{add36aa8-751a-4579-a266-d66f5202ccbb}';
  {$EXTERNALSYM CLSID_FolderViewHost}
  CLSID_FolderViewHost: TGUID = '{20b1cb23-6968-4eb9-b7d4-a66d00d07cee}';
  {$EXTERNALSYM CLSID_NamespaceWalker}
  CLSID_NamespaceWalker: TGUID = '{72eb61e0-8672-4303-9175-f2e4c68b2e7c}';
  {$EXTERNALSYM CLSID_ImageRecompress}
  CLSID_ImageRecompress: TGUID = '{6e33091c-d2f8-4740-b55e-2e11d1477a2c}';
  {$EXTERNALSYM CLSID_TrayBandSiteService}
  CLSID_TrayBandSiteService: TGUID = '{F60AD0A0-E5E1-45cb-B51A-E15B9F8B2934}';
  {$EXTERNALSYM CLSID_PassportWizard}
  CLSID_PassportWizard: TGUID = '{58f1f272-9240-4f51-b6d4-fd63d1618591}';
  {$EXTERNALSYM CLSID_AttachmentServices}
  CLSID_AttachmentServices: TGUID = '{4125dd96-e03a-4103-8f70-e0597d803b9c}';

// -- shlobj.h --

//===========================================================================
//
// Task allocator API
//
//  All the shell extensions MUST use the task allocator (see OLE 2.0
// programming guild for its definition) when they allocate or free
// memory objects (mostly ITEMIDLIST) that are returned across any
// shell interfaces. There are two ways to access the task allocator
// from a shell extension depending on whether or not it is linked with
// OLE32.DLL or not (purely for efficiency).
//
// (1) A shell extension which calls any OLE API (i.e., linked with
//  OLE32.DLL) should call OLE's task allocator (by retrieving
//  the task allocator by calling CoGetMalloc API).
//
// (2) A shell extension which does not call any OLE API (i.e., not linked
//  with OLE32.DLL) should call the shell task allocator API (defined
//  below), so that the shell can quickly loads it when OLE32.DLL is not
//  loaded by any application at that TPoint.
//
// Notes:
//  In next version of Windowso release, SHGetMalloc will be replaced by
// the following macro.
//
// #define SHGetMalloc(ppmem)   CoGetMalloc(MEMCTX_TASK, ppmem)
//
//===========================================================================

{$EXTERNALSYM SHGetMalloc}
function SHGetMalloc(out ppMalloc: IMalloc): HResult; stdcall;

{$EXTERNALSYM SHAlloc}
function SHAlloc(cb: Cardinal): Pointer; stdcall;
{$EXTERNALSYM SHFree}
procedure SHFree(pv: Pointer); stdcall;


//===========================================================================
//
// IContextMenu interface
//
// [OverView]
//
//  The shell uses the IContextMenu interface in following three cases.
//
// case-1: The shell is loading context menu extensions.
//
//   When the user clicks the right mouse button on an item within the shell's
//  name space (i.g., file, directory, server, work-group, etc.), it creates
//  the default context menu for its type, then loads context menu extensions
//  that are registered for that type (and its base type) so that they can
//  add extra menu items. Those context menu extensions are registered at
//  HKCR\{ProgID}\shellex\ContextMenuHandlers.
//
// case-2: The shell is retrieving a context menu of sub-folders in extended
//   name-space.
//
//   When the explorer's name space is extended by name space extensions,
//  the shell calls their IShellFolder::GetUIObjectOf to get the IContextMenu
//  objects when it creates context menus for folders under those extended
//  name spaces.
//
// case-3: The shell is loading non-default drag and drop handler for directories.
//
//   When the user performed a non-default drag and drop onto one of file
//  system folders (i.e., directories), it loads shell extensions that are
//  registered at HKCR\{ProgID}\DragDropHandlers.
//
//
// [Member functions]
//
//
// IContextMenu::QueryContextMenu
//
//   This member function may insert one or more menuitems to the specified
//  menu (hmenu) at the specified location (indexMenu which is never be -1).
//  The IDs of those menuitem must be in the specified range (idCmdFirst and
//  idCmdLast). It returns the maximum menuitem ID offset (ushort) in the
//  'code' field (low Word) of the scode.
//
//   The uFlags specify the context. It may have one or more of following
//  flags.
//
//  CMF_DEFAULTONLY: This flag is passed if the user is invoking the default
//   action (typically by double-clicking, case 1 and 2 only). Context menu
//   extensions (case 1) should not add any menu items, and returns NOERROR.
//
//  CMF_VERBSONLY: The explorer passes this flag if it is constructing
//   a context menu for a short-cut object (case 1 and case 2 only). If this
//   flag is passed, it should not add any menu-items that is not appropriate
//   from a short-cut.
//    A good example is the "Delete" menuitem, which confuses the user
//   because it is not clear whether it deletes the link source item or the
//   link itself.
//
//  CMF_EXPLORER: The explorer passes this flag if it has the left-side pane
//   (case 1 and 2 only). Context menu extensions should ignore this flag.
//
//   High Word (16-bit) are reserved for context specific communications
//  and the rest of flags (13-bit) are reserved by the system.
//
//
// IContextMenu::InvokeCommand
//
//   This member is called when the user has selected one of menuitems that
//  are inserted by previous QueryContextMenu member. In this case, the
//  LOWORD(lpici->lpVerb) contains the menuitem ID offset (menuitem ID -
//  idCmdFirst).
//
//   This member function may also be called programmatically. In such a case,
//  lpici->lpVerb specifies the canonical name of the command to be invoked,
//  which is typically retrieved by GetCommandString member previously.
//
//  Parameters in lpci:
//    cbSize -- Specifies the size of this structure (sizeof(*lpci))
//    hwnd   -- Specifies the owner window for any message/dialog box.
//    fMask  -- Specifies whether or not dwHotkey/hIcon paramter is valid.
//    lpVerb -- Specifies the command to be invoked.
//    lpParameters -- Parameters (optional)
//    lpDirectory  -- Working directory (optional)
//    nShow -- Specifies the flag to be passed to ShowWindow (SW_*).
//    dwHotKey -- Hot key to be assigned to the app after invoked (optional).
//    hIcon -- Specifies the icon (optional).
//    hMonitor -- Specifies the default monitor (optional).
//
//
// IContextMenu::GetCommandString
//
//   This member function is called by the explorer either to get the
//  canonical (language independent) command name (uFlags == GCS_VERB) or
//  the help text ((uFlags & GCS_HELPTEXT) != 0) for the specified command.
//  The retrieved canonical string may be passed to its InvokeCommand
//  member function to invoke a command programmatically. The explorer
//  displays the help texts in its status bar; therefore, the length of
//  the help text should be reasonably short (<40 characters).
//
//  Parameters:
//   idCmd -- Specifies menuitem ID offset (from idCmdFirst)
//   uFlags -- Either GCS_VERB or GCS_HELPTEXT
//   pwReserved -- Reserved (must pass NULL when calling, must ignore when called)
//   pszName -- Specifies the string buffer.
//   cchMax -- Specifies the size of the string buffer.
//
//===========================================================================

// QueryContextMenu uFlags
const
  {$EXTERNALSYM CMF_NORMAL}
  CMF_NORMAL              = $00000000;
  {$EXTERNALSYM CMF_DEFAULTONLY}
  CMF_DEFAULTONLY         = $00000001;
  {$EXTERNALSYM CMF_VERBSONLY}
  CMF_VERBSONLY           = $00000002;
  {$EXTERNALSYM CMF_EXPLORE}
  CMF_EXPLORE             = $00000004;
  {$EXTERNALSYM CMF_NOVERBS}
  CMF_NOVERBS             = $00000008;
  {$EXTERNALSYM CMF_CANRENAME}
  CMF_CANRENAME           = $00000010;
  {$EXTERNALSYM CMF_NODEFAULT}
  CMF_NODEFAULT           = $00000020;
  {$EXTERNALSYM CMF_INCLUDESTATIC}
  CMF_INCLUDESTATIC       = $00000040;
  {$EXTERNALSYM CMF_EXTENDEDVERBS}
  CMF_EXTENDEDVERBS       = $00000100;      // rarely used verbs
  {$EXTERNALSYM CMF_RESERVED}
  CMF_RESERVED            = $FFFF0000;      // View specific

// GetCommandString uFlags
  {$EXTERNALSYM GCS_VERBA}
  GCS_VERBA        = $00000000;    // canonical verb
  {$EXTERNALSYM GCS_HELPTEXTA}
  GCS_HELPTEXTA    = $00000001;    // help text (for status bar)
  {$EXTERNALSYM GCS_VALIDATEA}
  GCS_VALIDATEA    = $00000002;    // validate command exists
  {$EXTERNALSYM GCS_VERBW}
  GCS_VERBW        = $00000004;    // canonical verb (unicode)
  {$EXTERNALSYM GCS_HELPTEXTW}
  GCS_HELPTEXTW    = $00000005;    // help text (unicode version)
  {$EXTERNALSYM GCS_VALIDATEW}
  GCS_VALIDATEW    = $00000006;    // validate command exists (unicode)
  {$EXTERNALSYM GCS_UNICODE}
  GCS_UNICODE      = $00000004;    // for bit testing - Unicode string

  {$EXTERNALSYM GCS_VERB}
  GCS_VERB         = GCS_VERBA;
  {$EXTERNALSYM GCS_HELPTEXT}
  GCS_HELPTEXT     = GCS_HELPTEXTA;
  {$EXTERNALSYM GCS_VALIDATE}
  GCS_VALIDATE     = GCS_VALIDATEA;

  {$EXTERNALSYM CMDSTR_NEWFOLDERA}
  CMDSTR_NEWFOLDERA   = 'NewFolder';
  {$EXTERNALSYM CMDSTR_VIEWLISTA}
  CMDSTR_VIEWLISTA    = 'ViewList';
  {$EXTERNALSYM CMDSTR_VIEWDETAILSA}
  CMDSTR_VIEWDETAILSA = 'ViewDetails';
  {$EXTERNALSYM CMDSTR_NEWFOLDERW}
  CMDSTR_NEWFOLDERW   = 'NewFolder';
  {$EXTERNALSYM CMDSTR_VIEWLISTW}
  CMDSTR_VIEWLISTW    = 'ViewList';
  {$EXTERNALSYM CMDSTR_VIEWDETAILSW}
  CMDSTR_VIEWDETAILSW = 'ViewDetails';

  {$EXTERNALSYM CMDSTR_NEWFOLDER}
  CMDSTR_NEWFOLDER    = CMDSTR_NEWFOLDERA;
  {$EXTERNALSYM CMDSTR_VIEWLIST}
  CMDSTR_VIEWLIST     = CMDSTR_VIEWLISTA;
  {$EXTERNALSYM CMDSTR_VIEWDETAILS}
  CMDSTR_VIEWDETAILS  = CMDSTR_VIEWDETAILSA;

  {$EXTERNALSYM CMIC_MASK_HOTKEY}
  CMIC_MASK_HOTKEY        = SEE_MASK_HOTKEY;
  {$EXTERNALSYM CMIC_MASK_ICON}
  CMIC_MASK_ICON          = SEE_MASK_ICON;
  {$EXTERNALSYM CMIC_MASK_FLAG_NO_UI}
  CMIC_MASK_FLAG_NO_UI    = SEE_MASK_FLAG_NO_UI;
  {$EXTERNALSYM CMIC_MASK_UNICODE}
  CMIC_MASK_UNICODE       = SEE_MASK_UNICODE;
  {$EXTERNALSYM CMIC_MASK_NO_CONSOLE}
  CMIC_MASK_NO_CONSOLE    = SEE_MASK_NO_CONSOLE;
//  {$EXTERNALSYM CMIC_MASK_HASLINKNAME}
//  CMIC_MASK_HASLINKNAME   = SEE_MASK_HASLINKNAME; -- not defined in ShellAPI
//  {$EXTERNALSYM CMIC_MASK_FLAG_SEP_VDM}
//  CMIC_MASK_FLAG_SEP_VDM  = SEE_MASK_FLAG_SEPVDM; -- not defined in ShellAPI
//  {$EXTERNALSYM CMIC_MASK_HASTITLE}
//  CMIC_MASK_HASTITLE      = SEE_MASK_HASTITLE;    -- not defined in ShellAPI
  {$EXTERNALSYM CMIC_MASK_ASYNCOK}
  CMIC_MASK_ASYNCOK       = SEE_MASK_ASYNCOK;
  {$EXTERNALSYM CMIC_MASK_NOZONECHECKS}
  CMIC_MASK_NOZONECHECKS  = SEE_MASK_NOZONECHECKS;

  {$EXTERNALSYM CMIC_MASK_SHIFT_DOWN}
  CMIC_MASK_SHIFT_DOWN    = $10000000;
  {$EXTERNALSYM CMIC_MASK_CONTROL_DOWN}
  CMIC_MASK_CONTROL_DOWN  = $40000000;
  {$EXTERNALSYM CMIC_MASK_FLAG_LOG_USAGE}
  CMIC_MASK_FLAG_LOG_USAGE = SEE_MASK_FLAG_LOG_USAGE;

  {$EXTERNALSYM CMIC_MASK_PTINVOKE}
  CMIC_MASK_PTINVOKE      = $20000000;

//NOTE: When SEE_MASK_HMONITOR is set, hIcon is treated as hMonitor
type
  PCMInvokeCommandInfo = ^TCMInvokeCommandInfo;
  {$EXTERNALSYM _CMINVOKECOMMANDINFO}
  _CMINVOKECOMMANDINFO = record
    cbSize: DWORD;        // sizeof(CMINVOKECOMMANDINFO)
    fMask: DWORD;         // any combination of CMIC_MASK_*
    hwnd: HWND;           // might be NULL (indicating no owner window)
    lpVerb: PAnsiChar;       // either a string or MAKEINTRESOURCE(idOffset)
    lpParameters: PAnsiChar; // might be NULL (indicating no parameter)
    lpDirectory: PAnsiChar;  // might be NULL (indicating no specific directory)
    nShow: Integer;       // one of SW_ values for ShowWindow() API
    dwHotKey: DWORD;
    hIcon: THandle;
  end;
  {$EXTERNALSYM CMINVOKECOMMANDINFO}
  CMINVOKECOMMANDINFO = _CMINVOKECOMMANDINFO;
  TCMInvokeCommandInfo = _CMINVOKECOMMANDINFO;

  PCMInvokeCommandInfoEx = ^TCMInvokeCommandInfoEx;
  {$EXTERNALSYM _CMInvokeCommandInfoEx}
  _CMInvokeCommandInfoEx = record
    cbSize: DWORD;           // must be sizeof(CMINVOKECOMMANDINFOEX)
    fMask: DWORD;            // any combination of CMIC_MASK_*
    hwnd: HWND;              // might be NULL (indicating no owner window)
    lpVerb: PAnsiChar;          // either a string or MAKEINTRESOURCE(idOffset)
    lpParameters: PAnsiChar;    // might be NULL (indicating no parameter)
    lpDirectory: PAnsiChar;     // might be NULL (indicating no specific directory)
    nShow: Integer;          // one of SW_ values for ShowWindow() API
    dwHotKey: DWORD;
    hIcon: THandle;

    lpTitle: PAnsiChar;         // For CreateProcess-StartupInfo.lpTitle
    lpVerbW: PWideChar;        // Unicode verb (for those who can use it)
    lpParametersW: PWideChar;  // Unicode parameters (for those who can use it)
    lpDirectoryW: PWideChar;   // Unicode directory (for those who can use it)
    lpTitleW: PWideChar;       // Unicode title (for those who can use it)
    ptInvoke: TPoint;        // TPoint where it's invoked
  end;
  {$EXTERNALSYM CMINVOKECOMMANDINFOEX}
  CMINVOKECOMMANDINFOEX = _CMInvokeCommandInfoEx;
  TCMInvokeCommandInfoEx = _CMInvokeCommandInfoEx;

  {$EXTERNALSYM IContextMenu}
  IContextMenu = interface(IUnknown)
  ['{000214E4-0000-0000-C000-000000000046}']
    function QueryContextMenu(hmenu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: PAnsiChar; cchMax: UINT): HResult; stdcall;
  end;

//
// IContextMenu2 (IContextMenu with one new member)
//
// IContextMenu2::HandleMenuMsg
//
//  This function is called, if the client of IContextMenu is aware of
// IContextMenu2 interface and receives one of following messages while
// it is calling TrackPopupMenu (in the window proc of hwnd):
//      WM_INITPOPUP, WM_DRAWITEM and WM_MEASUREITEM
//  The callee may handle these messages to draw owner draw menuitems.
//

  {$EXTERNALSYM IContextMenu2}
  IContextMenu2 = interface(IContextMenu)
  ['{000214F4-0000-0000-C000-000000000046}']
    function HandleMenuMsg(uMsg: UINT;
      wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
  end;

//
// IContextMenu3 (IContextMenu with one new member)
//
// IContextMenu3::HandleMenuMsg2
//
//  This function is called, if the client of IContextMenu is aware of
// IContextMenu3 interface and receives a menu message while
// it is calling TrackPopupMenu (in the window proc of hwnd):
//

  {$EXTERNALSYM IContextMenu3}
  IContextMenu3 = interface(IContextMenu2)
  ['{BCFCE0A0-EC17-11D0-8D10-00A0C90F2719}']
    // *** IUnknown methods ***
    function HandleMenuMsg2(uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
      out plResult: LRESULT): HResult; stdcall;
  end;

// DESCRIPTION: PERSIST_FOLDER_TARGET_INFO
//    This stucture is used for Folder Shortcuts which allow the shell to
// have a file system folder act like another area in the name space.
// One of pidlTargetFolder, szTargetParsingName, or csidl needs to
// specify the destination name space.
//
// pidlTargetFolder: This is a full pidl to the target folder.  Can be NULL in the IPersistFolder3::InitializeEx()
//                   call but not in the GetFolderTargetInfo() return structure.
// szTargetParsingName: Empty string if not specified. Ortherwise, it is the parsible name
//                       to the target.  This name can be parsed by IShellFolder::
//                       ParsedName() from the desktop.
// szNetworkProvider: Can be an empty string.  If not empty, it specifies the type of network
//                    provider that will be used when binding to the target.  This is used
//                    for performance optimizations for the WNet APIs.
// dwAttributes: -1 if not known.  These are the SFGAO_ flags for IShellFolder::GetAttributesOf()
// csidl: This is -1 if it's not used.  This can be used instead of pidlTargetFolder or
//        szTargetParsingName to indicate the TargetFolder.  See the list of CSIDL_ folders
//        below.  CSIDL_FLAG_PFTI_TRACKTARGET means that the IShellFolder's target folder
//        should change if the user changes the target of the underlying CSIDL value.
//        You can also pass CSIDL_FLAG_CREATE to indicate that the target folder
//        should be created if it does not exist.  No other CSIDL_FLAG_* values are supported.


type
  PPersistFolderTargetInfo = ^TPersistFolderTargetInfo;
  {$EXTERNALSYM PERSIST_FOLDER_TARGET_INFO}
  PERSIST_FOLDER_TARGET_INFO = record
    pidlTargetFolder: PItemIDList;               // pidl for the folder we want to intiailize
    szTargetParsingName: array[0..MAX_PATH - 1] of WideChar;  // optional parsing name for the target
    szNetworkProvider: array[0..MAX_PATH - 1] of WideChar;    // optional network provider
    dwAttributes: DWORD;                   // optional FILE_ATTRIBUTES_ flags (-1 if not used)
    csidl: Integer;                          // optional folder index (SHGetFolderPath()) -1 if not used
  end;
  TPersistFolderTargetInfo = PERSIST_FOLDER_TARGET_INFO;

// DESCRIPTION: IPersistFolder3
//    This interface is implemented by an IShellFolder object that wants non-default
// handling of Folder Shortcuts.  In general, shell name space extensions should use
// pidlRoot (the alias pidl) as their location in the name space and pass it to public
// APIs, such as ShellExecute().  The one exception is that pidlTarget should be used
// when sending ChangeNotifies or registering to listen for change notifies
// (see SFVM_GETNOTIFY).
//
// InitializeEx: This method initializes an IShellFolder and specifies where
//               it is rooted in the name space.
//      pbc: May be NULL.
//      pidlRoot: This is the same parameter as IPersistFolder::Initialize(). Caller allocates
//                and frees this parameter.
//      ppfti: May be NULL, in which case this is the same as a call to IPersistFolder::Initialize().
//             Otherwise this is a Folder Shortcut and this structure specifies the target
//             folder and it's attributes.
// GetFolderTargetInfo: This is used by the caller to find information about
//             the folder shortcut.  This structure may not be initialized by the caller,
//             so the callee needs to initialize every member.  The callee allocates
//             pidlTargetFolder and the caller will free it.  Filling in pidlTargetFolder is
//             ALWAYS required.

  {$EXTERNALSYM IPersistFolder3}
  IPersistFolder3 = interface(IPersistFolder2)
  ['{CEF04FDF-FE72-11D2-87A5-00C04F6837CF}']
    function InitializeEx(pbc: IBindCtx; pidlRoot: PItemIDList;
      const ppfti: TPersistFolderTargetInfo): HResult; stdcall;
    function GetFolderTargetInfo(
      out ppfti: TPersistFolderTargetInfo): HResult; stdcall;
  end;

//

//===========================================================================
//
// IExtractIcon interface
//
//  This interface is used in two different places in the shell.
//
// Case-1: Icons of sub-folders for the scope-pane of the explorer.
//
//  It is used by the explorer to get the "icon location" of
// sub-folders from each shell folders. When the user expands a folder
// in the scope pane of the explorer, the explorer does following:
//  (1) binds to the folder (gets IShellFolder),
//  (2) enumerates its sub-folders by calling its EnumObjects member,
//  (3) calls its GetUIObjectOf member to get IExtractIcon interface
//     for each sub-folders.
//  In this case, the explorer uses only IExtractIcon::GetIconLocation
// member to get the location of the appropriate icon. An icon location
// always consists of a file name (typically DLL or EXE) and either an icon
// resource or an icon index.
//
//
// Case-2: Extracting an icon image from a file
//
//  It is used by the shell when it extracts an icon image
// from a file. When the shell is extracting an icon from a file,
// it does following:
//  (1) creates the icon extraction handler object (by getting its CLSID
//     under the {ProgID}\shell\ExtractIconHanler key and calling
//     CoCreateInstance requesting for IExtractIcon interface).
//  (2) Calls IExtractIcon::GetIconLocation.
//  (3) Then, calls IExtractIcon::ExtractIcon with the location/index pair.
//  (4) If (3) returns NOERROR, it uses the returned icon.
//  (5) Otherwise, it recursively calls this logic with new location
//     assuming that the location string contains a fully qualified path name.
//
//  From extension programmer's TPoint of view, there are only two cases
// where they provide implementations of IExtractIcon:
//  Case-1) providing explorer extensions (i.e., IShellFolder).
//  Case-2) providing per-instance icons for some types of files.
//
// Because Case-1 is described above, we'll explain only Case-2 here.
//
// When the shell is about display an icon for a file, it does following:
//  (1) Finds its ProgID and ClassID.
//  (2) If the file has a ClassID, it gets the icon location string from the
//    "DefaultIcon" key under it. The string indicates either per-class
//    icon (e.g., "FOOBAR.DLL,2") or per-instance icon (e.g., "%1,1").
//  (3) If a per-instance icon is specified, the shell creates an icon
//    extraction handler object for it, and extracts the icon from it
//    (which is described above).
//
//  It is important to note that the shell calls IExtractIcon::GetIconLocation
// first, then calls IExtractIcon::Extract. Most application programs
// that support per-instance icons will probably store an icon location
// (DLL/EXE name and index/id) rather than an icon image in each file.
// In those cases, a programmer needs to implement only the GetIconLocation
// member and it Extract member simply returns S_FALSE. They need to
// implement Extract member only if they decided to store the icon images
// within files themselved or some other database (which is very rare).
//
//
//
// [Member functions]
//
//
// IExtractIcon::GetIconLocation
//
//  This function returns an icon location.
//
//  Parameters:
//   uFlags     [in]  -- Specifies if it is opened or not (GIL_OPENICON or 0)
//   szIconFile [out] -- Specifies the string buffer buffer for a location name.
//   cchMax     [in]  -- Specifies the size of szIconFile (almost always MAX_PATH)
//   piIndex    [out] -- Sepcifies the address of UINT for the index.
//   pwFlags    [out] -- Returns GIL_* flags
//  Returns:
//   NOERROR, if it returns a valid location; S_FALSE, if the shell use a
//   default icon.
//
//  Notes: The location may or may not be a path to a file. The caller can
//   not assume anything unless the subsequent Extract member call returns
//   S_FALSE.
//
//   if the returned location is not a path to a file, GIL_NOTFILENAME should
//   be set in the returned flags.
//
// IExtractIcon::Extract
//
//  This function extracts an icon image from a specified file.
//
//  Parameters:
//   pszFile [in] -- Specifies the icon location (typically a path to a file).
//   nIconIndex [in] -- Specifies the icon index.
//   phiconLarge [out] -- Specifies the HICON variable for large icon.
//   phiconSmall [out] -- Specifies the HICON variable for small icon.
//   nIconSize [in] -- Specifies the size icon required (size of large icon)
//                     LOWORD is the requested large icon size
//                     HIWORD is the requested small icon size
//  Returns:
//   NOERROR, if it extracted the from the file.
//   S_FALSE, if the caller should extract from the file specified in the
//           location.
//
//===========================================================================

const
// GetIconLocation() input flags

  {$EXTERNALSYM GIL_OPENICON}
  GIL_OPENICON     = $0001;      // allows containers to specify an "open" look
  {$EXTERNALSYM GIL_FORSHELL}
  GIL_FORSHELL     = $0002;      // icon is to be displayed in a ShellFolder
  {$EXTERNALSYM GIL_ASYNC}
  GIL_ASYNC        = $0020;      // this is an async extract, return E_PENDING
  {$EXTERNALSYM GIL_DEFAULTICON}
  GIL_DEFAULTICON  = $0040;      // get the default icon location if the final one takes too long to get
  {$EXTERNALSYM GIL_FORSHORTCUT}
  GIL_FORSHORTCUT  = $0080;      // the icon is for a shortcut to the object

// GetIconLocation() return flags

  {$EXTERNALSYM GIL_SIMULATEDOC}
  GIL_SIMULATEDOC  = $0001;      // simulate this document icon for this
  {$EXTERNALSYM GIL_PERINSTANCE}
  GIL_PERINSTANCE  = $0002;      // icons from this class are per instance (each file has its own)
  {$EXTERNALSYM GIL_PERCLASS}
  GIL_PERCLASS     = $0004;      // icons from this class per class (shared for all files of this type)
  {$EXTERNALSYM GIL_NOTFILENAME}
  GIL_NOTFILENAME  = $0008;      // location is not a filename, must call ::ExtractIcon
  {$EXTERNALSYM GIL_DONTCACHE}
  GIL_DONTCACHE    = $0010;      // this icon should not be cached

type
  {$EXTERNALSYM IExtractIconA}
  IExtractIconA = interface(IUnknown)     // exic
  ['{000214EB-0000-0000-C000-000000000046}']
    function GetIconLocation(uFlags: UINT; szIconFile: PAnsiChar; cchMax: UINT;
      out piIndex: Integer; out pwFlags: UINT): HResult; stdcall;
    function Extract(pszFile: PAnsiChar; nIconIndex: UINT;
      out phiconLarge, phiconSmall: HICON; nIconSize: UINT): HResult; stdcall;
  end;

  {$EXTERNALSYM IExtractIconW}
  IExtractIconW = interface(IUnknown)     // exic
  ['{000214FA-0000-0000-C000-000000000046}']
    function GetIconLocation(uFlags: UINT; szIconFile: PWideChar; cchMax: UINT;
      out piIndex: Integer; out pwFlags: UINT): HResult; stdcall;
    function Extract(pszFile: PWideChar; nIconIndex: UINT;
      out phiconLarge, phiconSmall: HICON; nIconSize: UINT): HResult; stdcall;
  end;

  {$EXTERNALSYM IExtractIcon}
  IExtractIcon = IExtractIconA;

//===========================================================================
//
// IShellIcon Interface
//
// used to get a icon index for a IShellFolder object.
//
// this interface can be implemented by a IShellFolder, as a quick way to
// return the icon for a object in the folder.
//
// a instance of this interface is only created once for the folder, unlike
// IExtractIcon witch is created once for each object.
//
// if a ShellFolder does not implement this interface, the standard
// GetUIObject(....IExtractIcon) method will be used to get a icon
// for all objects.
//
// the following standard imagelist indexs can be returned:
//
//      0   document (blank page) (not associated)
//      1   document (with stuff on the page)
//      2   application (exe, com, bat)
//      3   folder (plain)
//      4   folder (open)
//
// IShellIcon:GetIconOf(pidl, flags, lpIconIndex)
//
//      pidl            object to get icon for.
//      flags           GIL_* input flags (GIL_OPEN, ...)
//      lpIconIndex     place to return icon index.
//
//  returns:
//      NOERROR, if lpIconIndex contains the correct system imagelist index.
//      S_FALSE, if unable to get icon for this object, go through
//               GetUIObject, IExtractIcon, methods.
//
//===========================================================================

  {$EXTERNALSYM IShellIcon}
  IShellIcon = interface(IUnknown)      // shi
  ['{000214E5-0000-0000-C000-000000000046}']
    function GetIconOf(pidl: PItemIDList; flags: UINT;
      out lpIconIndex: Integer): HResult; stdcall;
  end;

//===========================================================================
//
// IShellIconOverlayIdentifier
//
// Used to identify a file as a member of the group of files that have this specific
// icon overlay
//
// Users can create new IconOverlayIdentifiers and place them in the following registry
// location together with the Icon overlay image and their priority.
// HKEY_LOCAL_MACHINE "Software\\Microsoft\\Windows\\CurrentVersion\\ShellIconOverlayIdentifiers"
//
// The shell will enumerate through all IconOverlayIdentifiers at start, and prioritize
// them according to internal rules, in case the internal rules don't apply, we use their
// input priority
//
// IShellIconOverlayIdentifier:IsMemberOf(PWideChar pwszPath, DWORD dwAttrib)
//      pwszPath        full path of the file
//      dwAttrib        attribute of this file
//
//  returns:
//      S_OK,    if the file is a member
//      S_FALSE, if the file is not a member
//      E_FAIL,  if the operation failed due to bad WIN32_FIND_DATA
//
// IShellIconOverlayIdentifier::GetOverlayInfo(PWideChar pwszIconFile, int * pIndex, DWORD * dwFlags): HResult; stdcall;
//      pszIconFile    the path of the icon file
//      pIndex         Depend on the flags, this could contain the IconIndex
//      dwFlags        defined below
//
// IShellIconOverlayIdentifier::GetPriority(int * pIPriority): HResult; stdcall;
//      pIPriority     the priority of this Overlay Identifier
//
//===========================================================================

  {$EXTERNALSYM IShellIconOverlayIdentifier}
  IShellIconOverlayIdentifier = interface(IUnknown)
  ['{0C6C4200-C589-11D0-999A-00C04FD655E1}']
    function IsMemberOf(pwszPath: PWideChar; dwAttrib: DWORD): HResult; stdcall;
    function GetOverlayInfo(pwszIconFile: PWideChar; cchMax: Integer;
      out pIndex: Integer; out pdwFlags: DWORD): HResult; stdcall;
    function GetPriority(out pIPriority: Integer): HResult; stdcall;
  end;

const
  {$EXTERNALSYM ISIOI_ICONFILE}
  ISIOI_ICONFILE  = $00000001;          // path is returned through pwszIconFile
  {$EXTERNALSYM ISIOI_ICONINDEX}
  ISIOI_ICONINDEX = $00000002;          // icon index in pwszIconFile is returned through pIndex

//===========================================================================
//
// IShellIconOverlayManager
//
// Used to return the icon overlay information including OverlayIndex, Image Index or Priority for an IShellFolder object.
//
// IShellIconOverlayManager:GetFileOverlayInfo(PWideChar pwszPath, DWORD dwAttrib, int * pIndex, DWORD dwflags)
//      pwszPath        full path of the file
//      dwAttrib        attribute of this file
//      pIndex          pointer to the Icon Index in the system image list
//      pOverlayIndex   pointer to the OverlayIndex in the system image list
//      pPriority       pointer to the Priority of this overlay
// IShellIconOverlayManager:GetReservedOverlayInfo(PWideChar pwszPath, DWORD dwAttrib, int * pIndex, DWORD dwflags, int iReservedID)
//      iReservedID     reserved icon overlay id
//  returns:
//      S_OK,  if the index of an Overlay is found
//      S_FALSE, if no Overlay exists for this file
//      E_FAIL, if lpfd is bad
// IShellIconOverlayManager:RefreshOverlayImages(DWORD dwFlags)
//      This will refresh the overlay cache, depends on the dwFlags passed in
//      It will reload the icons into the imagelist, when passed SIOM_ICONINDEX
// IShellIconOverlayManager::LoadNonloadedOverlayIdentifiers()
//      This method loads any registered overlay identifiers (handlers) that
//      are not currently loaded.
// IShellIconOverlayManager::OverlayIndexFromImageIndex(int iImage, int *piIndex, BOOL fAdd)
//      iImage          existing shell image list index to look for
//      piIndex         returned overlay index
//      fAdd            Add image if not already present?
//===========================================================================

type
  {$EXTERNALSYM IShellIconOverlayManager}
  IShellIconOverlayManager = interface(IUnknown)
  ['{F10B5E34-DD3B-42A7-AA7D-2F4EC54BB09B}']
    function GetFileOverlayInfo(pwszPath: PWideChar; dwAttrib: DWORD;
      out pIndex: Integer; dwflags: DWORD): HResult; stdcall;
    function GetReservedOverlayInfo(pwszPath: PWideChar; dwAttrib: DWORD;
      out pIndex: Integer; dwflags: DWORD;
      iReservedID: Integer): HResult; stdcall;
    function RefreshOverlayImages(dwFlags: DWORD): HResult; stdcall;
    function LoadNonloadedOverlayIdentifiers: HResult; stdcall;
    function OverlayIndexFromImageIndex(iImage: Integer;
      out piIndex: Integer; fAdd: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM SIOM_OVERLAYINDEX}
  SIOM_OVERLAYINDEX         = 1;
  {$EXTERNALSYM SIOM_ICONINDEX}
  SIOM_ICONINDEX            = 2;
//  SIOM_PRIORITY           = 3;
  {$EXTERNALSYM SIOM_RESERVED_SHARED}
  SIOM_RESERVED_SHARED      = 0;
  {$EXTERNALSYM SIOM_RESERVED_LINK}
  SIOM_RESERVED_LINK        = 1;
  {$EXTERNALSYM SIOM_RESERVED_SLOWFILE}
  SIOM_RESERVED_SLOWFILE    = 2;

//===========================================================================
//
// IShellIconOverlay
//
// Used to return the icon overlay index or its icon index for an IShellFolder object,
// this is always implemented with IShellFolder
//
// IShellIconOverlay:GetOverlayIndex(PItemIDList pidl, DWORD * pdwIndex)
//      pidl            object to identify icon overlay for.
//      pdwIndex        the Overlay Index in the system image list
//
// IShellIconOverlay:GetOverlayIconIndex(PItemIDList pidl, DWORD * pdwIndex)
//      pdwIconIndex    the Overlay Icon index in the system image list
// This method is only used for those who are interested in seeing the real bits
// of the Overlay Icon
//
//  returns:
//      S_OK,  if the index of an Overlay is found
//      S_FALSE, if no Overlay exists for this file
//      E_FAIL, if pidl is bad
//
//===========================================================================

type
  {$EXTERNALSYM IShellIconOverlay}
  IShellIconOverlay = interface(IUnknown)
  ['{7D688A70-C613-11D0-999B-00C04FD655E1}']
    function GetOverlayIndex(pidl: PItemIDList;
      out pIndex: Integer): HResult; stdcall;
    function GetOverlayIconIndex(pidl: PItemIDList;
      out pIconIndex: Integer): HResult; stdcall;
  end;

const
  {$EXTERNALSYM OI_ASYNC}
  OI_ASYNC = $FFFFEEEE;

//-------------------------------------------------------------------------
//
// SHGetIconOverlayIndex
//
// This function takes the path and icon/res id to the icon and convert it into
// an overlay index in the system image list.
// Note: there are totally only 15 slots for system image overlays, some of which
// was reserved by the system, or taken by the overlayidentifiers, so it's possible
// that this function would fail and return -1;
//
// To get the default overlays in the system, such as the share hand, link shortcut
// and slow files, pass NULL as the file name, then the IDO_SHGIOI_* flags as the icon index
//-------------------------------------------------------------------------

const
  {$EXTERNALSYM IDO_SHGIOI_SHARE}
  IDO_SHGIOI_SHARE    = $0FFFFFFF;
  {$EXTERNALSYM IDO_SHGIOI_LINK}
  IDO_SHGIOI_LINK     = $0FFFFFFE;
  {$EXTERNALSYM IDO_SHGIOI_SLOWFILE}
  IDO_SHGIOI_SLOWFILE = $FFFFFFFD;

{$EXTERNALSYM SHGetIconOverlayIndexA}
function SHGetIconOverlayIndexA(pszIconPath: PAnsiChar;
  iIconIndex: Integer): Integer; stdcall;
{$EXTERNALSYM SHGetIconOverlayIndexW}
function SHGetIconOverlayIndexW(pszIconPath: PWideChar;
  iIconIndex: Integer): Integer; stdcall;
{$EXTERNALSYM SHGetIconOverlayIndex}
function SHGetIconOverlayIndex(pszIconPath: PTSTR;
  iIconIndex: Integer): Integer; stdcall;

// IShellLinkDataList::GetFlags()/SetFlags()
type
  {$EXTERNALSYM SHELL_LINK_DATA_FLAGS}
  SHELL_LINK_DATA_FLAGS = DWORD;
  TShellLinkDataFlags = DWORD;

const
  {$EXTERNALSYM SLDF_HAS_ID_LIST}
  SLDF_HAS_ID_LIST        = $00000001;   // Shell link saved with ID list
  {$EXTERNALSYM SLDF_HAS_LINK_INFO}
  SLDF_HAS_LINK_INFO      = $00000002;   // Shell link saved with LinkInfo
  {$EXTERNALSYM SLDF_HAS_NAME}
  SLDF_HAS_NAME           = $00000004;
  {$EXTERNALSYM SLDF_HAS_RELPATH}
  SLDF_HAS_RELPATH        = $00000008;
  {$EXTERNALSYM SLDF_HAS_WORKINGDIR}
  SLDF_HAS_WORKINGDIR     = $00000010;
  {$EXTERNALSYM SLDF_HAS_ARGS}
  SLDF_HAS_ARGS           = $00000020;
  {$EXTERNALSYM SLDF_HAS_ICONLOCATION}
  SLDF_HAS_ICONLOCATION   = $00000040;
  {$EXTERNALSYM SLDF_UNICODE}
  SLDF_UNICODE            = $00000080;   // the strings are unicode
  {$EXTERNALSYM SLDF_FORCE_NO_LINKINFO}
  SLDF_FORCE_NO_LINKINFO  = $00000100;   // don't create a LINKINFO (make a dumb link)
  {$EXTERNALSYM SLDF_HAS_EXP_SZ}
  SLDF_HAS_EXP_SZ         = $00000200;   // the link contains expandable env strings
  {$EXTERNALSYM SLDF_RUN_IN_SEPARATE}
  SLDF_RUN_IN_SEPARATE    = $00000400;   // Run the 16-bit target exe in a separate VDM/WOW
  {$EXTERNALSYM SLDF_HAS_LOGO3ID}
  SLDF_HAS_LOGO3ID        = $00000800;   // this link is a special Logo3/MSICD link
  {$EXTERNALSYM SLDF_HAS_DARWINID}
  SLDF_HAS_DARWINID       = $00001000;   // this link is a special Darwin link
  {$EXTERNALSYM SLDF_RUNAS_USER}
  SLDF_RUNAS_USER         = $00002000;   // Run this link as a different user
  {$EXTERNALSYM SLDF_HAS_EXP_ICON_SZ}
  SLDF_HAS_EXP_ICON_SZ    = $00004000;   // contains expandable env string for icon path
  {$EXTERNALSYM SLDF_NO_PIDL_ALIAS}
  SLDF_NO_PIDL_ALIAS      = $00008000;   // don't ever resolve to a logical location
  {$EXTERNALSYM SLDF_FORCE_UNCNAME}
  SLDF_FORCE_UNCNAME      = $00010000;   // make GetPath() prefer the UNC name to the local name
  {$EXTERNALSYM SLDF_RUN_WITH_SHIMLAYER}
  SLDF_RUN_WITH_SHIMLAYER = $00020000;   // Launch the target of this link w/ shim layer active
  {$EXTERNALSYM SLDF_RESERVED}
  SLDF_RESERVED           = $80000000;   // Reserved-- so we can use the low Word as an index value in the future

type
  PDataBlcokHeader = ^TDataBlockHeader;
  {$EXTERNALSYM tagDATABLOCKHEADER}
  tagDATABLOCKHEADER = record
    cbSize: DWORD;             // Size of this extra data block
    dwSignature: DWORD;        // signature of this extra data block
  end;
  {$EXTERNALSYM DATABLOCK_HEADER}
  DATABLOCK_HEADER = tagDATABLOCKHEADER;
  TDataBlockHeader = tagDATABLOCKHEADER;
  {$EXTERNALSYM LPDBLIST}
  LPDBLIST = tagDATABLOCKHEADER;

  PNTConsoleProps = ^TNTConsoleProps;
  {$EXTERNALSYM NT_CONSOLE_PROPS}
  NT_CONSOLE_PROPS = record
    dbh: TDataBlockHeader;
    wFillAttribute: Word;         // fill attribute for console
    wPopupFillAttribute: Word;    // fill attribute for console popups
    dwScreenBufferSize: TCoord;   // screen buffer size for console
    dwWindowSize: TCoord;         // window size for console
    dwWindowOrigin: TCoord;       // window origin for console
    nFont: DWORD;
    nInputBufferSize: DWORD;
    dwFontSize: TCoord;
    uFontFamily: UINT;
    uFontWeight: UINT;
    FaceName: array[0..LF_FACESIZE - 1] of WideChar;
    uCursorSize: UINT;
    bFullScreen: BOOL;
    bQuickEdit: BOOL;
    bInsertMode: BOOL;
    bAutoPosition: BOOL;
    uHistoryBufferSize: UINT;
    uNumberOfHistoryBuffers: UINT;
    bHistoryNoDup: BOOL;
    ColorTable: array[0..15] of TColorRef;
  end;
  TNTConsoleProps = NT_CONSOLE_PROPS;

const
  {$EXTERNALSYM NT_CONSOLE_PROPS_SIG}
  NT_CONSOLE_PROPS_SIG = $A0000002;

type
// This is a FE Console property
  PNTFEConsoleProps = ^TNTFEConsoleProps;
  {$EXTERNALSYM NT_FE_CONSOLE_PROPS}
  NT_FE_CONSOLE_PROPS = record
    dbh: TDataBlockHeader;
    uCodePage: UINT;
  end;
  TNTFEConsoleProps = NT_FE_CONSOLE_PROPS;

const
  {$EXTERNALSYM NT_FE_CONSOLE_PROPS_SIG}
  NT_FE_CONSOLE_PROPS_SIG = $A0000004;

type
  PExpDarwinLink = ^TExpDarwinLink;
  {$EXTERNALSYM EXP_DARWIN_LINK}
  EXP_DARWIN_LINK = record
    dbh: TDataBlockHeader;
    szDarwinID: array[0..MAX_PATH - 1] of AnsiChar;  // ANSI darwin ID associated with link
    szwDarwinID: array[0..MAX_PATH - 1] of WideChar; // UNICODE darwin ID associated with link
  end;
  TExpDarwinLink = EXP_DARWIN_LINK;

const
  {$EXTERNALSYM EXP_DARWIN_ID_SIG}
  EXP_DARWIN_ID_SIG = $A0000006;
  {$EXTERNALSYM EXP_LOGO3_ID_SIG}
  EXP_LOGO3_ID_SIG  = $A0000007;

  {$EXTERNALSYM EXP_SPECIAL_FOLDER_SIG}
  EXP_SPECIAL_FOLDER_SIG = $A0000005;   // LPEXP_SPECIAL_FOLDER

type
  PExpSpecialFolder = ^TExpSpecialFolder;
  {$EXTERNALSYM EXP_SPECIAL_FOLDER}
  EXP_SPECIAL_FOLDER = record
    cbSize: DWORD;             // Size of this extra data block
    dwSignature: DWORD;        // signature of this extra data block
    idSpecialFolder: DWORD;    // special folder id this link points into
    cbOffset: DWORD;           // ofset into pidl from SLDF_HAS_ID_LIST for child
  end;
  TExpSpecialFolder = EXP_SPECIAL_FOLDER;

  PExpSzLink = ^TExpSzLink;
  {$EXTERNALSYM EXP_SZ_LINK}
  EXP_SZ_LINK = record
    cbSize: DWORD;             // Size of this extra data block
    dwSignature: DWORD;        // signature of this extra data block
    szTarget: array[0..MAX_PATH - 1] of AnsiChar;   // ANSI target name w/EXP_SZ in it
    swzTarget: array[0..MAX_PATH - 1] of WideChar;  // UNICODE target name w/EXP_SZ in it
  end;
  TExpSzLink = EXP_SZ_LINK;

const
  {$EXTERNALSYM EXP_SZ_LINK_SIG}
  EXP_SZ_LINK_SIG = $A0000001;   // LPEXP_SZ_LINK (target)
  {$EXTERNALSYM EXP_SZ_ICON_SIG}
  EXP_SZ_ICON_SIG = $A0000007;   // LPEXP_SZ_LINK (icon)

type
  {$EXTERNALSYM IShellLinkDataList}
  IShellLinkDataList = interface(IUnknown)
  ['{45E2B4AE-B1C3-11D0-B92F-00A0C90312E1}']
    function AddDataBlock(pDataBlock: Pointer): HResult; stdcall;
    function CopyDataBlock(dwSig: DWORD; out ppDataBlock): HResult; stdcall;
    function RemoveDataBlock(dwSig: DWORD): HResult; stdcall;
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function SetFlags(dwFlags: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IResolveShellLink}
  IResolveShellLink = interface(IUnknown)
  ['{5CD52983-9449-11D2-963A-00C04F79ADF0}']
    function ResolveShellLink(punk: IUnknown; hwnd: HWND;
      fFlags: DWORD): HResult; stdcall;
  end;

//===========================================================================
//
// IShellExecuteHook Interface
//
//===========================================================================

  {$EXTERNALSYM IShellExecuteHookA}
  IShellExecuteHookA = interface(IUnknown) // shexhk
  ['{000214F5-0000-0000-C000-000000000046}']
    function Execute(var pei: TSHellExecuteInfoA): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellExecuteHookW}
  IShellExecuteHookW = interface(IUnknown) // shexhk
  ['{000214FB-0000-0000-C000-000000000046}']
    function Execute(var pei: TShellExecuteInfoW): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellExecuteHook}
  IShellExecuteHook = IShellExecuteHookA;

//===========================================================================
//
// IURLSearchHook Interface
//
//===========================================================================

  {$EXTERNALSYM IURLSearchHook}
  IURLSearchHook = interface(IUnknown)
  ['{AC60F6A0-0FD9-11D0-99CB-00C04FD64497}']
    function Translate(lpwszSearchURL: PWideChar;
      cchBufferSize: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM ISearchContext}
  ISearchContext = interface(IUnknown)
  ['{09F656A2-41AF-480C-88F7-16CC0D164615}']
    function GetSearchUrl(out pbstrSearchUrl: TBStr): HResult; stdcall;
    function GetSearchText(out pbstrSearchText: TBStr): HResult; stdcall;
    function GetSearchStyle(out pdwSearchStyle: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IURLSearchHook2}
  IURLSearchHook2 = interface(IURLSearchHook)
  ['{5EE44DA4-6D32-46E3-86BC-07540DEDD0E0}']
    function TranslateWithSearchContext(lpwszSearchURL: PWideChar;
      cchBufferSize: DWORD; pSearchContext: ISearchContext): HResult; stdcall;
  end;

//===========================================================================
//
// INewShortcutHook Interface
//
//===========================================================================

  {$EXTERNALSYM INewShortcutHookA}
  INewShortcutHookA = interface(IUnknown) // nshhk
  ['{000214E1-0000-0000-C000-000000000046}']
    function SetReferent(pcszReferent: PAnsiChar; hwnd: HWND): HResult; stdcall;
    function GetReferent(pszReferent: PAnsiChar; cchReferent: Integer): HResult; stdcall;
    function SetFolder(pcszFolder: PAnsiChar): HResult; stdcall;
    function GetFolder(pszFolder: PAnsiChar; cchFolder: Integer): HResult; stdcall;
    function GetName(pszName: PAnsiChar; cchName: Integer): HResult; stdcall;
    function GetExtension(pszExtension: PAnsiChar; cchExtension: Integer): HResult; stdcall;
  end;

  {$EXTERNALSYM INewShortcutHookW}
  INewShortcutHookW = interface(IUnknown) // nshhk
  ['{000214F7-0000-0000-C000-000000000046}']
    function SetReferent(pcszReferent: PWideChar; hwnd: HWND): HResult; stdcall;
    function GetReferent(pszReferent: PWideChar; cchReferent: Integer): HResult; stdcall;
    function SetFolder(pcszFolder: PWideChar): HResult; stdcall;
    function GetFolder(pszFolder: PWideChar; cchFolder: Integer): HResult; stdcall;
    function GetName(pszName: PWideChar; cchName: Integer): HResult; stdcall;
    function GetExtension(pszExtension: PWideChar; cchExtension: Integer): HResult; stdcall;
  end;

  {$EXTERNALSYM INewShortcutHook}
  INewShortcutHook = INewShortcutHookA;

//===========================================================================
//
// ICopyHook Interface
//
//  The copy hook is called whenever file system directories are
//  copy/moved/deleted/renamed via the shell.  It is also called by the shell
//  on changes of status of printers.
//
//  Clients register their id under STRREG_SHEX_COPYHOOK for file system hooks
//  and STRREG_SHEx_PRNCOPYHOOK for printer hooks.
//  the CopyCallback is called prior to the action, so the hook has the chance
//  to allow, deny or cancel the operation by returning the falues:
//     IDYES  -  means allow the operation
//     IDNO   -  means disallow the operation on this file, but continue with
//              any other operations (eg. batch copy)
//     IDCANCEL - means disallow the current operation and cancel any pending
//              operations
//
//   arguments to the CopyCallback
//      hwnd - window to use for any UI
//      wFunc - what operation is being done
//      wFlags - and flags (FOF_*) set in the initial call to the file operation
//      pszSrcFile - name of the source file
//      dwSrcAttribs - file attributes of the source file
//      pszDestFile - name of the destiation file (for move and renames)
//      dwDestAttribs - file attributes of the destination file
//
//
//===========================================================================

// These need to be kept in sync with the ones in ShellAPI.pas

// file operations
{$IFNDEF JWA_INCLUDEMODE}
const
  {$EXTERNALSYM FO_MOVE}
  FO_MOVE           = $0001;
  {$EXTERNALSYM FO_COPY}
  FO_COPY           = $0002;
  {$EXTERNALSYM FO_DELETE}
  FO_DELETE         = $0003;
  {$EXTERNALSYM FO_RENAME}
  FO_RENAME         = $0004;

  {$EXTERNALSYM FOF_MULTIDESTFILES}
  FOF_MULTIDESTFILES         = $0001;
  {$EXTERNALSYM FOF_CONFIRMMOUSE}
  FOF_CONFIRMMOUSE           = $0002;
  {$EXTERNALSYM FOF_SILENT}
  FOF_SILENT                 = $0004;  // don't create progress/report
  {$EXTERNALSYM FOF_RENAMEONCOLLISION}
  FOF_RENAMEONCOLLISION      = $0008;
  {$EXTERNALSYM FOF_NOCONFIRMATION}
  FOF_NOCONFIRMATION         = $0010;  // Don't prompt the user.
  {$EXTERNALSYM FOF_WANTMAPPINGHANDLE}
  FOF_WANTMAPPINGHANDLE      = $0020;  // Fill in SHFILEOPSTRUCT.hNameMappings
                                       // Must be freed using SHFreeNameMappings
  {$EXTERNALSYM FOF_ALLOWUNDO}
  FOF_ALLOWUNDO              = $0040;
  {$EXTERNALSYM FOF_FILESONLY}
  FOF_FILESONLY              = $0080;  // on *.*, do only files
  {$EXTERNALSYM FOF_SIMPLEPROGRESS}
  FOF_SIMPLEPROGRESS         = $0100;  // means don't show names of files
  {$EXTERNALSYM FOF_NOCONFIRMMKDIR}
  FOF_NOCONFIRMMKDIR         = $0200;  // don't confirm making any needed dirs
  {$EXTERNALSYM FOF_NOERRORUI}
  FOF_NOERRORUI              = $0400;  // don't put up error UI
  {$EXTERNALSYM FOF_NOCOPYSECURITYATTRIBS}
  FOF_NOCOPYSECURITYATTRIBS  = $0800;  // dont copy NT file Security Attributes
  {$EXTERNALSYM FOF_NORECURSION}
  FOF_NORECURSION            = $1000;  // don't recurse into directories.
  {$EXTERNALSYM FOF_NO_CONNECTED_ELEMENTS}
  FOF_NO_CONNECTED_ELEMENTS  = $2000;  // don't operate on connected file elements.
  {$EXTERNALSYM FOF_WANTNUKEWARNING}
  FOF_WANTNUKEWARNING        = $4000;  // during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)
  {$EXTERNALSYM FOF_NORECURSEREPARSE}
  FOF_NORECURSEREPARSE       = $8000;  // treat reparse points as objects, not containers
{$ENDIF JWA_INCLUDEMODE}

type
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM FILEOP_FLAGS}
  FILEOP_FLAGS = Word;
{$ENDIF JWA_INCLUDEMODE}  
  TFileOpFlags = Word;


{$IFNDEF JWA_INCLUDEMODE}
// printer operations
const
  {$EXTERNALSYM PO_DELETE}
  PO_DELETE       = $0013;  // printer is being deleted
  {$EXTERNALSYM PO_RENAME}
  PO_RENAME       = $0014;  // printer is being renamed
  {$EXTERNALSYM PO_PORTCHANGE}
  PO_PORTCHANGE   = $0020;  // port this printer connected to is being changed
                            // if this id is set, the strings received by
                            // the copyhook are a doubly-null terminated
                            // list of strings.  The first is the printer
                            // name and the second is the printer port.
  {$EXTERNALSYM PO_REN_PORT}
  PO_REN_PORT     = $0034;  // PO_RENAME and PO_PORTCHANGE at same time.
{$ENDIF JWA_INCLUDEMODE}

// no POF_ flags currently defined

type
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM PRINTEROP_FLAGS}
  PRINTEROP_FLAGS = UINT;
{$ENDIF JWA_INCLUDEMODE}  
  TPrinterOpFlags = UINT;

  {$EXTERNALSYM ICopyHookA}
  ICopyHookA = interface(IUnknown)        // sl
  ['{000214EF-0000-0000-C000-000000000046}']
    function CopyCallback(hwnd: HWND; wFunc, wFlags: UINT;
      pszSrcFile: PAnsiChar; dwSrcAttribs: DWORD;
      pszDestFile: PAnsiChar; dwDestAttribs: DWORD): UINT; stdcall;
  end;

  {$EXTERNALSYM ICopyHookW}
  ICopyHookW = interface(IUnknown)        // sl
  ['{000214FC-0000-0000-C000-000000000046}']
    // *** ICopyHook methods ***
    function CopyCallback(hwnd: HWND; wFunc, wFlags: UINT;
      pszSrcFile: PWideChar; dwSrcAttribs: DWORD;
      pszDestFile: PWideChar; dwDestAttribs: DWORD): UINT; stdcall;
  end;

  {$EXTERNALSYM ICopyHook}
  ICopyHook = ICopyHookA;

//===========================================================================
//
// IFileViewerSite Interface
//
//===========================================================================

  {$EXTERNALSYM IFileViewerSite}
  IFileViewerSite = interface(IUnknown)
  ['{000214F3-0000-0000-C000-000000000046}']
    function SetPinnedWindow(hwnd: HWND): HResult; stdcall;
    function GetPinnedWindow(out phwnd: HWND): HResult; stdcall;
  end;

//===========================================================================
//
// IFileViewer Interface
//
// Implemented in a FileViewer component object.  Used to tell a
// FileViewer to PrintTo or to view, the latter happening though
// ShowInitialize and Show.  The filename is always given to the
// viewer through IPersistFile.
//
//===========================================================================

type
  PFVShowInfo = ^TFVShowInfo;
  {$EXTERNALSYM FVSHOWINFO}
  FVSHOWINFO = record
    // Stuff passed into viewer (in)
    cbSize: DWORD;           // Size of structure for future expansion...
    hwndOwner: HWND;         // who is the owner window.
    iShow: Integer;          // The show command

    // Passed in and updated  (in/Out)
    dwFlags: DWORD;          // flags
    rect: TRect;             // Where to create the window may have defaults
    punkRel: IUnknown;       // Relese this interface when window is visible

    // Stuff that might be returned from viewer (out)
    strNewFile: array[0..MAX_PATH - 1] of TOleChar;   // New File to view.
  end;
  TFVShowInfo = FVSHOWINFO;

// Define File View Show Info Flags.
const
  {$EXTERNALSYM FVSIF_RECT}
  FVSIF_RECT      = $00000001;      // The rect variable has valid data.
  {$EXTERNALSYM FVSIF_PINNED}
  FVSIF_PINNED    = $00000002;      // We should Initialize pinned

  {$EXTERNALSYM FVSIF_NEWFAILED}
  FVSIF_NEWFAILED = $08000000;      // The new file passed back failed
                                    // to be viewed.

  {$EXTERNALSYM FVSIF_NEWFILE}
  FVSIF_NEWFILE   = $80000000;      // A new file to view has been returned
  {$EXTERNALSYM FVSIF_CANVIEWIT}
  FVSIF_CANVIEWIT = $40000000;      // The viewer can view it.

type
  {$EXTERNALSYM IFileViewerA}
  IFileViewerA = interface(IUnknown)
  ['{000214F0-0000-0000-C000-000000000046}']
    function ShowInitialize(lpfsi: IFileViewerSite): HResult; stdcall;
    function Show(var pvsi: TFVShowInfo): HResult; stdcall;
    function PrintTo(pszDriver: PAnsiChar; fSuppressUI: BOOL): HResult; stdcall;
  end;

  {$EXTERNALSYM IFileViewerW}
  IFileViewerW = interface(IUnknown)
  ['{000214F8-0000-0000-C000-000000000046}']
    function ShowInitialize(lpfsi: IFileViewerSite): HResult; stdcall;
    function Show(var pvsi: TFVShowInfo): HResult; stdcall;
    function PrintTo(pszDriver: PWideChar; fSuppressUI: BOOL): HResult; stdcall;
  end;

  {$EXTERNALSYM IFileViewer}
  IFileViewer = IFileViewerA;

//==========================================================================
//
// IShellBrowser/IShellView/IShellFolder interface
//
//  These three interfaces are used when the shell communicates with
// name space extensions. The shell (explorer) provides IShellBrowser
// interface, and extensions implements IShellFolder and IShellView
// interfaces.
//
//==========================================================================


//--------------------------------------------------------------------------
//
// Command/menuitem IDs
//
//  The explorer dispatches WM_COMMAND messages based on the range of
// command/menuitem IDs. All the IDs of menuitems that the view (right
// pane) inserts must be in FCIDM_SHVIEWFIRST/LAST (otherwise, the explorer
// won't dispatch them). The view should not deal with any menuitems
// in FCIDM_BROWSERFIRST/LAST (otherwise, it won't work with the future
// version of the shell).
//
//  FCIDM_SHVIEWFIRST/LAST      for the right pane (IShellView)
//  FCIDM_BROWSERFIRST/LAST     for the explorer frame (IShellBrowser)
//  FCIDM_GLOBAL/LAST           for the explorer's submenu IDs
//
//--------------------------------------------------------------------------

const
  {$EXTERNALSYM FCIDM_SHVIEWFIRST}
  FCIDM_SHVIEWFIRST           = $0000;
  {$EXTERNALSYM FCIDM_SHVIEWLAST}
  FCIDM_SHVIEWLAST            = $7FFF;
  {$EXTERNALSYM FCIDM_BROWSERFIRST}
  FCIDM_BROWSERFIRST          = $A000;
  {$EXTERNALSYM FCIDM_BROWSERLAST}
  FCIDM_BROWSERLAST           = $BF00;
  {$EXTERNALSYM FCIDM_GLOBALFIRST}
  FCIDM_GLOBALFIRST           = $8000;
  {$EXTERNALSYM FCIDM_GLOBALLAST}
  FCIDM_GLOBALLAST            = $9FFF;

//
// Global submenu IDs and separator IDs
//
  {$EXTERNALSYM FCIDM_MENU_FILE}
  FCIDM_MENU_FILE             = FCIDM_GLOBALFIRST + $0000;
  {$EXTERNALSYM FCIDM_MENU_EDIT}
  FCIDM_MENU_EDIT             = FCIDM_GLOBALFIRST + $0040;
  {$EXTERNALSYM FCIDM_MENU_VIEW}
  FCIDM_MENU_VIEW             = FCIDM_GLOBALFIRST + $0080;
  {$EXTERNALSYM FCIDM_MENU_VIEW_SEP_OPTIONS}
  FCIDM_MENU_VIEW_SEP_OPTIONS = FCIDM_GLOBALFIRST + $0081;
  {$EXTERNALSYM FCIDM_MENU_TOOLS}
  FCIDM_MENU_TOOLS            = FCIDM_GLOBALFIRST + $00c0; // for Win9x compat
  {$EXTERNALSYM FCIDM_MENU_TOOLS_SEP_GOTO}
  FCIDM_MENU_TOOLS_SEP_GOTO   = FCIDM_GLOBALFIRST + $00c1; // for Win9x compat
  {$EXTERNALSYM FCIDM_MENU_HELP}
  FCIDM_MENU_HELP             = FCIDM_GLOBALFIRST + $0100;
  {$EXTERNALSYM FCIDM_MENU_FIND}
  FCIDM_MENU_FIND             = FCIDM_GLOBALFIRST + $0140;
  {$EXTERNALSYM FCIDM_MENU_EXPLORE}
  FCIDM_MENU_EXPLORE          = FCIDM_GLOBALFIRST + $0150;
  {$EXTERNALSYM FCIDM_MENU_FAVORITES}
  FCIDM_MENU_FAVORITES        = FCIDM_GLOBALFIRST + $0170;

//--------------------------------------------------------------------------
// control IDs known to the view
//--------------------------------------------------------------------------

  {$EXTERNALSYM FCIDM_TOOLBAR}
  FCIDM_TOOLBAR               = FCIDM_BROWSERFIRST + 0;
  {$EXTERNALSYM FCIDM_STATUS}
  FCIDM_STATUS                = FCIDM_BROWSERFIRST + 1;

//--------------------------------------------------------------------------
//
// The resource id of the offline cursor
// This cursor is avaialble in shdocvw.dll
const
  {$EXTERNALSYM IDC_OFFLINE_HAND}
  IDC_OFFLINE_HAND = 103;
//
//--------------------------------------------------------------------------

// SBCMDID_GETPANE - not necessarily in order
const
  {$EXTERNALSYM PANE_NONE}
  PANE_NONE       = -1;
  {$EXTERNALSYM PANE_ZONE}
  PANE_ZONE       =  1;
  {$EXTERNALSYM PANE_OFFLINE}
  PANE_OFFLINE    =  2;
  {$EXTERNALSYM PANE_PRINTER}
  PANE_PRINTER    =  3;
  {$EXTERNALSYM PANE_SSL}
  PANE_SSL        =  4;
  {$EXTERNALSYM PANE_NAVIGATION}
  PANE_NAVIGATION =  5;
  {$EXTERNALSYM PANE_PROGRESS}
  PANE_PROGRESS   =  6;
  {$EXTERNALSYM PANE_PRIVACY}
  PANE_PRIVACY    =  7;


//-------------------------------------------------------------------------
// ICommDlgBrowser interface
//
//  ICommDlgBrowser interface is the interface that is provided by the new
// common dialog window to hook and modify the behavior of IShellView.  When
// a default view is created, it queries its parent IShellBrowser for the
// ICommDlgBrowser interface.  If supported, it calls out to that interface
// in several cases that need to behave differently in a dialog.
//
// Member functions:
//
//  ICommDlgBrowser::OnDefaultCommand()
//    Called when the user double-clicks in the view or presses Enter.  The
//   browser should return S_OK if it processed the action itself, S_FALSE
//   to let the view perform the default action.
//
//  ICommDlgBrowser::OnStateChange(ULONG uChange)
//    Called when some states in the view change.  'uChange' is one of the
//   CDBOSC_* values.  This call is made after the state (selection, focus,
//   etc) has changed.  There is no return value.
//
//  ICommDlgBrowser::IncludeObject(PItemIDList pidl)
//    Called when the view is enumerating objects.  'pidl' is a relative
//   IDLIST.  The browser should return S_OK to include the object in the
//   view, S_FALSE to hide it
//
//-------------------------------------------------------------------------

  {$EXTERNALSYM CDBOSC_SETFOCUS}
  CDBOSC_SETFOCUS     = $00000000;
  {$EXTERNALSYM CDBOSC_KILLFOCUS}
  CDBOSC_KILLFOCUS    = $00000001;
  {$EXTERNALSYM CDBOSC_SELCHANGE}
  CDBOSC_SELCHANGE    = $00000002;
  {$EXTERNALSYM CDBOSC_RENAME}
  CDBOSC_RENAME       = $00000003;
  {$EXTERNALSYM CDBOSC_STATECHANGE}
  CDBOSC_STATECHANGE  = $00000004;

type
  {$EXTERNALSYM ICommDlgBrowser}
  ICommDlgBrowser = interface(IUnknown)
  ['{000214F1-0000-0000-C000-000000000046}']
    function OnDefaultCommand(ppshv: IShellView): HResult; stdcall;
    function OnStateChange(ppshv: IShellView;
      uChange: ULONG): HResult; stdcall;
    function IncludeObject(ppshv: IShellView;
      pidl: PItemIDList): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
// ICommDlgBrowser2 interface
//
// Member functions:
//
//  ICommDlgBrowser2::Notify(IShellView *pshv, DWORD dwNotfyType)
//   Called when the view is wants to notify common dialog when an event
//  occurrs.
//
//  CDB2N_CONTEXTMENU_START indicates the context menu has started.
//  CDB2N_CONTEXTMENU_DONE  indicates the context menu has completed.
//
//  ICommDlgBrowser2::GetDefaultMenuText(IShellView *pshv,
//                                      WideChar *pszText, INT cchMax)
//   Called when the view wants to get the default context menu text.
//  pszText points to buffer and cchMax specifies the size of the
//  buffer in characters.  The browser on return has filled the buffer
//  with the default context menu text.  The Shell will call this method
//  with at least a buffer size of MAX_PATH.  The browser should return
//  S_OK if it returned a new default menu text, S_FALSE to let the view
//  to use the normal default menu text.
//
//  ICommDlgBrowser2::GetViewFlags(DWORD *pdwFlags)
//     Called when the view wants to determine  if special customization needs to
//    be done for the common dialog browser. For example View calls this function to
//    determin if all files(hidden and system)needs to be shown. If the GetViewFlags returns a DWORD with
//    CDB2GVF_SHOWALLFILES  flag set then it will show all the files.
//-------------------------------------------------------------------------

const
  {$EXTERNALSYM CDB2N_CONTEXTMENU_DONE}
  CDB2N_CONTEXTMENU_DONE  = $00000001;
  {$EXTERNALSYM CDB2N_CONTEXTMENU_START}
  CDB2N_CONTEXTMENU_START = $00000002;

//GetViewFlags
  {$EXTERNALSYM CDB2GVF_SHOWALLFILES}
  CDB2GVF_SHOWALLFILES        = $00000001;

type
  {$EXTERNALSYM ICommDlgBrowser2}
  ICommDlgBrowser2 = interface(IUnknown)
  ['{10339516-2894-11D2-9039-00C04F8EEB3E}']
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView;
      pszText: PWideChar; cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
  end;

//
// function assumes the size of the buffer (MAX_PATH). The pidl
// should point to a file system object.

{$EXTERNALSYM SHGetPathFromIDListA}
function SHGetPathFromIDListA(pidl: PItemIDList;
  pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SHGetPathFromIDListW}
function SHGetPathFromIDListW(pidl: PItemIDList;
  pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SHGetPathFromIDList}
function SHGetPathFromIDList(pidl: PItemIDList;
  pszPath: PTSTR): BOOL; stdcall;

{$EXTERNALSYM SHCreateDirectory}
function SHCreateDirectory(hwnd: HWND; pszPath: PWideChar): Integer; stdcall;

{$EXTERNALSYM SHCreateDirectoryExA}
function SHCreateDirectoryExA(hwnd: HWND; pszPath: PAnsiChar;
  var psa: TSecurityAttributes): Integer; stdcall;
{$EXTERNALSYM SHCreateDirectoryExW}
function SHCreateDirectoryExW(hwnd: HWND; pszPath: PWideChar;
  var psa: TSecurityAttributes): Integer; stdcall;
{$EXTERNALSYM SHCreateDirectoryEx}
function SHCreateDirectoryEx(hwnd: HWND; pszPath: PTSTR;
  var psa: TSecurityAttributes): Integer; stdcall;

{$EXTERNALSYM SHOpenFolderAndSelectItems}
function SHOpenFolderAndSelectItems(pidlFolder: PItemIDList; cidl: UINT;
  apidl: PPItemIDListArray; dwFlags: DWORD): HResult; stdcall;

{$EXTERNALSYM SHCreateShellItem}
function SHCreateShellItem(pidlParent: PItemIDList;
  psfParent: IShellFolder; pidl: PItemIDList;
  out ppsi: IShellItem): HResult stdcall;


//-------------------------------------------------------------------------
//
// SHGetSpecialFolderLocation
//
//  Caller should use SHGetMalloc to obtain an allocator that can free the pidl
//
//
//-------------------------------------------------------------------------
//
// registry entries for special paths are kept in :
const
  {$EXTERNALSYM REGSTR_PATH_SPECIAL_FOLDERS}
  REGSTR_PATH_SPECIAL_FOLDERS = '\Shell Folders';

  {$EXTERNALSYM CSIDL_DESKTOP}
  CSIDL_DESKTOP                   = $0000;        // <desktop>
  {$EXTERNALSYM CSIDL_INTERNET}
  CSIDL_INTERNET                  = $0001;        // Internet Explorer (icon on desktop)
  {$EXTERNALSYM CSIDL_PROGRAMS}
  CSIDL_PROGRAMS                  = $0002;        // Start Menu\Programs
  {$EXTERNALSYM CSIDL_CONTROLS}
  CSIDL_CONTROLS                  = $0003;        // My Computer\Control Panel
  {$EXTERNALSYM CSIDL_PRINTERS}
  CSIDL_PRINTERS                  = $0004;        // My Computer\Printers
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL                  = $0005;        // My Documents
{$ENDIF JWA_INCLUDEMODE}  
  {$EXTERNALSYM CSIDL_FAVORITES}
  CSIDL_FAVORITES                 = $0006;        // <user name>\Favorites
  {$EXTERNALSYM CSIDL_STARTUP}
  CSIDL_STARTUP                   = $0007;        // Start Menu\Programs\Startup
  {$EXTERNALSYM CSIDL_RECENT}
  CSIDL_RECENT                    = $0008;        // <user name>\Recent
  {$EXTERNALSYM CSIDL_SENDTO}
  CSIDL_SENDTO                    = $0009;        // <user name>\SendTo
  {$EXTERNALSYM CSIDL_BITBUCKET}
  CSIDL_BITBUCKET                 = $000A;        // <desktop>\Recycle Bin
  {$EXTERNALSYM CSIDL_STARTMENU}
  CSIDL_STARTMENU                 = $000B;        // <user name>\Start Menu
  {$EXTERNALSYM CSIDL_MYDOCUMENTS}
  CSIDL_MYDOCUMENTS               = $000C;        // logical "My Documents" desktop icon
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_MYMUSIC}
  CSIDL_MYMUSIC                   = $000D;        // "My Music" folder
{$ENDIF JWA_INCLUDEMODE}  
  {$EXTERNALSYM CSIDL_MYVIDEO}
  CSIDL_MYVIDEO                   = $000E;        // "My Videos" folder
  CSIDL_DESKTOPDIRECTORY          = $0010;        // <user name>\Desktop
  {$EXTERNALSYM CSIDL_DRIVES}
  CSIDL_DRIVES                    = $0011;        // My Computer
  {$EXTERNALSYM CSIDL_NETWORK}
  CSIDL_NETWORK                   = $0012;        // Network Neighborhood (My Network Places)
  {$EXTERNALSYM CSIDL_NETHOOD}
  CSIDL_NETHOOD                   = $0013;        // <user name>\nethood
  {$EXTERNALSYM CSIDL_FONTS}
  CSIDL_FONTS                     = $0014;        // windows\fonts
  {$EXTERNALSYM CSIDL_TEMPLATES}
  CSIDL_TEMPLATES                 = $0015;
  {$EXTERNALSYM CSIDL_COMMON_STARTMENU}
  CSIDL_COMMON_STARTMENU          = $0016;        // All Users\Start Menu
  {$EXTERNALSYM CSIDL_COMMON_PROGRAMS}
  CSIDL_COMMON_PROGRAMS           = $0017;        // All Users\Start Menu\Programs
  {$EXTERNALSYM CSIDL_COMMON_STARTUP}
  CSIDL_COMMON_STARTUP            = $0018;        // All Users\Startup
  {$EXTERNALSYM CSIDL_COMMON_DESKTOPDIRECTORY}
  CSIDL_COMMON_DESKTOPDIRECTORY   = $0019;        // All Users\Desktop
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA                   = $001A;        // <user name>\Application Data
{$ENDIF JWA_INCLUDEMODE}  
  {$EXTERNALSYM CSIDL_PRINTHOOD}
  CSIDL_PRINTHOOD                 = $001B;        // <user name>\PrintHood
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA             = $001C;        // <user name>\Local Settings\Applicaiton Data (non roaming)
{$ENDIF JWA_INCLUDEMODE}  

  {$EXTERNALSYM CSIDL_ALTSTARTUP}
  CSIDL_ALTSTARTUP                = $001D;        // non localized startup
  {$EXTERNALSYM CSIDL_COMMON_ALTSTARTUP}
  CSIDL_COMMON_ALTSTARTUP         = $001E;        // non localized common startup
  {$EXTERNALSYM CSIDL_COMMON_FAVORITES}
  CSIDL_COMMON_FAVORITES          = $001F;

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE            = $0020;
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES                   = $0021;
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY                   = $0022;
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA            = $0023;        // All Users\Application Data
  {$EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_WINDOWS                   = $0024;        // GetWindowsDirectory()
  {$EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_SYSTEM                    = $0025;        // GetSystemDirectory()
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_PROGRAM_FILES             = $0026;        // C:\Program Files
  {$EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_MYPICTURES                = $0027;        // C:\Program Files\My Pictures
{$ENDIF JWA_INCLUDEMODE}    

  {$EXTERNALSYM CSIDL_PROFILE}
  CSIDL_PROFILE                   = $0028;        // USERPROFILE
  {$EXTERNALSYM CSIDL_SYSTEMX86}
  CSIDL_SYSTEMX86                 = $0029;        // x86 system directory on RISC
  {$EXTERNALSYM CSIDL_PROGRAM_FILESX86}
  CSIDL_PROGRAM_FILESX86          = $002A;        // x86 C:\Program Files on RISC

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_PROGRAM_FILES_COMMON      = $002B;        // C:\Program Files\Common
{$ENDIF JWA_INCLUDEMODE}  

  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMONX86}
  CSIDL_PROGRAM_FILES_COMMONX86   = $002C;        // x86 Program Files\Common on RISC
  {$EXTERNALSYM CSIDL_COMMON_TEMPLATES}
  CSIDL_COMMON_TEMPLATES          = $002D;        // All Users\Templates
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_DOCUMENTS          = $002E;        // All Users\Documents

  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_COMMON_ADMINTOOLS         = $002F;        // All Users\Start Menu\Programs\Administrative Tools
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  CSIDL_ADMINTOOLS                = $0030;        // <user name>\Start Menu\Programs\Administrative Tools
{$ENDIF JWA_INCLUDEMODE}
  
  {$EXTERNALSYM CSIDL_CONNECTIONS}
  CSIDL_CONNECTIONS               = $0031;        // Network and Dial-up Connections
  {$EXTERNALSYM CSIDL_COMMON_MUSIC}
  CSIDL_COMMON_MUSIC              = $0035;        // All Users\My Music
  {$EXTERNALSYM CSIDL_COMMON_PICTURES}
  CSIDL_COMMON_PICTURES           = $0036;        // All Users\My Pictures
  {$EXTERNALSYM CSIDL_COMMON_VIDEO}
  CSIDL_COMMON_VIDEO              = $0037;        // All Users\My Video
{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_RESOURCES}
  CSIDL_RESOURCES                 = $0038;        // Resource Direcotry

  {$EXTERNALSYM CSIDL_RESOURCES_LOCALIZED}
  CSIDL_RESOURCES_LOCALIZED       = $0039;        // Localized Resource Direcotry
{$ENDIF JWA_INCLUDEMODE}    

  {$EXTERNALSYM CSIDL_COMMON_OEM_LINKS}
  CSIDL_COMMON_OEM_LINKS          = $003A;        // Links to All Users OEM specific apps
  {$EXTERNALSYM CSIDL_CDBURN_AREA}
  CSIDL_CDBURN_AREA               = $003B;        // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
// unused                         = $003C;
  {$EXTERNALSYM CSIDL_COMPUTERSNEARME}
  CSIDL_COMPUTERSNEARME           = $003D;        // Computers Near Me (computered from Workgroup membership)

{$IFNDEF JWA_INCLUDEMODE}
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE               = $8000;        // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM CSIDL_FLAG_DONT_VERIFY}
  CSIDL_FLAG_DONT_VERIFY          = $4000;        // combine with CSIDL_ value to return an unverified folder path
  {$EXTERNALSYM CSIDL_FLAG_NO_ALIAS}
  CSIDL_FLAG_NO_ALIAS             = $1000;        // combine with CSIDL_ value to insure non-alias versions of the pidl
  {$EXTERNALSYM CSIDL_FLAG_PER_USER_INIT}
  CSIDL_FLAG_PER_USER_INIT        = $0800;        // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
  {$EXTERNALSYM CSIDL_FLAG_MASK}
  CSIDL_FLAG_MASK                 = $FF00;        // mask for all possible flag values

  {$EXTERNALSYM CSIDL_FLAG_PFTI_TRACKTARGET}
  CSIDL_FLAG_PFTI_TRACKTARGET     = CSIDL_FLAG_DONT_VERIFY;

{$EXTERNALSYM SHGetSpecialFolderLocation}
function SHGetSpecialFolderLocation(hwnd: HWND; csidl: Integer; out ppidl: PItemIDList): HResult; stdcall;

{$EXTERNALSYM SHFlushSFCache}
procedure SHFlushSFCache; stdcall;
{$EXTERNALSYM SHCloneSpecialIDList}
function SHCloneSpecialIDList(hwnd: HWND; csidl: Integer; fCreate: BOOL): PItemIDList; stdcall;

{$EXTERNALSYM SHGetSpecialFolderPathA}
function SHGetSpecialFolderPathA(hwnd: HWND; pszPath: PAnsiChar; csidl: Integer; fCreate: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHGetSpecialFolderPathW}
function SHGetSpecialFolderPathW(hwnd: HWND; pszPath: PWideChar; csidl: Integer; fCreate: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHGetSpecialFolderPath}
function SHGetSpecialFolderPath(hwnd: HWND; pszPath: PTSTR; csidl: Integer; fCreate: BOOL): BOOL; stdcall;

type
  {$EXTERNALSYM SHGFP_TYPE}
  SHGFP_TYPE = DWORD;
  TSHGFPType = DWORD;

const
  {$EXTERNALSYM SHGFP_TYPE_CURRENT}
  SHGFP_TYPE_CURRENT = 0;   // current value for user, verify it exists
  {$EXTERNALSYM SHGFP_TYPE_DEFAULT}
  SHGFP_TYPE_DEFAULT = 1;   // default value, may not exist

{$IFNDEF JWA_INCLUDEMODE}
{$EXTERNALSYM SHGetFolderPathA}
function SHGetFolderPathA(hwnd: HWND; csidl: Integer; hToken: THandle;
  dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPathW}
function SHGetFolderPathW(hwnd: HWND; csidl: Integer; hToken: THandle;
  dwFlags: DWORD; pszPath: PWideChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPath}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle;
  dwFlags: DWORD; pszPath: PTSTR): HResult; stdcall;
{$ENDIF JWA_INCLUDEMODE}

{$EXTERNALSYM SHGetFolderLocation}
function SHGetFolderLocation(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; out ppidl: ItemIDList): HResult; stdcall;

{$EXTERNALSYM SHGetFolderPathAndSubDirA}
function SHGetFolderPathAndSubDirA(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszSubDir, pszPath: PAnsiChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPathAndSubDirW}
function SHGetFolderPathAndSubDirW(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszSubDir, pszPath: PWideChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPathAndSubDir}
function SHGetFolderPathAndSubDir(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszSubDir, pszPath: PTSTR): HResult; stdcall;

const
  {$EXTERNALSYM FCS_READ}
  FCS_READ                   = $00000001;
  {$EXTERNALSYM FCS_FORCEWRITE}
  FCS_FORCEWRITE             = $00000002;
  {$EXTERNALSYM FCS_WRITE}
  FCS_WRITE                  = FCS_READ or FCS_FORCEWRITE;

  {$EXTERNALSYM FCS_FLAG_DRAGDROP}
  FCS_FLAG_DRAGDROP         =  2;

// Mask which values have been retreived or being set.
  {$EXTERNALSYM FCSM_VIEWID}
  FCSM_VIEWID                = $00000001;
  {$EXTERNALSYM FCSM_WEBVIEWTEMPLATE}
  FCSM_WEBVIEWTEMPLATE       = $00000002;
  {$EXTERNALSYM FCSM_INFOTIP}
  FCSM_INFOTIP               = $00000004;
  {$EXTERNALSYM FCSM_CLSID}
  FCSM_CLSID                 = $00000008;
  {$EXTERNALSYM FCSM_ICONFILE}
  FCSM_ICONFILE              = $00000010;
  {$EXTERNALSYM FCSM_LOGO}
  FCSM_LOGO                  = $00000020;
  {$EXTERNALSYM FCSM_FLAGS}
  FCSM_FLAGS                 = $00000040;

// Used by SHGetSetFolderCustomSettingsA
type
  PSHFolderCustomSettingsA = ^TSHFolderCustomSettingsA;
  {$EXTERNALSYM SHFOLDERCUSTOMSETTINGSA}
  SHFOLDERCUSTOMSETTINGSA = record
    dwSize: DWORD;
    dwMask: DWORD;                  // IN/OUT   Which Attributes to Get/Set
    pvid: PShellViewID;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // The folder's WebView template path
    pszWebViewTemplate: PAnsiChar;  // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchWebViewTemplate: DWORD;      // IN - Specifies the size of the buffer pointed to by pszWebViewTemplate
                                    // Ignored if dwReadWrite is FCS_READ
    pszWebViewTemplateVersion: PAnsiChar;  // currently IN only
    // Infotip for the folder
    pszInfoTip: PAnsiChar;          // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchInfoTip: DWORD;              // IN - Specifies the size of the buffer pointed to by pszInfoTip
                                    // Ignored if dwReadWrite is FCS_READ
    // CLSID that points to more info in the registry
    pclsid: PCLSID;                 // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // Other flags for the folder. Takes FCS_FLAG_* values
    dwFlags: DWORD;                 // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszIconFile: PAnsiChar;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchIconFile: DWORD;             // IN - Specifies the size of the buffer pointed to by pszIconFile
                                    // Ignored if dwReadWrite is FCS_READ

    iIconIndex: Integer;            // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszLogo: PAnsiChar;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchLogo: DWORD;                 // IN - Specifies the size of the buffer pointed to by pszIconFile
                                    // Ignored if dwReadWrite is FCS_READ
  end;
  TSHFolderCustomSettingsA = SHFOLDERCUSTOMSETTINGSA;

  PSHFolderCustomSettingsW = ^TSHFolderCustomSettingsW;
  {$EXTERNALSYM SHFOLDERCUSTOMSETTINGSW}
  SHFOLDERCUSTOMSETTINGSW = record
    dwSize: DWORD;
    dwMask: DWORD;                  // IN/OUT  Which Attributes to Get/Set
    pvid: PShellViewID;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // The folder's WebView template path
    pszWebViewTemplate: PWideChar;  // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchWebViewTemplate: DWORD;      // IN - Specifies the size of the buffer pointed to by pszWebViewTemplate
                                    // Ignored if dwReadWrite is FCS_READ
    pszWebViewTemplateVersion: PWideChar;  // currently IN only
    // Infotip for the folder
    pszInfoTip: PWideChar;          // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchInfoTip: DWORD;              // IN - Specifies the size of the buffer pointed to by pszInfoTip
                                    // Ignored if dwReadWrite is FCS_READ
    // CLSID that points to more info in the registry
    pclsid: PCLSID;                 // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    // Other flags for the folder. Takes FCS_FLAG_* values
    dwFlags: DWORD;                 // OUT - if dwReadWrite is FCS_READ, IN - otherwise


    pszIconFile: PWideChar;         // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchIconFile: DWORD;             // IN - Specifies the size of the buffer pointed to by pszIconFile
                                    // Ignored if dwReadWrite is FCS_READ

    iIconIndex: Integer;            // OUT - if dwReadWrite is FCS_READ, IN - otherwise

    pszLogo: PWideChar;             // OUT - if dwReadWrite is FCS_READ, IN - otherwise
    cchLogo: DWORD;                 // IN - Specifies the size of the buffer pointed to by pszIconFile
                                    // Ignored if dwReadWrite is FCS_READ
  end;
  TSHFolderCustomSettingsW = SHFOLDERCUSTOMSETTINGSW;

  {$EXTERNALSYM SHFOLDERCUSTOMSETTINGS}
  SHFOLDERCUSTOMSETTINGS = SHFOLDERCUSTOMSETTINGSA;
  TSHFolderCustomSettings = TSHFolderCustomSettingsA;
  PSHFolderCustomSettings = PSHFolderCustomSettingsA;

// Gets/Sets the Folder Custom Settings for pszPath based on dwReadWrite.
// dwReadWrite can be FCS_READ/FCS_WRITE/FCS_FORCEWRITE

{$EXTERNALSYM SHGetSetFolderCustomSettingsA}
function SHGetSetFolderCustomSettingsA(var pfcs: TSHFolderCustomSettingsA;
    pszPath: PAnsiChar; dwReadWrite: DWORD): HResult; stdcall;
{$EXTERNALSYM SHGetSetFolderCustomSettingsW}
function SHGetSetFolderCustomSettingsW(var pfcs: TSHFolderCustomSettingsW;
    pszPath: PWideChar; dwReadWrite: DWORD): HResult; stdcall;
{$EXTERNALSYM SHGetSetFolderCustomSettingsA}
function SHGetSetFolderCustomSettings(var pfcs: TSHFolderCustomSettings;
    pszPath: PTSTR; dwReadWrite: DWORD): HResult; stdcall;

//-------------------------------------------------------------------------
//
// SHBrowseForFolder API
//
//
//-------------------------------------------------------------------------

{$IFNDEF JWA_INCLUDEMODE}
type
  {$EXTERNALSYM BFFCALLBACK}
  BFFCALLBACK = function(hwnd: HWND; uMsg: UINT; lParam: LPARAM; lpData: LPARAM): Integer stdcall;
  TBFFCallback = BFFCALLBACK;
{$ENDIF JWA_INCLUDEMODE}  

//#include <pshpack8.h>

type
  PBrowseInfoA = ^TBrowseInfoA;
  {$EXTERNALSYM _browseinfoA}
  _browseinfoA = record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PAnsiChar;   // Return display name of item selected.
    lpszTitle: PAnsiChar;        // text to go in the banner over the tree.
    ulFlags: UINT;               // Flags that control the return stuff
    lpfn: TBFFCallback;
    lParam: LPARAM;              // extra info that's passed back in callbacks
    iImage: Integer;             // output var: where to return the Image index.
  end;
  {$EXTERNALSYM BROWSEINFOA}
  BROWSEINFOA = _browseinfoA;
  TBrowseInfoA = _browseinfoA;

  PBrowseInfoW = ^TBrowseInfoW;
  {$EXTERNALSYM _browseinfoW}
  _browseinfoW = record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PWideChar;   // Return display name of item selected.
    lpszTitle: PWideChar;        // text to go in the banner over the tree.
    ulFlags: UINT;               // Flags that control the return stuff
    lpfn: TBFFCallback;
    lParam: LPARAM;              // extra info that's passed back in callbacks
    iImage: Integer;             // output var: where to return the Image index.
  end;
  {$EXTERNALSYM BROWSEINFOA}
  BROWSEINFOW = _browseinfoW;
  TBrowseInfoW = _browseinfoW;

  {$EXTERNALSYM BROWSEINFO}
  BROWSEINFO = BROWSEINFOA;
  TBrowseInfo = TBrowseInfoA;
  PBrowseInfo = PBrowseInfoA;

// Browsing for directory.
const
  {$EXTERNALSYM BIF_RETURNONLYFSDIRS}
  BIF_RETURNONLYFSDIRS  = $0001;  // For finding a folder to start document searching
  {$EXTERNALSYM BIF_DONTGOBELOWDOMAIN}
  BIF_DONTGOBELOWDOMAIN = $0002;  // For starting the Find Computer
  {$EXTERNALSYM BIF_STATUSTEXT}
  BIF_STATUSTEXT        = $0004;  // Top of the dialog has 2 lines of text for BROWSEINFO.lpszTitle and one line if
                                  // this flag is set.  Passing the message BFFM_SETSTATUSTEXTA to the hwnd can set the
                                  // rest of the text.  This is not used with BIF_USENEWUI and BROWSEINFO.lpszTitle gets
                                  // all three lines of text.
  {$EXTERNALSYM BIF_RETURNFSANCESTORS}
  BIF_RETURNFSANCESTORS = $0008;
  {$EXTERNALSYM BIF_EDITBOX}
  BIF_EDITBOX           = $0010;  // Add an editbox to the dialog
  {$EXTERNALSYM BIF_VALIDATE}
  BIF_VALIDATE          = $0020;  // insist on valid result (or CANCEL)

  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_NEWDIALOGSTYLE    = $0040;  // Use the new dialog layout with the ability to resize
                                  // Caller needs to call OleInitialize() before using this API

  {$EXTERNALSYM BIF_USENEWUI}
  BIF_USENEWUI          = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;

  {$EXTERNALSYM BIF_BROWSEINCLUDEURLS}
  BIF_BROWSEINCLUDEURLS = $0080;  // Allow URLs to be displayed or entered. (Requires BIF_USENEWUI)
  {$EXTERNALSYM BIF_UAHINT}
  BIF_UAHINT            = $0100;  // Add a UA hint to the dialog, in place of the edit box. May not be combined with BIF_EDITBOX
  {$EXTERNALSYM BIF_NONEWFOLDERBUTTON}
  BIF_NONEWFOLDERBUTTON = $0200;  // Do not add the "New Folder" button to the dialog.  Only applicable with BIF_NEWDIALOGSTYLE.
  {$EXTERNALSYM BIF_NOTRANSLATETARGETS}
  BIF_NOTRANSLATETARGETS= $0400;  // don't traverse target as shortcut

  {$EXTERNALSYM BIF_BROWSEFORCOMPUTER}
  BIF_BROWSEFORCOMPUTER = $1000;  // Browsing for Computers.
  {$EXTERNALSYM BIF_BROWSEFORPRINTER}
  BIF_BROWSEFORPRINTER  = $2000;  // Browsing for Printers
  {$EXTERNALSYM BIF_BROWSEINCLUDEFILES}
  BIF_BROWSEINCLUDEFILES= $4000;  // Browsing for Everything
  {$EXTERNALSYM BIF_SHAREABLE}
  BIF_SHAREABLE         = $8000;  // sharable resources displayed (remote shares, requires BIF_USENEWUI)

// message from browser
  {$EXTERNALSYM BFFM_INITIALIZED}
  BFFM_INITIALIZED      = 1;
  {$EXTERNALSYM BFFM_SELCHANGED}
  BFFM_SELCHANGED       = 2;
  {$EXTERNALSYM BFFM_VALIDATEFAILEDA}
  BFFM_VALIDATEFAILEDA  = 3;      // lParam:szPath ret:1(cont),0(EndDialog)
  {$EXTERNALSYM BFFM_VALIDATEFAILEDW}
  BFFM_VALIDATEFAILEDW  = 4;      // lParam:wzPath ret:1(cont),0(EndDialog)
  {$EXTERNALSYM BFFM_IUNKNOWN}
  BFFM_IUNKNOWN         = 5;      // provides IUnknown to client. lParam: IUnknown*

// messages to browser
  {$EXTERNALSYM BFFM_SETSTATUSTEXTA}
  BFFM_SETSTATUSTEXTA   = WM_USER + 100;
  {$EXTERNALSYM BFFM_ENABLEOK}
  BFFM_ENABLEOK         = WM_USER + 101;
  {$EXTERNALSYM BFFM_SETSELECTIONA}
  BFFM_SETSELECTIONA    = WM_USER + 102;
  {$EXTERNALSYM BFFM_SETSELECTIONW}
  BFFM_SETSELECTIONW    = WM_USER + 103;
  {$EXTERNALSYM BFFM_SETSTATUSTEXTW}
  BFFM_SETSTATUSTEXTW   = WM_USER + 104;
  {$EXTERNALSYM BFFM_SETOKTEXT}
  BFFM_SETOKTEXT        = WM_USER + 105; // Unicode only
  {$EXTERNALSYM BFFM_SETEXPANDED}
  BFFM_SETEXPANDED      = WM_USER + 106; // Unicode only

{$EXTERNALSYM SHBrowseForFolderA}
function SHBrowseForFolderA(var lpbi: TBrowseInfoA): PItemIDList; stdcall;
{$EXTERNALSYM SHBrowseForFolderW}
function SHBrowseForFolderW(var lpbi: TBrowseInfoW): PItemIDList; stdcall;
{$EXTERNALSYM SHBrowseForFolder}
function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall;

const
  {$EXTERNALSYM BFFM_SETSTATUSTEXT}
  BFFM_SETSTATUSTEXT  = BFFM_SETSTATUSTEXTA;
  {$EXTERNALSYM BFFM_SETSELECTION}
  BFFM_SETSELECTION   = BFFM_SETSELECTIONA;

  {$EXTERNALSYM BFFM_VALIDATEFAILED}
  BFFM_VALIDATEFAILED = BFFM_VALIDATEFAILEDA;

//-------------------------------------------------------------------------
//
// SHLoadInProc
//
//   When this function is called, the shell calls CoCreateInstance
//  (or equivalent) with CLSCTX_INPROC_SERVER and the specified CLSID
//  from within the shell's process and release it immediately.
//
//-------------------------------------------------------------------------

{$EXTERNALSYM SHLoadInProc}
function SHLoadInProc(const clsid: TCLSID): HResult; stdcall;

//-------------------------------------------------------------------------
//
// SHEnableServiceObject
//
//   Like SHLoadInProc, but gives control over the object's lifetime
//  via fEnable parameter.  TRUE tells the shell to create the object
//  and hold onto it, FALSE tells the shell to look for the previously
//  created instance of the object and release it.
//
//-------------------------------------------------------------------------

{$EXTERNALSYM SHEnableServiceObject}
function SHEnableServiceObject(const rclsid: TCLSID; fEnable: BOOL): HResult stdcall;


//-------------------------------------------------------------------------
//
// Internet Shortcut Object
//
//-------------------------------------------------------------------------
// Cmds for CGID_ShortCut
const
  {$EXTERNALSYM ISHCUTCMDID_DOWNLOADICON}
  ISHCUTCMDID_DOWNLOADICON      = 0;
  {$EXTERNALSYM ISHCUTCMDID_INTSHORTCUTCREATE}
  ISHCUTCMDID_INTSHORTCUTCREATE = 1;

  {$EXTERNALSYM CMDID_INTSHORTCUTCREATE}
  CMDID_INTSHORTCUTCREATE = ISHCUTCMDID_INTSHORTCUTCREATE;

//
//  Helper function which returns a IShellFolder interface to the desktop
// folder. This is equivalent to call CoCreateInstance with CLSID_ShellDesktop.
//
//  CoCreateInstance(CLSID_Desktop, NULL,
//                   CLSCTX_INPROC, IID_IShellFolder, &pshf);
//
{$EXTERNALSYM SHGetDesktopFolder}
function SHGetDesktopFolder(out ppshf: IShellFolder): HResult; stdcall;


// IShellFolder IBindCtx* parameters. the IUnknown for these are
// accessed through IBindCtx::RegisterObjectParam/GetObjectParam
// use this to provide the data needed create IDLists through
// IShellFolder::ParseDisplayName(). this data applies to the last element
// of the name that is parsed (c:\foo\bar.txt, data applies to bar.txt)
// this makes creating these IDLists much faster that suppling the name only

const
  {$EXTERNALSYM STR_FILE_SYS_BIND_DATA}
  STR_FILE_SYS_BIND_DATA = 'File System Bind Data';

type                                               
  {$EXTERNALSYM IFileSystemBindData}
  IFileSystemBindData = interface(IUnknown)
  ['{01e18d10-4d8b-11d2-855d-006008059367}']
    function SetFindData(const pfd: TWin32FindDataW): HResult; stdcall;
    function GetFindData(out pfd: TWin32FindDataW): HResult; stdcall;
  end;

  {$EXTERNALSYM IShellDetails}
  IShellDetails = interface(IUnknown)
  ['{000214EC-0000-0000-C000-000000000046}']
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT; out pDetails: TShellDetails): HResult; stdcall;
    function ColumnClick(iColumn: UINT): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IObjMgr interface
//
//
// [Member functions]
//
// IObjMgr::Append(punk)
//   This function adds an object to the end of a list of objects.
//
// IObjMgr::Remove(punk)
//   This function removes an object from a list of objects.
//
// This is implemented by CLSID_ACLMulti so each AutoComplete List
// (CLSID_ACLHistory, CLSID_ACListISF, CLSID_ACLMRU) can be added.
// CLSID_ACLMulti's IEnumString will then be the union of the results
// from the COM Objects added.
//-------------------------------------------------------------------------

  {$EXTERNALSYM IObjMgr}
  IObjMgr = interface(IUnknown)
  ['{00BB2761-6A77-11D0-A535-00C04FD7D062}']
    function Append(punk: IUnknown): HResult; stdcall;
    function Remove(punk: IUnknown): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// ICurrentWorkingDirectory interface
//
//
// [Member functions]
//
// ICurrentWorkingDirectory::GetDirectory(PWideChar pwzPath, DWORD cchSize)
//   This function gets the Current Working Directory from a COM object that
//   stores such state.
//
// ICurrentWorkingDirectory::SetDirectory(PWideChar pwzPath)
//   This function sets the Current Working Directory of a COM object that
//   stores such state.
//
// This function can be used generically.  One COM object that implements it
// is CLSID_ACListISF so that the AutoComplete engine can complete relative
// paths.  SetDirectory() will set the "Current Working Directory" and
// AutoComplete with then complete both absolute and relative paths.
// For Example, if ::SetDirectory(L"C:\Program Files") is called, then
// the user can AutoComplete "..\winnt".  In order to set the current
// working directory for non-file system paths, "ftp://ftp.microsoft.com/" or
// "Control Panel" for example, use IPersistFolder.
//-------------------------------------------------------------------------

  {$EXTERNALSYM ICurrentWorkingDirectory}
  ICurrentWorkingDirectory = interface(IUnknown)
  ['{91956D21-9276-11D1-921A-006097DF5BD4}']
    function GetDirectory(pwzPath: PWideChar; cchSize: DWORD): HResult; stdcall;
    function SetDirectory(pwzPath: PWideChar): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IACList interface
//
//
// [Member functions]
//
// IObjMgr::Expand(LPCOLESTR)
//   This function tells an autocomplete list to expand a specific string.
//
// If the user enters a multi-level path, AutoComplete (CLSID_AutoComplete)
// will use this interface to tell the "AutoComplete Lists" where to expand
// the results.
//
// For Example, if the user enters "C:\Program Files\Micros", AutoComplete
// first completely enumerate the "AutoComplete Lists" via IEnumString.  Then it
// will call the "AutoComplete Lists" with IACList::Expand(L"C:\Program Files").
// It will then enumerate the IEnumString interface again to get results in
// that directory.
//-------------------------------------------------------------------------

  {$EXTERNALSYM IACList}
  IACList = interface(IUnknown)
  ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand: POleStr): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IACList2 interface
//
// [Description]
//              This interface exists to allow the caller to set filter criteria
// for an AutoComplete List.  AutoComplete Lists generates the list of
// possible AutoComplete completions.  CLSID_ACListISF is one AutoComplete
// List COM object that implements this interface.
//-------------------------------------------------------------------------

type
  {$EXTERNALSYM _tagAUTOCOMPLETELISTOPTIONS}
  _tagAUTOCOMPLETELISTOPTIONS = DWORD;
  {$EXTERNALSYM AUTOCOMPLETELISTOPTIONS}
  AUTOCOMPLETELISTOPTIONS = _tagAUTOCOMPLETELISTOPTIONS;
  TAutoCompleteListOptions = _tagAUTOCOMPLETELISTOPTIONS;

const
  {$EXTERNALSYM ACLO_NONE}
  ACLO_NONE        = 0;   // don't enumerate anything
  {$EXTERNALSYM ACLO_CURRENTDIR}
  ACLO_CURRENTDIR  = 1;   // enumerate current directory
  {$EXTERNALSYM ACLO_MYCOMPUTER}
  ACLO_MYCOMPUTER  = 2;   // enumerate MyComputer
  {$EXTERNALSYM ACLO_DESKTOP}
  ACLO_DESKTOP     = 4;   // enumerate Desktop Folder
  {$EXTERNALSYM ACLO_FAVORITES}
  ACLO_FAVORITES   = 8;   // enumerate Favorites Folder
  {$EXTERNALSYM ACLO_FILESYSONLY}
  ACLO_FILESYSONLY = 16;  // enumerate only the file system
  {$EXTERNALSYM ACLO_FILESYSDIRS}
  ACLO_FILESYSDIRS = 32;  // enumerate only the file system dirs, UNC shares, and UNC servers.

type
  {$EXTERNALSYM IACList2}
  IACList2 = interface(IACList)
  ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

(*-------------------------------------------------------------------------*\
    INTERFACE: IProgressDialog

    DESCRIPTION:
        CLSID_ProgressDialog/IProgressDialog exist to allow a caller to create
    a progress dialog, set it's title, animation, text lines, progress, and
    it will do all the work of updating on a background thread, being modless,
    handling the user cancelling the operation, and estimating the time remaining
    until the operation completes.

    USAGE:
        This is how the dialog is used during operations that require progress
    and the ability to cancel:
    {
        DWORD dwComplete, dwTotal;
        IProgressDialog * ppd;
        CoCreateInstance(CLSID_ProgressDialog, NULL, CLSCTX_INPROC_SERVER, IID_IProgressDialog, (void ** )&ppd);
        ppd->SetTitle(L"My Slow Operation");                                // Set the title of the dialog.
        ppd->SetAnimation(hInstApp, IDA_OPERATION_ANIMATION);               // Set the animation to play.
        ppd->StartProgressDialog(hwndParent, punk, PROGDLG_AUTOTIME, NULL); // Display and enable automatic estimated time remaining.
        ppd->SetCancelMsg(L"Please wait while the current operation is cleaned up", NULL);   // Will only be displayed if Cancel button is pressed.

        dwComplete = 0;
        dwTotal = CalcTotalUnitsToDo();

        // Reset because CalcTotalUnitsToDo() took a long time and the estimated time
        // is based on the time between ::StartProgressDialog() and the first
        // ::SetProgress() call.
        ppd->Timer(PDTIMER_RESET, NULL);

        for (nIndex = 0; nIndex < nTotal; nIndex++)
        {
            if (TRUE == ppd->HasUserCancelled())
                break;

            ppd->SetLine(2, L"I'm processing item n", FALSE, NULL);
            dwComplete += DoSlowOperation();

            ppd->SetProgress(dwCompleted, dwTotal);
        }

        ppd->StopProgressDialog();
        ppd->Release();
    }
\*-------------------------------------------------------------------------*)

const
// Flags for IProgressDialog::StartProgressDialog() (dwFlags)
  {$EXTERNALSYM PROGDLG_NORMAL}
  PROGDLG_NORMAL        = $00000000;      // default normal progress dlg behavior
  {$EXTERNALSYM PROGDLG_MODAL}
  PROGDLG_MODAL         = $00000001;      // the dialog is modal to its hwndParent (default is modeless)
  {$EXTERNALSYM PROGDLG_AUTOTIME}
  PROGDLG_AUTOTIME      = $00000002;      // automatically updates the "Line3" text with the "time remaining" (you cant call SetLine3 if you passs this!)
  {$EXTERNALSYM PROGDLG_NOTIME}
  PROGDLG_NOTIME        = $00000004;      // we dont show the "time remaining" if this is set. We need this if dwTotal < dwCompleted for sparse files
  {$EXTERNALSYM PROGDLG_NOMINIMIZE}
  PROGDLG_NOMINIMIZE    = $00000008;      // Do not have a minimize button in the caption bar.
  {$EXTERNALSYM PROGDLG_NOPROGRESSBAR}
  PROGDLG_NOPROGRESSBAR = $00000010;      // Don't display the progress bar

// Time Actions (dwTimerAction)
  {$EXTERNALSYM PDTIMER_RESET}
  PDTIMER_RESET         = $00000001;       // Reset the timer so the progress will be calculated from now until the first ::SetProgress() is called so
                                           // those this time will correspond to the values passed to ::SetProgress().  Only do this before ::SetProgress() is called.

type                                           
  {$EXTERNALSYM IProgressDialog}
  IProgressDialog = interface(IUnknown)
  ['{EBBC7C04-315E-11D2-B62F-006097DF5BD4}']
    function StartProgressDialog(hwndParent: HWND;
      punkEnableModless: IUnknown; dwFlags: DWORD;
      pvResevered: Pointer): HResult; stdcall;
    function StopProgressDialog: HResult; stdcall;
    function SetTitle(pwzTitle: PWideChar): HResult; stdcall;
    function SetAnimation(hInstAnimation: THandle;
      idAnimation: UINT): HResult; stdcall;
    function HasUserCancelled: BOOL; stdcall;
    function SetProgress(dwCompleted: DWORD; dwTotal: DWORD): HResult; stdcall;
    function SetProgress64(ullCompleted, ullTotal: Int64): HResult; stdcall;
    function SetLine(dwLineNum: DWORD; pwzString: PWideChar;
      fCompactPath: BOOL; pvResevered: Pointer): HResult; stdcall;
    function SetCancelMsg(pwzCancelMsg: PWideChar;
      pvResevered:Pointer): HResult; stdcall;
    function Timer(dwTimerAction: DWORD;
      pvResevered: Pointer): HResult; stdcall;
  end;

//==========================================================================
// IInputObjectSite/IInputObject interfaces
//
//  These interfaces allow us (or ISVs) to install/update external Internet
// Toolbar for IE and the shell. The frame will simply get the CLSID from
// registry (to be defined) and CoCreateInstance it.
//
//==========================================================================

//-------------------------------------------------------------------------
//
// IInputObjectSite interface
//
//   A site implements this interface so the object can communicate
// focus change to it.
//
// [Member functions]
//
// IInputObjectSite::OnFocusChangeIS(punkObj, fSetFocus)
//   Object (punkObj) is getting or losing the focus.
//
//-------------------------------------------------------------------------

  {$EXTERNALSYM IInputObjectSite}
  IInputObjectSite = interface(IUnknown)
  ['{F1DB8392-7331-11D0-8C99-00A0C92DBFE8}']
    function OnFocusChangeIS(punkObj: IUnknown; fSetFocus: BOOL): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IInputObject interface
//
//   An object implements this interface so the site can communicate
// activation and accelerator events to it.
//
// [Member functions]
//
// IInputObject::UIActivateIO(fActivate, lpMsg)
//   Activates or deactivates the object.  lpMsg may be NULL.  Returns
//   S_OK if the activation succeeded.
//
// IInputObject::HasFocusIO()
//   Returns S_OK if the object has the focus, S_FALSE if not.
//
// IInputObject::TranslateAcceleratorIO(lpMsg)
//   Allow the object to process the message.  Returns S_OK if the
//   message was processed (eaten).
//
//-------------------------------------------------------------------------

  {$EXTERNALSYM IInputObject}
  IInputObject = interface(IUnknown)
  ['{68284FAA-6A48-11D0-8C78-00C04FD918B4}']
    function UIActivateIO(fActivate: BOOL; var lpMsg: TMsg): HResult; stdcall;
    function HasFocusIO: HResult; stdcall;
    function TranslateAcceleratorIO(var lpMsg: TMsg): HResult; stdcall;
  end;

//==========================================================================
// IDockingWindowSite/IDockingWindow/IDockingWindowFrame interfaces
// IInputObjectSite/IInputObject interfaces
//
//  These interfaces allow us (or ISVs) to install/update external Internet
// Toolbar for IE and the shell. The frame will simply get the CLSID from
// registry (to be defined) and CoCreateInstance it.
//
//==========================================================================


//-------------------------------------------------------------------------
//
// IDockingWindowSite interface
//
//   A site implements this interface so the object can negotiate for
// and inquire about real estate on the site.
//
// [Member functions]
//
// IDockingWindowSite::GetBorderDW(punkObj, prcBorder)
//   Site returns the bounding rectangle of the given source object
//   (punkObj).
//
// IDockingWindowSite::RequestBorderSpaceDW(punkObj, pbw)
//   Object requests that the site makes room for it, as specified in
//   *pbw.
//
// IDockingWindowSite::SetBorderSpaceDW(punkObj, pbw)
//   Object requests that the site set the border spacing to the size
//   specified in *pbw.
//
//-------------------------------------------------------------------------

  {$EXTERNALSYM IDockingWindowSite}
  IDockingWindowSite = interface(IOleWindow)
  ['{2A342FC2-7B26-11D0-8CA9-00A0C92DBFE8}']
    function GetBorderDW(punkObj: IUnknown; var prcBorder: TRect): HResult; stdcall;
    function RequestBorderSpaceDW(punkObj: IUnknown; out pbw: TBorderWidths): HResult; stdcall;
    function SetBorderSpaceDW(punkObj: IUnknown; var pbw: TBorderWidths): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IDockingWindowFrame interface
//
// [Member functions]
//
// IDockingWindowFrame::AddToolbar(punkSrc, pwszItem, dwReserved)
//
// IDockingWindowFrame::RemoveToolbar(punkSrc, dwRemoveFlags)
//
// IDockingWindowFrame::FindToolbar(pwszItem, riid, ppv)
//
//-------------------------------------------------------------------------

// flags for RemoveToolbar
const
  {$EXTERNALSYM DWFRF_NORMAL}
  DWFRF_NORMAL           = $0000;
  {$EXTERNALSYM DWFRF_DELETECONFIGDATA}
  DWFRF_DELETECONFIGDATA = $0001;

// flags for AddToolbar
  DWFAF_HIDDEN = $0001;   // add hidden

type
  {$EXTERNALSYM IDockingWindowFrame}
  IDockingWindowFrame = interface(IOleWindow)
  ['{47D2657A-7B27-11D0-8CA9-00A0C92DBFE8}']
    function AddToolbar(punkSrc: IUnknown; pwszItem: PWideChar; dwAddFlags: DWORD): HResult; stdcall;
    function RemoveToolbar(punkSrc: IUnknown; dwRemoveFlags: DWORD): HResult; stdcall;
    function FindToolbar(pwszItem: PWideChar; const riid: TIID; out ppv): HResult; stdcall;
  end;

//-------------------------------------------------------------------------
//
// IRunnableTask interface
//
//   This is a free threaded interface used for putting items on a background
// scheduler for execution within the view.  It allows a scheduler to start and
// stop tasks on as many worker threads as it deems necessary.
//
// Run(), Kill() and Suspend() may be called from different threads.
//
// [Member functions]
//
// IRunnableTask::Run(void)
//   Initiate the task to run.  This should return E_PENDING if the task
//   has been suspended.
//
// IRunnableTask::Kill(void)
//
// IRunnableTask::Suspend(void)
//
// IRunnableTask::Resume(void)
//
// IRunnableTask::IsRunning(void)
//
//-------------------------------------------------------------------------

const
  // Convenient state values
  {$EXTERNALSYM IRTIR_TASK_NOT_RUNNING}
  IRTIR_TASK_NOT_RUNNING = 0;
  {$EXTERNALSYM IRTIR_TASK_RUNNING}
  IRTIR_TASK_RUNNING     = 1;
  {$EXTERNALSYM IRTIR_TASK_SUSPENDED}
  IRTIR_TASK_SUSPENDED   = 2;
  {$EXTERNALSYM IRTIR_TASK_PENDING}
  IRTIR_TASK_PENDING     = 3;
  {$EXTERNALSYM IRTIR_TASK_FINISHED}
  IRTIR_TASK_FINISHED    = 4;

type
  {$EXTERNALSYM IRunnableTask}
  IRunnableTask = interface(IUnknown )
  ['{85788D00-6807-11D0-B810-00C04FD706EC}']
    function Run: HResult; stdcall;
    function Kill(fWait: BOOL): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function IsRunning: ULONG; stdcall;
  end;

type
  {$EXTERNALSYM TASKOWNERID}
  TASKOWNERID = TGUID;
  TTaskOwnerID = TGUID;
  PTaskOwnerID = PGUID;

// ---IShellTaskScheduler
// An interface for interacting with and controlling a task scheduler. This
// interface does not need to be free-threaded unless the items in the queue
// interact with the scheduler as well as the main execution thread on which the
// task scheduler was created.

// IShellTaskScheduler::AddTask()
//      Adds Tasks to the scheduler's background queue. The TASKOWNERID allow particular types
//      of tasks to be grouped so that they can be counted or removed. The lParam allows the task
//      to be associated with a particular item (for example an item in a listview).
// IShellTaskScheduler::RemoveTasks()
//      Removes tasks from the scheduler's queue. These can be sepcified in terms of their TASKOWNERID
//      or their LPARAM, or both, or neither (TOID_NULL && ITSAT_DEFAULT_LPARAM results in all tasks being
//      removed). If a task that matches is currently running and ITaskScheduler::Status() has been
//      passeed ITSSFLAG_KILL_ON_DESTROY then the scheduler will attempt to kill the current task. The
//      fWaitIfRunning parameter is then passed to IRunnableTask::Kill().
// IShellTaskScheduler::CountTasks()
//      Counts the tasks in the queue depending upon the TASKOWNERID and the LPARAM passed. (TOID_NULL and
//      ITSAT_DEFAULT_LPARAM will count all tasks in the queue)
// IShellTaskScheduler::Status()
//      This sets the ReleaseStatus for the current task and the background thread timeout. When
//      ITaskScheduler::RemoveTasks() is called and there is a task currently running that matches
//      ITSSFLAG_COMPLETE_ON_DESTROY will cause TRUE to be passed to the task's IRunnableTask::Kill().
//      The dwThreadTimeout parameter if not set to the default will cause the background thread to
//      die if no new tasks have been added to the queue in the timeout period. The Thread will be
//      recreated when the next new task is added.

const
////////////////////////
// Status() flags,
// wait for the current task to complete before deleting the scheduler
  {$EXTERNALSYM ITSSFLAG_COMPLETE_ON_DESTROY}
  ITSSFLAG_COMPLETE_ON_DESTROY        = $0000;

// kill the current task (if there is one) when the task scheduler is deleted
  {$EXTERNALSYM ITSSFLAG_KILL_ON_DESTROY}
  ITSSFLAG_KILL_ON_DESTROY            = $0001;

  {$EXTERNALSYM ITSSFLAG_SUPPORTS_TERMINATE}
  ITSSFLAG_SUPPORTS_TERMINATE         = $0002;

  {$EXTERNALSYM ITSSFLAG_FLAGS_MASK}
  ITSSFLAG_FLAGS_MASK                 = $0003;

// set the timeout for killing the thread when the object is terminated.
// this timeout can be used to stop the object from blocking the system
// indefinitely.
  {$EXTERNALSYM ITSSFLAG_THREAD_TERMINATE_TIMEOUT}
  ITSSFLAG_THREAD_TERMINATE_TIMEOUT   = $0010;

// set the timeout for threads that are idle in the thread pool
  {$EXTERNALSYM ITSSFLAG_THREAD_POOL_TIMEOUT}
  ITSSFLAG_THREAD_POOL_TIMEOUT        = $0020;

// The default timeout passed to release Status to determine how long the thread
// can be asleep before the thread is expired
  {$EXTERNALSYM ITSS_THREAD_DESTROY_DEFAULT_TIMEOUT}
  ITSS_THREAD_DESTROY_DEFAULT_TIMEOUT = 60*1000;

// default, we won't kill it...
  {$EXTERNALSYM ITSS_THREAD_TERMINATE_TIMEOUT}
  ITSS_THREAD_TERMINATE_TIMEOUT       = INFINITE;

// there is no change to the thread timeout
  {$EXTERNALSYM ITSS_THREAD_TIMEOUT_NO_CHANGE}
  ITSS_THREAD_TIMEOUT_NO_CHANGE       = INFINITE - 1;

// the LPARAM allows task to be associated with items thus all tasks owned by a
// particular item can be accessed by passing a non default value for this parameter
  {$EXTERNALSYM ITSAT_DEFAULT_LPARAM}
  ITSAT_DEFAULT_LPARAM                = $FFFFFFFF;

// Task priorities
// ---------------
// This depends on the cooperation of tasks currently under execution. New tasks will
// be inserted in the queue in priority order. If a task of a low priority is currently
// under execution when a higher priority task is added, the scheduler will attempt
// to suspend the task currently under execution. It will be resumed when the other tasks
// have been completed.

  {$EXTERNALSYM TOID_NULL}
  TOID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}'; // CLSID_NULL

type
  {$EXTERNALSYM IShellTaskScheduler}
  IShellTaskScheduler = interface(IUnknown )
  ['{6CCB7BE0-6807-11D0-B810-00C04FD706EC}']
    function AddTask(pTask: IRunnableTask; const rtoid: TTaskOwnerID;
      lParam: DWORD; dwPriority: DWORD): HResult; stdcall;
    function RemoveTasks(const rtoid: TTaskOwnerID; lParam: DWORD;
      fWaitIfRunning: BOOL): HResult; stdcall;
    function CountTasks(const rtoid: TTaskOwnerID): UINT; stdcall;
    function Status(dwReleaseStatus: DWORD; dwThreadTimeout: DWORD ): HResult; stdcall;
  end;

const
  {$EXTERNALSYM ITSSFLAG_TASK_PLACEINFRONT}
  ITSSFLAG_TASK_PLACEINFRONT = $00000001;
  {$EXTERNALSYM ITSSFLAG_TASK_PLACEINBACK}
  ITSSFLAG_TASK_PLACEINBACK  = $00000002;

type
  {$EXTERNALSYM IShellTaskScheduler2}
  IShellTaskScheduler2 = interface(IShellTaskScheduler)
  // ['{}']  IID is unknown?
    function AddTask2(pTask: IRunnableTask; const rtoid: TTaskOwnerID;
      lParam: DWORD; dwPriority: DWORD; grfFlags: DWORD): HResult; stdcall;
    function MoveTask(rtoid: PTaskOwnerID; lParam: DWORD; dwPriority: DWORD;
      grfFlags: DWORD): HResult; stdcall;
  end;

(* ***************** IThumbnailCapture
 * CaptureThumbnail : takes an IHTMLDocument2 and returns a thumbnail of specified
 *                    size as an hbitmap
 *)

  {$EXTERNALSYM IThumbnailCapture}
  IThumbnailCapture = interface(IUnknown )
  ['{4EA39266-7211-409F-B622-F63DBD16C533}']
    // *** IThumbnailCapture methods ***
    function CaptureThumbnail(const pMaxSize: TSize; pHTMLDoc2: IUnknown;
      out phbmThumbnail: HBITMAP ): HResult; stdcall;
  end;

  PEnumShellImageStoreData = ^TEnumShellImageStoreData;
  {$EXTERNALSYM _EnumImageStoreDATAtag}
  _EnumImageStoreDATAtag = record
    szPath: array[0..MAX_PATH - 1] of WideChar;
    ftTimeStamp: TFileTime;
  end;
  {$EXTERNALSYM ENUMSHELLIMAGESTOREDATA}
  ENUMSHELLIMAGESTOREDATA = _EnumImageStoreDATAtag;
  TEnumShellImageStoreData = _EnumImageStoreDATAtag;

  {$EXTERNALSYM IEnumShellImageStore}
  IEnumShellImageStore = interface(IUnknown)
  ['{6DFD582B-92E3-11D1-98A3-00C04FB687DA}']
    function  Reset: HResult; stdcall;
    function  Next(celt: ULONG; var prgElt: PEnumShellImageStoreData; out pceltFetched: ULONG ): HResult; stdcall;
    function  Skip(celt: ULONG): HResult; stdcall;
    function  Clone(out ppEnum: IEnumShellImageStore ): HResult; stdcall;
  end;

const
// flags used to determine the capabilities of the storage for the images
  {$EXTERNALSYM SHIMSTCAPFLAG_LOCKABLE}
  SHIMSTCAPFLAG_LOCKABLE    = $0001;       // does the store require/support locking
  {$EXTERNALSYM SHIMSTCAPFLAG_PURGEABLE}
  SHIMSTCAPFLAG_PURGEABLE   = $0002;       // does the store require dead items purging externally ?

type
// this interface is used to manipulate the Image cache. It can potentially be used
// in a free threaded manner in conjunction with the Lock parameter to Open and close
  {$EXTERNALSYM IShellImageStore}
  IShellImageStore = interface(IUnknown )
  ['{48C8118C-B924-11D1-98D5-00C04FB687DA}']

    // if the lock parameter is used, then all other calls into
    // open and/or create will block until the lock is released.
    function Open(dwMode: DWORD; out pdwLock: DWORD): HResult; stdcall;
    function Create_(dwMode: DWORD; out pdwLock: DWORD): HResult; stdcall;

    // if the lock is passed to either of these two methods, it releases the lock
    // once the operation is complete.
    function ReleaseLock(var pdwLock: DWORD): HResult; stdcall;
    function Close(var pdwLock: DWORD ): HResult; stdcall;
    function Commit(var pdwLock: DWORD): HResult; stdcall;
    function IsLocked: HResult; stdcall;
    function GetMode(out pdwMode: DWORD): HResult; stdcall;
    function GetCapabilities(out pdwCapMask: DWORD): HResult; stdcall;
    function AddEntry(pszName: PWideChar; const pftTimeStamp: TFileTime;
      dwMode: DWORD; hImage: HBITMAP ): HResult; stdcall;
    function GetEntry(pszName: PWideChar; dwMode: DWORD;
      out phImage: HBITMAP): HResult; stdcall;
    function DeleteEntry(pszName: PWideChar): HResult; stdcall;
    function IsEntryInStore(pszName: PWideChar;
      var pftTimeStamp: TFileTime): HResult; stdcall;
    function Enum(out ppEnum: IEnumShellImageStore): HResult; stdcall;
  end;

////  IShellFolderBand

const
// Field mask
  {$EXTERNALSYM ISFB_MASK_STATE}
  ISFB_MASK_STATE          = $00000001; // TRUE if dwStateMask and dwState is valid
  {$EXTERNALSYM ISFB_MASK_BKCOLOR}
  ISFB_MASK_BKCOLOR        = $00000002; // TRUE if crBkgnd field is valid
  {$EXTERNALSYM ISFB_MASK_VIEWMODE}
  ISFB_MASK_VIEWMODE       = $00000004; // TRUE if wViewMode field is valid
  {$EXTERNALSYM ISFB_MASK_SHELLFOLDER}
  ISFB_MASK_SHELLFOLDER    = $00000008;
  {$EXTERNALSYM ISFB_MASK_IDLIST}
  ISFB_MASK_IDLIST         = $00000010;
  {$EXTERNALSYM ISFB_MASK_COLORS}
  ISFB_MASK_COLORS         = $00000020; // TRUE if crXXXX fields are valid (except bkgnd)

  {$EXTERNALSYM ISFB_STATE_DEFAULT}
  ISFB_STATE_DEFAULT       = $00000000;
  {$EXTERNALSYM ISFB_STATE_DEBOSSED}
  ISFB_STATE_DEBOSSED      = $00000001;
  {$EXTERNALSYM ISFB_STATE_ALLOWRENAME}
  ISFB_STATE_ALLOWRENAME   = $00000002;
  {$EXTERNALSYM ISFB_STATE_NOSHOWTEXT}
  ISFB_STATE_NOSHOWTEXT    = $00000004; // TRUE if _fNoShowText
  {$EXTERNALSYM ISFB_STATE_CHANNELBAR}
  ISFB_STATE_CHANNELBAR    = $00000010; // TRUE if we want NavigateTarget support
  {$EXTERNALSYM ISFB_STATE_QLINKSMODE}
  ISFB_STATE_QLINKSMODE    = $00000020; // TRUE if we want to turn off drag & drop onto content items
  {$EXTERNALSYM ISFB_STATE_FULLOPEN}
  ISFB_STATE_FULLOPEN      = $00000040; // TRUE if band should maximize when opened
  {$EXTERNALSYM ISFB_STATE_NONAMESORT}
  ISFB_STATE_NONAMESORT    = $00000080; // TRUE if band should _not_ sort icons by name
  {$EXTERNALSYM ISFB_STATE_BTNMINSIZE}
  ISFB_STATE_BTNMINSIZE    = $00000100; // TRUE if band should report min thickness of button

  {$EXTERNALSYM ISFBVIEWMODE_SMALLICONS}
  ISFBVIEWMODE_SMALLICONS  = $0001;
  {$EXTERNALSYM ISFBVIEWMODE_LARGEICONS}
  ISFBVIEWMODE_LARGEICONS  = $0002;
  {$EXTERNALSYM ISFBVIEWMODE_LOGOS}
  ISFBVIEWMODE_LOGOS       = $0003;

type
  PBandInfoSFB = ^TBandInfoSFB;
  {$EXTERNALSYM BANDINFOSFB}
  BANDINFOSFB = record
    dwMask: DWORD;       // [in] ISFB_MASK mask of valid fields from crBkgnd on
    dwStateMask: DWORD;  // [in] ISFB_STATE mask of dwState bits being set/queried
    dwState: DWORD;      // [in/out] ISFB_STATE bits
    crBkgnd: TColorRef;  // [in/out]
    crBtnLt: TColorRef;  // [in/out]
    crBtnDk: TColorRef;  // [in/out]
    wViewMode: Word;     // [in/out]
    wAlign: Word;        // not used (yet)
    psf: IShellFolder;   // [out]
    pidl: PItemIDList;   // [out]
  end;
  TBandInfoSFB = BANDINFOSFB;

type
  {$EXTERNALSYM IShellFolderBand}
  IShellFolderBand = interface(IUnknown)
  ['{7FE80CC8-C247-11D0-B93A-00A0C90312E1}']
    function InitializeSFB(psf: IShellFolder; pidl: PItemIDList): HResult; stdcall;
    function SetBandInfoSFB(const pbi: TBandInfoSFB): HResult; stdcall;
    function GetBandInfoSFB(out pbi: TBandInfoSFB): HResult; stdcall;
  end;

const
// Command Target IDs
  {$EXTERNALSYM SFBID_PIDLCHANGED}
  SFBID_PIDLCHANGED = 0;

////  IDeskBarClient
type
  {$EXTERNALSYM IDeskBarClient}
  IDeskBarClient = interface(IOleWindow)
  // ['{}'] IID_IDeskBarClient unknown.
    function SetDeskBarSite(punkSite: IUnknown): HResult; stdcall;
    function SetModeDBC(dwMode: DWORD): HResult; stdcall;
    function UIActivateDBC(dwState: DWORD): HResult; stdcall;
    function GetSize(dwWhich: DWORD; out prc: TRect): HResult; stdcall;
  end;

const
  {$EXTERNALSYM DBC_GS_IDEAL}
  DBC_GS_IDEAL     = 0;  // get the ideal size
  {$EXTERNALSYM DBC_GS_SIZEDOWN}
  DBC_GS_SIZEDOWN  = 1;  // clip the height of a rect to a multiple of the rebar's integral size

  {$EXTERNALSYM DBC_HIDE}
  DBC_HIDE         = 0; // Band is hidden (being destroyed)
  {$EXTERNALSYM DBC_SHOW}
  DBC_SHOW         = 1; // Band is visible
  {$EXTERNALSYM DBC_SHOWOBSCURE}
  DBC_SHOWOBSCURE  = 2; // Band is completely obscured

  {$EXTERNALSYM DBCID_EMPTY}
  DBCID_EMPTY      = 0; // bandsite is empty
  {$EXTERNALSYM DBCID_ONDRAG}
  DBCID_ONDRAG     = 1; // (down)DragMoveEnter/Leave vaIn:I4:eDrag
  {$EXTERNALSYM DBCID_CLSIDOFBAR}
  DBCID_CLSIDOFBAR = 2; // clsid of bar inside
  {$EXTERNALSYM DBCID_RESIZE}
  DBCID_RESIZE     = 3; // resize from keyboard
  {$EXTERNALSYM DBCID_GETBAR}
  DBCID_GETBAR     = 4; // returns vaOut:VT_UNKNOWN of hosting dockbar (IDeskBar)

//
// We need to make sure that WININET.H is included before this interface is
// used because the COMPONENT structure uses INTERNET_MAX_URL_LENGTH
//

//
//  Flags and structures used by IActiveDesktop
//
type
  PWallPaperOpt = ^TWallPaperOpt;
  {$EXTERNALSYM _tagWALLPAPEROPT}
  _tagWALLPAPEROPT = record
    dwSize: DWORD;     // size of this Structure.
    dwStyle: DWORD;    // WPSTYLE_* mentioned above
  end;
  {$EXTERNALSYM WALLPAPEROPT}
  WALLPAPEROPT = _tagWALLPAPEROPT;
  TWallPaperOpt = _tagWALLPAPEROPT;

  PComponentsOpt = ^TComponentsOpt;
  {$EXTERNALSYM _tagCOMPONENTSOPT}
  _tagCOMPONENTSOPT = record
    dwSize: DWORD;            // Size of this structure
    fEnableComponents: BOOL;  // Enable components?
    fActiveDesktop: BOOL;     // Active desktop enabled ?
  end;
  {$EXTERNALSYM COMPONENTSOPT}
  COMPONENTSOPT = _tagCOMPONENTSOPT;
  TComponentsOpt = _tagCOMPONENTSOPT;

  PCompPos = ^TCompPos;
  {$EXTERNALSYM _tagCOMPPOS}
  _tagCOMPPOS = record
    dwSize: DWORD;                  // Size of this structure
    iLeft: Integer;                 // Left of top-left corner in screen co-ordinates.
    iTop: Integer;                  // Top of top-left corner in screen co-ordinates.
    dwWidth: DWORD;                 // Width in pixels.
    dwHeight: DWORD;                // Height in pixels.
    izIndex: Integer;               // Indicates the Z-order of the component.
    fCanResize: BOOL;               // Is the component resizeable?
    fCanResizeX: BOOL;              // Resizeable in X-direction?
    fCanResizeY: BOOL;              // Resizeable in Y-direction?
    iPreferredLeftPercent: Integer; // Left of top-left corner as percent of screen width
    iPreferredTopPercent: Integer;  // Top of top-left corner as percent of screen height
  end;
  {$EXTERNALSYM COMPPOS}
  COMPPOS = _tagCOMPPOS;
  TCompPos = _tagCOMPPOS;

  PCompStateInfo = ^TCompStateInfo;
  {$EXTERNALSYM _tagCOMPSTATEINFO}
  _tagCOMPSTATEINFO = record
    dwSize: DWORD;             // Size of this structure.
    iLeft: Integer;            // Left of the top-left corner in screen co-ordinates.
    iTop: Integer;             // Top of top-left corner in screen co-ordinates.
    dwWidth: DWORD;            // Width in pixels.
    dwHeight: DWORD;           // Height in pixels.
    dwItemState: DWORD;        // State of the component (full-screen mode or split-screen or normal state.
  end;
  {$EXTERNALSYM COMPSTATEINFO}
  COMPSTATEINFO = _tagCOMPSTATEINFO;
  TCompStateInfo = _tagCOMPSTATEINFO;

const
  {$EXTERNALSYM COMPONENT_TOP}
  COMPONENT_TOP = $3FFFFFFF;   // izOrder value meaning component is at the top

  // iCompType values
  {$EXTERNALSYM COMP_TYPE_HTMLDOC}
  COMP_TYPE_HTMLDOC = 0;
  {$EXTERNALSYM COMP_TYPE_PICTURE}
  COMP_TYPE_PICTURE = 1;
  {$EXTERNALSYM COMP_TYPE_WEBSITE}
  COMP_TYPE_WEBSITE = 2;
  {$EXTERNALSYM COMP_TYPE_CONTROL}
  COMP_TYPE_CONTROL = 3;
  {$EXTERNALSYM COMP_TYPE_CFHTML}
  COMP_TYPE_CFHTML  = 4;
  {$EXTERNALSYM COMP_TYPE_MAX}
  COMP_TYPE_MAX     = 4;

type
// The following is the COMPONENT structure used in IE4.01, IE4.0 and Memphis. It is kept here for compatibility
// reasons.
  PIE4Component = ^TIE4Component;
  {$EXTERNALSYM _tagIE4COMPONENT}
  _tagIE4COMPONENT = record
    dwSize: DWORD;               // Size of this structure
    dwID: DWORD;                 // Reserved: Set it always to zero.
    iComponentType: Integer;     // One of COMP_TYPE_*
    fChecked: BOOL;              // Is this component enabled?
    fDirty: BOOL;                // Had the component been modified and not yet saved to disk?
    fNoScroll: BOOL;             // Is the component scrollable?
    cpPos: TCompPos;             // Width, height etc.,
    wszFriendlyName: array[0..MAX_PATH - 1] of WideChar;          // Friendly name of component.
    wszSource: array[0..INTERNET_MAX_URL_LENGTH - 1] of WideChar; // URL of the component.
    wszSubscribedURL: array[0..INTERNET_MAX_URL_LENGTH - 1] of WideChar; // Subscrined URL
  end;
  {$EXTERNALSYM IE4COMPONENT}
  IE4COMPONENT = _tagIE4COMPONENT;
  TIE4Component = _tagIE4COMPONENT;

//
// The following is the new NT5 component structure. Note that the initial portion of this component exactly
// matches the IE4COMPONENT structure. All new fields are added at the bottom and the dwSize field is used to
// distinguish between IE4COMPONENT and the new COMPONENT structures.
//
  PComponent = ^COMPONENT;
  {$EXTERNALSYM _tagCOMPONENT}
  _tagCOMPONENT = record
    dwSize: DWORD;               // Size of this structure
    dwID: DWORD;                 // Reserved: Set it always to zero.
    iComponentType: Integer;     // One of COMP_TYPE_*
    fChecked: BOOL;              // Is this component enabled?
    fDirty: BOOL;                // Had the component been modified and not yet saved to disk?
    fNoScroll: BOOL;             // Is the component scrollable?
    cpPos: COMPPOS;              // Width, height etc.,
    wszFriendlyName: array[0..MAX_PATH - 1] of WideChar;          // Friendly name of component.
    wszSource: array[0..INTERNET_MAX_URL_LENGTH - 1] of WideChar; // URL of the component.
    wszSubscribedURL: array[0..INTERNET_MAX_URL_LENGTH - 1] of WideChar; // Subscrined URL

    //New fields are added below. Everything above here must exactly match the IE4COMPONENT Structure.
    dwCurItemState: DWORD;       // Current state of the Component.
    csiOriginal: TCompStateInfo; // Original state of the component when it was first added.
    csiRestored: TCompStateInfo; // Restored state of the component.
  end;
  {$EXTERNALSYM COMPONENT}
  COMPONENT = _tagCOMPONENT;
  // TComponent declaration omitted, for obvious reasons.

const
// Defines for dwCurItemState
  {$EXTERNALSYM IS_NORMAL}
  IS_NORMAL                 = $00000001;
  {$EXTERNALSYM IS_FULLSCREEN}
  IS_FULLSCREEN             = $00000002;
  {$EXTERNALSYM IS_SPLIT}
  IS_SPLIT                  = $00000004;
  {$EXTERNALSYM IS_VALIDSIZESTATEBITS}
  IS_VALIDSIZESTATEBITS     = IS_NORMAL or IS_SPLIT or IS_FULLSCREEN;  // The set of IS_* state bits which define the "size" of the component - these bits are mutually exclusive.
  {$EXTERNALSYM IS_VALIDSTATEBITS}
  IS_VALIDSTATEBITS         = IS_NORMAL or IS_SPLIT or IS_FULLSCREEN or $80000000 or $40000000;  // All of the currently defined IS_* bits.

////////////////////////////////////////////
// Flags for IActiveDesktop::ApplyChanges()
  {$EXTERNALSYM AD_APPLY_SAVE}
  AD_APPLY_SAVE             = $00000001;
  {$EXTERNALSYM AD_APPLY_HTMLGEN}
  AD_APPLY_HTMLGEN          = $00000002;
  {$EXTERNALSYM AD_APPLY_REFRESH}
  AD_APPLY_REFRESH          = $00000004;
  {$EXTERNALSYM AD_APPLY_ALL}
  AD_APPLY_ALL              = AD_APPLY_SAVE or AD_APPLY_HTMLGEN or AD_APPLY_REFRESH;
  {$EXTERNALSYM AD_APPLY_FORCE}
  AD_APPLY_FORCE            = $00000008;
  {$EXTERNALSYM AD_APPLY_BUFFERED_REFRESH}
  AD_APPLY_BUFFERED_REFRESH = $00000010;
  {$EXTERNALSYM AD_APPLY_DYNAMICREFRESH}
  AD_APPLY_DYNAMICREFRESH   = $00000020;

////////////////////////////////////////////
// Flags for IActiveDesktop::GetWallpaperOptions()
//           IActiveDesktop::SetWallpaperOptions()
  {$EXTERNALSYM WPSTYLE_CENTER}
  WPSTYLE_CENTER  = 0;
  {$EXTERNALSYM WPSTYLE_TILE}
  WPSTYLE_TILE    = 1;
  {$EXTERNALSYM WPSTYLE_STRETCH}
  WPSTYLE_STRETCH = 2;
  {$EXTERNALSYM WPSTYLE_MAX}
  WPSTYLE_MAX     = 3;

////////////////////////////////////////////
// Flags for IActiveDesktop::ModifyComponent()

  {$EXTERNALSYM COMP_ELEM_TYPE}
  COMP_ELEM_TYPE          = $00000001;
  {$EXTERNALSYM COMP_ELEM_CHECKED}
  COMP_ELEM_CHECKED       = $00000002;
  {$EXTERNALSYM COMP_ELEM_DIRTY}
  COMP_ELEM_DIRTY         = $00000004;
  {$EXTERNALSYM COMP_ELEM_NOSCROLL}
  COMP_ELEM_NOSCROLL      = $00000008;
  {$EXTERNALSYM COMP_ELEM_POS_LEFT}
  COMP_ELEM_POS_LEFT      = $00000010;
  {$EXTERNALSYM COMP_ELEM_POS_TOP}
  COMP_ELEM_POS_TOP       = $00000020;
  {$EXTERNALSYM COMP_ELEM_SIZE_WIDTH}
  COMP_ELEM_SIZE_WIDTH    = $00000040;
  {$EXTERNALSYM COMP_ELEM_SIZE_HEIGHT}
  COMP_ELEM_SIZE_HEIGHT   = $00000080;
  {$EXTERNALSYM COMP_ELEM_POS_ZINDEX}
  COMP_ELEM_POS_ZINDEX    = $00000100;
  {$EXTERNALSYM COMP_ELEM_SOURCE}
  COMP_ELEM_SOURCE        = $00000200;
  {$EXTERNALSYM COMP_ELEM_FRIENDLYNAME}
  COMP_ELEM_FRIENDLYNAME  = $00000400;
  {$EXTERNALSYM COMP_ELEM_SUBSCRIBEDURL}
  COMP_ELEM_SUBSCRIBEDURL = $00000800;
  {$EXTERNALSYM COMP_ELEM_ORIGINAL_CSI}
  COMP_ELEM_ORIGINAL_CSI  = $00001000;
  {$EXTERNALSYM COMP_ELEM_RESTORED_CSI}
  COMP_ELEM_RESTORED_CSI  = $00002000;
  {$EXTERNALSYM COMP_ELEM_CURITEMSTATE}
  COMP_ELEM_CURITEMSTATE  = $00004000;

  {$EXTERNALSYM COMP_ELEM_ALL}
  COMP_ELEM_ALL           = COMP_ELEM_TYPE or COMP_ELEM_CHECKED or
                            COMP_ELEM_DIRTY or COMP_ELEM_NOSCROLL or
                            COMP_ELEM_POS_LEFT or COMP_ELEM_SIZE_WIDTH or
                            COMP_ELEM_SIZE_HEIGHT or COMP_ELEM_POS_ZINDEX or
                            COMP_ELEM_SOURCE or COMP_ELEM_FRIENDLYNAME or
                            COMP_ELEM_POS_TOP or COMP_ELEM_SUBSCRIBEDURL or
                            COMP_ELEM_ORIGINAL_CSI or COMP_ELEM_RESTORED_CSI or
                            COMP_ELEM_CURITEMSTATE;

////////////////////////////////////////////
// Flags for IActiveDesktop::AddDesktopItemWithUI()
  {$EXTERNALSYM DTI_ADDUI_DEFAULT}
  DTI_ADDUI_DEFAULT       = $00000000;
  {$EXTERNALSYM DTI_ADDUI_DISPSUBWIZARD}
  DTI_ADDUI_DISPSUBWIZARD = $00000001;
  {$EXTERNALSYM DTI_ADDUI_POSITIONITEM}
  DTI_ADDUI_POSITIONITEM  = $00000002;

type
  {$EXTERNALSYM tagDTI_ADTIWUI}
  tagDTI_ADTIWUI = DWORD;
  {$EXTERNALSYM DTI_ADTIWUI}
  DTI_ADTIWUI = tagDTI_ADTIWUI;

const
////////////////////////////////////////////
// Flags for IActiveDesktop::AddUrl()
  {$EXTERNALSYM ADDURL_SILENT}
  ADDURL_SILENT           = $0001;

////////////////////////////////////////////
// Default positions for ADI
  {$EXTERNALSYM COMPONENT_DEFAULT_LEFT}
  COMPONENT_DEFAULT_LEFT  = $FFFF;
  {$EXTERNALSYM COMPONENT_DEFAULT_TOP}
  COMPONENT_DEFAULT_TOP   = $FFFF;

//
//  Interface for manipulating the Active Desktop.
//
type
  {$EXTERNALSYM IActiveDesktop}
  IActiveDesktop = interface(IUnknown)
  ['{F490EB00-1240-11D1-9888-006097DEACF9}']
    function ApplyChanges(dwFlags: DWORD): HResult; stdcall;
    function GetWallpaper(pwszWallpaper: PWideChar; cchWallpaper: UINT;
      dwReserved: DWORD): HResult; stdcall;
    function SetWallpaper(pwszWallpaper: PWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function GetWallpaperOptions(out pwpo: TWallPaperOpt;
      dwReserved: DWORD): HResult; stdcall;
    function SetWallpaperOptions(const pwpo: TWallPaperOpt;
      dwReserved: DWORD): HResult; stdcall;
    function GetPattern(pwszPattern: PWideChar; cchPattern: UINT;
      dwReserved: DWORD): HResult; stdcall;
    function SetPattern(pwszPattern: PWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemOptions(out pco: TComponentsOpt;
      dwReserved: DWORD): HResult; stdcall;
    function SetDesktopItemOptions(const pco: TComponentsOpt;
      dwReserved: DWORD): HResult; stdcall;
    function AddDesktopItem(var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function AddDesktopItemWithUI(hwnd: HWND; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function ModifyDesktopItem(var pcomp: COMPONENT;
      dwFlags: DWORD): HResult; stdcall;
    function RemoveDesktopItem(var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemCount(out lpiCount: Integer;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItem(nComponent: Integer; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemByID(dwID: ULONG; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GenerateDesktopItemHtml(pwszFileName: PWideChar;
     var  pcomp: COMPONENT; dwReserved: DWORD): HResult; stdcall;
    function AddUrl(hwnd: HWND; pszSource: PWideChar; var pcomp: COMPONENT;
      dwFlags: DWORD): HResult; stdcall;
    function GetDesktopItemBySource(pwszSource: PWideChar;
      var pcomp: COMPONENT; dwReserved: DWORD): HResult; stdcall;
  end;

const
// Flags for SetSafeMode
  {$EXTERNALSYM SSM_CLEAR}
  SSM_CLEAR   = $0000;
  {$EXTERNALSYM SSM_SET}
  SSM_SET     = $0001;
  {$EXTERNALSYM SSM_REFRESH}
  SSM_REFRESH = $0002;
  {$EXTERNALSYM SSM_UPDATE}
  SSM_UPDATE  = $0004;

// Flags for Set/GetScheme
  {$EXTERNALSYM SCHEME_DISPLAY}
  SCHEME_DISPLAY          = $0001;
  {$EXTERNALSYM SCHEME_EDIT}
  SCHEME_EDIT             = $0002;
  {$EXTERNALSYM SCHEME_LOCAL}
  SCHEME_LOCAL            = $0004;
  {$EXTERNALSYM SCHEME_GLOBAL}
  SCHEME_GLOBAL           = $0008;
  {$EXTERNALSYM SCHEME_REFRESH}
  SCHEME_REFRESH          = $0010;
  {$EXTERNALSYM SCHEME_UPDATE}
  SCHEME_UPDATE           = $0020;
  {$EXTERNALSYM SCHEME_DONOTUSE}
  SCHEME_DONOTUSE         = $0040; // used to be SCHEME_ENUMERATE; no longer supported
  {$EXTERNALSYM SCHEME_CREATE}
  SCHEME_CREATE           = $0080;

type
  {$EXTERNALSYM IActiveDesktopP}
  IActiveDesktopP = interface(IUnknown )
  ['{52502EE0-EC80-11D0-89AB-00C04FC2972D}']
    function SetSafeMode(dwFlags: DWORD): HResult; stdcall;
    function EnsureUpdateHTML: HResult; stdcall;
    function SetScheme(pwszSchemeName: PWideChar;
      dwFlags: DWORD): HResult; stdcall;
    function GetScheme(pwszSchemeName: PWideChar; out lpdwcchBuffer: DWORD;
      dwFlags: DWORD): HResult; stdcall;
  end;

const
//Flags for GetObjectFlags
  {$EXTERNALSYM GADOF_DIRTY}
  GADOF_DIRTY  = $00000001;

type
  {$EXTERNALSYM IADesktopP2}
  IADesktopP2 = interface(IUnknown )
  ['{B22754E2-4574-11D1-9888-006097DEACF9}']
    function ReReadWallpaper: HResult; stdcall;
    function GetADObjectFlags(out lpdwFlags: DWORD; dwMask: DWORD): HResult; stdcall;
    function UpdateAllDesktopSubscriptions: HResult; stdcall;
    function MakeDynamicChanges(pOleObj: IOleObject): HResult; stdcall;
  end;

const
  {$EXTERNALSYM MAX_COLUMN_NAME_LEN}
  MAX_COLUMN_NAME_LEN = 80;
  {$EXTERNALSYM MAX_COLUMN_DESC_LEN}
  MAX_COLUMN_DESC_LEN = 128;

type
  PSHColumnInfo = ^TSHColumnInfo;
  {$EXTERNALSYM SHCOLUMNINFO}
  SHCOLUMNINFO = packed record
    scid: TSHColumnID;          // OUT the unique identifier of this column
    vt: TVARTYPE;                // OUT the native type of the data returned
    fmt: DWORD;                 // OUT this listview format (LVCFMT_LEFT, usually)
    cChars: UINT;               // OUT the default width of the column, in characters
    csFlags: DWORD;             // OUT SHCOLSTATE flags
    wszTitle: array[0..MAX_COLUMN_NAME_LEN - 1] of WideChar;        // OUT the title of the column
    wszDescription: array[0..MAX_COLUMN_DESC_LEN - 1] of WideChar;  // OUT full description of this column
  end;
  TSHColumnInfo = SHCOLUMNINFO;

  PSHColumnInit = ^TSHColumnInit;
  {$EXTERNALSYM SHCOLUMNINIT}
  SHCOLUMNINIT = record
    dwFlags: ULONG;              // initialization flags
    dwReserved: ULONG;           // reserved for future use.
    wszFolder: array[0..MAX_PATH - 1] of WideChar;  // fully qualified folder path (or empty if multiple folders)
  end;
  TSHColumnInit = SHCOLUMNINIT;

const
  {$EXTERNALSYM SHCDF_UPDATEITEM}
  SHCDF_UPDATEITEM = $00000001;  // this flag is a hint that the file has changed since the last call to GetItemData

type
  PSHColumnData = ^TSHColumnData;
  {$EXTERNALSYM SHCOLUMNDATA}
  SHCOLUMNDATA = record
    dwFlags: ULONG;              // combination of SHCDF_ flags.
    dwFileAttributes: DWORD;     // file attributes.
    dwReserved: ULONG;           // reserved for future use.
    pwszExt: PWideChar;          // address of file name extension
    wszFile: array[0..MAX_PATH - 1] of WideChar;   // Absolute path of file.
  end;
  TSHColumnData = SHCOLUMNDATA;

type
// Note: these objects must be threadsafe!  GetItemData _will_ be called
// simultaneously from multiple threads.
  {$EXTERNALSYM IColumnProvider}
  IColumnProvider = interface(IUnknown)
  ['{E8025004-1C42-11D2-BE2C-00A0C9A83DA1}']
    function Initialize(const psci: TSHColumnInit): HResult; stdcall;
    function GetColumnInfo(dwIndex: DWORD;
      out psci: TSHColumnInfo): HResult; stdcall;
    function GetItemData(const pscid: TSHColumnID; const pscd: TSHColumnData;
      out pvarData: OleVariant): HResult; stdcall;
  end;

///////////////////////////////////////////////////////
//
// Drag and Drop helper
//
// Purpose: To expose the Shell drag images
//
// This interface is implemented in the shell by CLSID_DragDropHelper.
//
// To use:
//   If you are the source of a drag (i.e. in response to LV_DRAGBEGIN or
//    equivelent begin drag message) call
//    IDragSourceHelper::InitializeFromWindow
//              (<hwnd of window supporting DI_GETDRAGIMAGE>,
//               <pointer to TPoint indicating offset to the mouse from
//                  the upper left corner of the image>,
//               <pointer to data object>)
//
//      NOTE: The Data object must support IDataObject::SetData with multiple
//            data types and GetData must implement data type cloning
//            (Including HGLOBAL), not just aliasing.
//
//   If you wish to have an image while over your application add the
//    IDragImages::Dr* calls to your IDropTarget implementation. For Example:
//
//    STDMETHODIMP CUserDropTarget::DragEnter(IDataObject* pDataObject,
//                                            DWORD grfKeyState,
//                                            TPointL pt, DWORD* pdwEffect)
//    {
//          // Process your DragEnter
//          // Call IDragImages::DragEnter last.
//          _pDropTargetHelper->DragEnter(_hwndDragOver, pDataObject,
//                                        (TPoint*)&pt, *pdwEffect);
//          return hres;
//    }
//
//
//   If you wish to be able to source a drag image from a custom control,
//     implement a handler for the RegisterWindowMessage(DI_GETDRAGIMAGE).
//     The LPARAM is a pointer to an SHDRAGIMAGE structure.
//
//      sizeDragImage  -   Calculate the length and width required to render
//                          the images.
//      ptOffset       -   Calculate the offset from the upper left corner to
//                          the mouse cursor within the image
//      hbmpDragImage  -   CreateBitmap(sizeDragImage.cx, sizeDragImage.cy,
//                           GetDeviceCaps(hdcScreen, PLANES),
//                           GetDeviceCaps(hdcScreen, BITSPIXEL),
//                           NULL);
//
//   Drag Images will only be displayed on Windows NT 5.0 or later.
//
//
//   Note about IDropTargetHelper::Show - This method is provided for
//     showing/hiding the Drag image in low color depth video modes. When
//     painting to a window that is currently being dragged over (i.e. For
//     indicating a selection) you need to hide the drag image by calling this
//     method passing FALSE. After the window is done painting, Show the image
//     again by passing TRUE.

  PSHDragImage = ^TSHDragImage;
  {$EXTERNALSYM SHDRAGIMAGE}
  SHDRAGIMAGE = record
    sizeDragImage: TSize;           // OUT - The length and width of the
                                    //        rendered image
    ptOffset: TPoint;               // OUT - The offset from the mouse cursor to
                                    //        the upper left corner of the image
    hbmpDragImage: HBITMAP;         // OUT - The bitmap containing the rendered
                                    //        drag images
    crColorKey: TColorRef;          // OUT - The ColorRef that has been blitted
                                    //        to the background of the images
  end;
  TSHDragImage = SHDRAGIMAGE;

const
// This is sent to a window to get the rendered images to a bitmap
// Call RegisterWindowMessage to get the ID
  {$EXTERNALSYM DI_GETDRAGIMAGE}
  DI_GETDRAGIMAGE = 'ShellGetDragImage';

type
  {$EXTERNALSYM IDropTargetHelper}
  IDropTargetHelper = interface(IUnknown )
  ['{4657278b-411b-11d2-839a-00c04fd918d0}']
    function DragEnter(hwndTarget: HWND; pDataObject: IDataObject;
      var ppt: TPoint; dwEffect: DWORD): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function DragOver(var ppt: TPoint; dwEffect: DWORD): HResult; stdcall;
    function Drop(pDataObject: IDataObject; var ppt: TPoint;
      dwEffect: DWORD): HResult; stdcall;
    function Show(fShow: BOOL): HResult; stdcall;
  end;

  {$EXTERNALSYM IDragSourceHelper}
  IDragSourceHelper = interface(IUnknown )
  ['{de5bf786-477a-11d2-839d-00c04fd918d0}']
    function InitializeFromBitmap(var pshdi: TSHDragImage;
      pDataObject: IDataObject): HResult; stdcall;
    function InitializeFromWindow(hwnd: HWND; var ppt: TPoint;
      pDataObject: IDataObject): HResult; stdcall;
  end;

//==========================================================================
// Clipboard format which may be supported by IDataObject from system
// defined shell folders (such as directories, network, ...).
//==========================================================================
const
  {$EXTERNALSYM CFSTR_SHELLIDLIST}
  CFSTR_SHELLIDLIST                   = 'Shell IDList Array';      // CF_IDLIST
  {$EXTERNALSYM CFSTR_SHELLIDLISTOFFSET}
  CFSTR_SHELLIDLISTOFFSET             = 'Shell Object Offsets';    // CF_OBJECTPOSITIONS
  {$EXTERNALSYM CFSTR_NETRESOURCES}
  CFSTR_NETRESOURCES                  = 'Net Resource';            // CF_NETRESOURCE
  {$EXTERNALSYM CFSTR_FILEDESCRIPTORA}
  CFSTR_FILEDESCRIPTORA               = 'FileGroupDescriptor';     // CF_FILEGROUPDESCRIPTORA
  {$EXTERNALSYM CFSTR_FILEDESCRIPTORW}
  CFSTR_FILEDESCRIPTORW               = 'FileGroupDescriptorW';    // CF_FILEGROUPDESCRIPTORW
  {$EXTERNALSYM CFSTR_FILECONTENTS}
  CFSTR_FILECONTENTS                  = 'FileContents';            // CF_FILECONTENTS
  {$EXTERNALSYM CFSTR_FILENAMEA}
  CFSTR_FILENAMEA                     = 'FileName';                // CF_FILENAMEA
  {$EXTERNALSYM CFSTR_FILENAMEW}
  CFSTR_FILENAMEW                     = 'FileNameW';               // CF_FILENAMEW
  {$EXTERNALSYM CFSTR_PRINTERGROUP}
  CFSTR_PRINTERGROUP                  = 'PrinterFriendlyName';     // CF_PRINTERS
  {$EXTERNALSYM CFSTR_FILENAMEMAPA}
  CFSTR_FILENAMEMAPA                  = 'FileNameMap';             // CF_FILENAMEMAPA
  {$EXTERNALSYM CFSTR_FILENAMEMAPW}
  CFSTR_FILENAMEMAPW                  = 'FileNameMapW';            // CF_FILENAMEMAPW
  {$EXTERNALSYM CFSTR_SHELLURL}
  CFSTR_SHELLURL                      = 'UniformResourceLocator';
  {$EXTERNALSYM CFSTR_INETURLA}
  CFSTR_INETURLA                      = CFSTR_SHELLURL;
  {$EXTERNALSYM CFSTR_INETURLW}
  CFSTR_INETURLW                      = 'UniformResourceLocatorW';
  {$EXTERNALSYM CFSTR_PREFERREDDROPEFFECT}
  CFSTR_PREFERREDDROPEFFECT           = 'Preferred DropEffect';
  {$EXTERNALSYM CFSTR_PERFORMEDDROPEFFECT}
  CFSTR_PERFORMEDDROPEFFECT           = 'Performed DropEffect';
  {$EXTERNALSYM CFSTR_PASTESUCCEEDED}
  CFSTR_PASTESUCCEEDED                = 'Paste Succeeded';
  {$EXTERNALSYM CFSTR_INDRAGLOOP}
  CFSTR_INDRAGLOOP                    = 'InShellDragLoop';
  {$EXTERNALSYM CFSTR_DRAGCONTEXT}
  CFSTR_DRAGCONTEXT                   = 'DragContext';
  {$EXTERNALSYM CFSTR_MOUNTEDVOLUME}
  CFSTR_MOUNTEDVOLUME                 = 'MountedVolume';
  {$EXTERNALSYM CFSTR_PERSISTEDDATAOBJECT}
  CFSTR_PERSISTEDDATAOBJECT           = 'PersistedDataObject';
  {$EXTERNALSYM CFSTR_TARGETCLSID}
  CFSTR_TARGETCLSID                   = 'TargetCLSID';                      // HGLOBAL with a CLSID of the drop target
  {$EXTERNALSYM CFSTR_LOGICALPERFORMEDDROPEFFECT}
  CFSTR_LOGICALPERFORMEDDROPEFFECT    = 'Logical Performed DropEffect';
  {$EXTERNALSYM CFSTR_AUTOPLAY_SHELLIDLISTS}
  CFSTR_AUTOPLAY_SHELLIDLISTS         = 'Autoplay Enumerated IDList Array'; //  (HGLOBAL with LPIDA)
  {$EXTERNALSYM CFSTR_UNTRUSTEDDRAGDROP}
  CFSTR_UNTRUSTEDDRAGDROP             = 'UntrustedDragDrop'; //  DWORD

  {$EXTERNALSYM CFSTR_FILEDESCRIPTOR}
  CFSTR_FILEDESCRIPTOR                = CFSTR_FILEDESCRIPTORA;
  {$EXTERNALSYM CFSTR_FILENAME}
  CFSTR_FILENAME                      = CFSTR_FILENAMEA;
  {$EXTERNALSYM CFSTR_FILENAMEMAP}
  CFSTR_FILENAMEMAP                   = CFSTR_FILENAMEMAPA;
  {$EXTERNALSYM CFSTR_INETURL}
  CFSTR_INETURL                       = CFSTR_INETURLA;

  {$EXTERNALSYM DVASPECT_SHORTNAME}
  DVASPECT_SHORTNAME      = 2; // use for CF_HDROP to get short name version of file paths
  {$EXTERNALSYM DVASPECT_COPY}
  DVASPECT_COPY           = 3; // use to indicate format is a "Copy" of the data (FILECONTENTS, FILEDESCRIPTOR, etc)
  {$EXTERNALSYM DVASPECT_LINK}
  DVASPECT_LINK           = 4; // use to indicate format is a "Shortcut" to the data (FILECONTENTS, FILEDESCRIPTOR, etc)

type
//
// format of CF_NETRESOURCE
//
  PNResArray = ^TNResArray;
  {$EXTERNALSYM _NRESARRAY}
  _NRESARRAY = record     // anr
    cItems: UINT;
    nr: array[0..0] of TNetResource;
  end;
  {$EXTERNALSYM NRESARRAY}
  NRESARRAY = _NRESARRAY;
  TNResArray = _NRESARRAY;

//
// format of CF_IDLIST
//
  PCIDA = ^TCIDA;
  {$EXTERNALSYM _IDA}
  _IDA = record
    cidl: UINT;          // number of relative IDList
    aoffset: array[0..0] of UINT;    // [0]: folder IDList, [1]-[cidl]: item IDList
  end;
  {$EXTERNALSYM CIDA}
  CIDA = _IDA;
  TCIDA = _IDA;

//
// FILEDESCRIPTOR.dwFlags field indicate which fields are to be used
//
  {$EXTERNALSYM FD_FLAGS}
  FD_FLAGS = DWORD;
  TFDFlags = DWORD;

const
  {$EXTERNALSYM FD_CLSID}
  FD_CLSID      = $0001;
  {$EXTERNALSYM FD_SIZEPOINT}
  FD_SIZEPOINT  = $0002;
  {$EXTERNALSYM FD_ATTRIBUTES}
  FD_ATTRIBUTES = $0004;
  {$EXTERNALSYM FD_CREATETIME}
  FD_CREATETIME = $0008;
  {$EXTERNALSYM FD_ACCESSTIME}
  FD_ACCESSTIME = $0010;
  {$EXTERNALSYM FD_WRITESTIME}
  FD_WRITESTIME = $0020;
  {$EXTERNALSYM FD_FILESIZE}
  FD_FILESIZE   = $0040;
  {$EXTERNALSYM FD_PROGRESSUI}
  FD_PROGRESSUI = $4000;   // Show Progress UI w/Drag and Drop
  {$EXTERNALSYM FD_LINKUI}
  FD_LINKUI     = $8000;   // 'link' UI is prefered

type
  PFileDescriptorA = ^TFileDescriptorA;
  {$EXTERNALSYM _FILEDESCRIPTORA}
  _FILEDESCRIPTORA = record // fod
    dwFlags: DWORD;
    clsid: TCLSID;
    sizel: TSize;
    pointl: TPointL;
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    cFileName: array[0..MAX_PATH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM FILEDESCRIPTORA}
  FILEDESCRIPTORA = _FILEDESCRIPTORA;
  TFileDescriptorA = _FILEDESCRIPTORA;

  PFileDescriptorW = ^TFileDescriptorW;
  {$EXTERNALSYM _FILEDESCRIPTORW}
  _FILEDESCRIPTORW = record // fod
    dwFlags: DWORD;
    clsid: TCLSID;
    sizel: TSize;
    pointl: TPointL;
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    cFileName: array[0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM FILEDESCRIPTORW}
  FILEDESCRIPTORW = _FILEDESCRIPTORW;
  TFileDescriptorW = _FILEDESCRIPTORW;

  PFileDescriptor = PFileDescriptorA;
  {$EXTERNALSYM FILEDESCRIPTOR}
  FILEDESCRIPTOR = FILEDESCRIPTORA;
  TFileDescriptor = TFileDescriptorA;

//
// format of CF_FILEGROUPDESCRIPTOR
//
  PFileGroupDescriptorA = ^TFileGroupDescriptorA;
  {$EXTERNALSYM _FILEGROUPDESCRIPTORA}
  _FILEGROUPDESCRIPTORA = record // fgd
    cItems: UINT;
    fgd: array[0..0] of TFileDescriptorA;
  end;
  {$EXTERNALSYM FILEGROUPDESCRIPTORA}
  FILEGROUPDESCRIPTORA = _FILEGROUPDESCRIPTORA;
  TFileGroupDescriptorA = _FILEGROUPDESCRIPTORA;

  PFileGroupDescriptorW = ^TFileGroupDescriptorW;
  {$EXTERNALSYM _FILEGROUPDESCRIPTORW}
  _FILEGROUPDESCRIPTORW = record // fgd
    cItems: UINT;
    fgd: array[0..0] of TFileDescriptorW;
  end;
  {$EXTERNALSYM FILEGROUPDESCRIPTORA}
  FILEGROUPDESCRIPTORW = _FILEGROUPDESCRIPTORW;
  TFileGroupDescriptorW = _FILEGROUPDESCRIPTORW;

  PFileGroupDescriptor = PFileGroupDescriptorA;
  {$EXTERNALSYM FILEGROUPDESCRIPTOR}
  FILEGROUPDESCRIPTOR = FILEGROUPDESCRIPTORA;
  TFileGroupDescriptor = TFileGroupDescriptorA;

//
// format of CF_HDROP and CF_PRINTERS, in the HDROP case the data that follows
// is a double null terminated list of file names, for printers they are printer
// friendly names
//
  PDropFiles = ^TDropFiles;
  {$EXTERNALSYM _DROPFILES}
  _DROPFILES = record
    pFiles: DWORD;      // offset of file list
    pt: TPoint;         // drop TPoint (client coords)
    fNC: BOOL;          // is it on NonClient area
                        // and pt is in screen coords
    fWide: BOOL;        // WIDE character switch
  end;
 {$EXTERNALSYM DROPFILES}
 DROPFILES = _DROPFILES;
 TDropFiles = _DROPFILES;


//====== File System Notification APIs ===============================
//
  PSHChangeNotifyEntry = ^TSHChangeNotifyEntry;
  {$EXTERNALSYM _SHChangeNotifyEntry}
  _SHChangeNotifyEntry = record
    pidl: PItemIDList;
    fRecursive: BOOL;
  end;
  {$EXTERNALSYM SHChangeNotifyEntry}
  SHChangeNotifyEntry = _SHChangeNotifyEntry;
  TSHChangeNotifyEntry = _SHChangeNotifyEntry;

//
//  File System Notification flags
//
const
  {$EXTERNALSYM SHCNE_RENAMEITEM}
  SHCNE_RENAMEITEM          = $00000001;
  {$EXTERNALSYM SHCNE_CREATE}
  SHCNE_CREATE              = $00000002;
  {$EXTERNALSYM SHCNE_DELETE}
  SHCNE_DELETE              = $00000004;
  {$EXTERNALSYM SHCNE_MKDIR}
  SHCNE_MKDIR               = $00000008;
  {$EXTERNALSYM SHCNE_RMDIR}
  SHCNE_RMDIR               = $00000010;
  {$EXTERNALSYM SHCNE_MEDIAINSERTED}
  SHCNE_MEDIAINSERTED       = $00000020;
  {$EXTERNALSYM SHCNE_MEDIAREMOVED}
  SHCNE_MEDIAREMOVED        = $00000040;
  {$EXTERNALSYM SHCNE_DRIVEREMOVED}
  SHCNE_DRIVEREMOVED        = $00000080;
  {$EXTERNALSYM SHCNE_DRIVEADD}
  SHCNE_DRIVEADD            = $00000100;
  {$EXTERNALSYM SHCNE_NETSHARE}
  SHCNE_NETSHARE            = $00000200;
  {$EXTERNALSYM SHCNE_NETUNSHARE}
  SHCNE_NETUNSHARE          = $00000400;
  {$EXTERNALSYM SHCNE_ATTRIBUTES}
  SHCNE_ATTRIBUTES          = $00000800;
  {$EXTERNALSYM SHCNE_UPDATEDIR}
  SHCNE_UPDATEDIR           = $00001000;
  {$EXTERNALSYM SHCNE_UPDATEITEM}
  SHCNE_UPDATEITEM          = $00002000;
  {$EXTERNALSYM SHCNE_SERVERDISCONNECT}
  SHCNE_SERVERDISCONNECT    = $00004000;
  {$EXTERNALSYM SHCNE_UPDATEIMAGE}
  SHCNE_UPDATEIMAGE         = $00008000;
  {$EXTERNALSYM SHCNE_DRIVEADDGUI}
  SHCNE_DRIVEADDGUI         = $00010000;
  {$EXTERNALSYM SHCNE_RENAMEFOLDER}
  SHCNE_RENAMEFOLDER        = $00020000;
  {$EXTERNALSYM SHCNE_FREESPACE}
  SHCNE_FREESPACE           = $00040000;

// SHCNE_EXTENDED_EVENT: the extended event is identified in dwItem1,
// packed in PItemIDList format (same as SHCNF_DWORD packing).
// Additional information can be passed in the dwItem2 parameter
// of SHChangeNotify (called "pidl2" below), which if present, must also
// be in PItemIDList format.
//
// Unlike the standard events, the extended events are ORDINALs, so we
// don't run out of bits.  Extended events follow the SHCNEE_* naming
// convention.
//
// The dwItem2 parameter varies according to the extended event.

  {$EXTERNALSYM SHCNE_EXTENDED_EVENT}
  SHCNE_EXTENDED_EVENT = $04000000;

  {$EXTERNALSYM SHCNE_ASSOCCHANGED}
  SHCNE_ASSOCCHANGED   = $08000000;

  {$EXTERNALSYM SHCNE_DISKEVENTS}
  SHCNE_DISKEVENTS     = $0002381F;
  {$EXTERNALSYM SHCNE_GLOBALEVENTS}
  SHCNE_GLOBALEVENTS   = $0C0581E0; // Events that dont match pidls first
  {$EXTERNALSYM SHCNE_ALLEVENTS}
  SHCNE_ALLEVENTS      = $7FFFFFFF;
  {$EXTERNALSYM SHCNE_INTERRUPT}
  SHCNE_INTERRUPT      = $80000000; // The presence of this flag indicates
                                    // that the event was generated by an
                                    // interrupt.  It is stripped out before
                                    // the clients of SHCNNotify_ see it.

// SHCNE_EXTENDED_EVENT extended events.  These events are ordinals.
// This is not a bitfield.
  {$EXTERNALSYM SHCNEE_ORDERCHANGED}
  SHCNEE_ORDERCHANGED  =         2;  // pidl2 is the changed folder
  {$EXTERNALSYM SHCNEE_MSI_CHANGE}
  SHCNEE_MSI_CHANGE    =         4;  // pidl2 is a SHChangeProductKeyAsIDList
  {$EXTERNALSYM SHCNEE_MSI_UNINSTALL}
  SHCNEE_MSI_UNINSTALL =         5;  // pidl2 is a SHChangeProductKeyAsIDList

// Flags
// uFlags & SHCNF_TYPE is an ID which indicates what dwItem1 and dwItem2 mean
  {$EXTERNALSYM SHCNF_IDLIST}
  SHCNF_IDLIST      = $0000;        // PItemIDList
  {$EXTERNALSYM SHCNF_PATHA}
  SHCNF_PATHA       = $0001;        // path name
  {$EXTERNALSYM SHCNF_PRINTERA}
  SHCNF_PRINTERA    = $0002;        // printer friendly name
  {$EXTERNALSYM SHCNF_DWORD}
  SHCNF_DWORD       = $0003;        // DWORD
  {$EXTERNALSYM SHCNF_PATHW}
  SHCNF_PATHW       = $0005;        // path name
  {$EXTERNALSYM SHCNF_PRINTERW}
  SHCNF_PRINTERW    = $0006;        // printer friendly name
  {$EXTERNALSYM SHCNF_TYPE}
  SHCNF_TYPE        = $00FF;
  {$EXTERNALSYM SHCNF_FLUSH}
  SHCNF_FLUSH       = $1000;
  {$EXTERNALSYM SHCNF_FLUSHNOWAIT}
  SHCNF_FLUSHNOWAIT = $2000;

  {$EXTERNALSYM SHCNF_PATH}
  SHCNF_PATH        = SHCNF_PATHA;
  {$EXTERNALSYM SHCNF_PRINTER}
  SHCNF_PRINTER     = SHCNF_PRINTERA;

//
//  APIs
//
{$EXTERNALSYM SHChangeNotify}
procedure SHChangeNotify(wEventId: Longint; uFlags: UINT; dwItem1, dwItem2: Pointer); stdcall;

//
// IShellChangeNotify
//
type
  {$EXTERNALSYM IShellChangeNotify}
  IShellChangeNotify = interface(IUnknown)
  ['{D82BE2B1-5764-11D0-A96E-00C04FD705A2}']
    function OnChange(lEvent: Longint; pidl1, pidl2: PItemIDList): HResult; stdcall;
  end;

//
// IQueryInfo
//
//-------------------------------------------------------------------------
//
// IQueryInfo interface
//
// [Methods]
//              ::GetInfoTip()
//-------------------------------------------------------------------------

  {$EXTERNALSYM IQueryInfo}
  IQueryInfo = interface(IUnknown)
  ['{00021500-0000-0000-C000-000000000046}']
    function GetInfoTip(dwFlags: DWORD; ppwszTip: PPWideChar): HResult; stdcall;
    function GetInfoFlags(out pdwFlags: DWORD): HResult; stdcall;
  end;

const
  {$EXTERNALSYM QITIPF_DEFAULT}
  QITIPF_DEFAULT          = $00000000;
  {$EXTERNALSYM QITIPF_USENAME}
  QITIPF_USENAME          = $00000001;
  {$EXTERNALSYM QITIPF_LINKNOTARGET}
  QITIPF_LINKNOTARGET     = $00000002;
  {$EXTERNALSYM QITIPF_LINKUSETARGET}
  QITIPF_LINKUSETARGET    = $00000004;
  {$EXTERNALSYM QITIPF_USESLOWTIP}
  QITIPF_USESLOWTIP       = $00000008;  // Flag says it's OK to take a long time generating tip

  {$EXTERNALSYM QIF_CACHED}
  QIF_CACHED              = $00000001;
  {$EXTERNALSYM QIF_DONTEXPANDFOLDER}
  QIF_DONTEXPANDFOLDER    = $00000002;


//
// SHAddToRecentDocs
//
  {$EXTERNALSYM SHARD_PIDL}
  SHARD_PIDL  = $00000001;
  {$EXTERNALSYM SHARD_PATHA}
  SHARD_PATHA = $00000002;
  {$EXTERNALSYM SHARD_PATHW}
  SHARD_PATHW = $00000003;

  {$EXTERNALSYM SHARD_PATH}
  SHARD_PATH  = SHARD_PATHA;

{$EXTERNALSYM SHAddToRecentDocs}
procedure SHAddToRecentDocs(uFlags: UINT; pv: Pointer); stdcall;

type
  PSHChangeDWORDAsIDList = ^TSHChangeDWORDAsIDList;
  {$EXTERNALSYM _SHChangeDWORDAsIDList}
  _SHChangeDWORDAsIDList = record
    cb: Word;
    dwItem1: DWORD;
    dwItem2: DWORD;
    cbZero: Word;
  end;
  {$EXTERNALSYM SHChangeDWORDAsIDList}
  SHChangeDWORDAsIDList = _SHChangeDWORDAsIDList;
  TSHChangeDWORDAsIDList = _SHChangeDWORDAsIDList;

  PSHChangeUpdateImageIDList = ^TSHChangeUpdateImageIDList;
  {$EXTERNALSYM _SHChangeUpdateImageIDList}
  _SHChangeUpdateImageIDList = record
    cb: Word;
    iIconIndex: Integer;
    iCurIndex: Integer;
    uFlags: UINT;
    dwProcessID: DWORD;
    szName: array[0..MAX_PATH - 1] of WideChar;
    cbZero: Word;
  end;
  {$EXTERNALSYM SHChangeUpdateImageIDList}
  SHChangeUpdateImageIDList = _SHChangeUpdateImageIDList;
  TSHChangeUpdateImageIDList = _SHChangeUpdateImageIDList;

{$EXTERNALSYM SHHandleUpdateImage}
function SHHandleUpdateImage(pidlExtra: PItemIDList): Integer; stdcall;

type
  PSHChangeProductKeyAsIDList = ^TSHChangeProductKeyAsIDList;
  {$EXTERNALSYM _SHChangeProductKeyAsIDList}
  _SHChangeProductKeyAsIDList = record
    cb: Word;
    wszProductKey: array[0..38] of WideChar;
    cbZero: Word;
  end;
  {$EXTERNALSYM SHChangeProductKeyAsIDList}
  SHChangeProductKeyAsIDList = _SHChangeProductKeyAsIDList;
  TSHChangeProductKeyAsIDList = _SHChangeProductKeyAsIDList;

{$EXTERNALSYM SHUpdateImageA}
procedure SHUpdateImageA(pszHashItem: PAnsiChar; iIndex: Integer;
  uFlags: UINT; iImageIndex: Integer); stdcall;
{$EXTERNALSYM SHUpdateImageW}
procedure SHUpdateImageW(pszHashItem: PWideChar; iIndex: Integer;
  uFlags: UINT; iImageIndex: Integer); stdcall;
{$EXTERNALSYM SHUpdateImage}
procedure SHUpdateImage(pszHashItem: PTSTR; iIndex: Integer; uFlags: UINT;
  iImageIndex: Integer); stdcall;

{$EXTERNALSYM SHChangeNotifyRegister}
function SHChangeNotifyRegister(hwnd: HWND; fSources: Integer;
  fEvents: Longint; wMsg: UINT; cEntries: Integer;
  var pshcne: TSHChangeNotifyEntry): ULONG; stdcall;
{$EXTERNALSYM SHChangeNotifyDeregister}
function SHChangeNotifyDeregister(ulID: LongWord): BOOL; stdcall;
{$EXTERNALSYM SHChangeNotification_Lock}
function SHChangeNotification_Lock(hChangeNotification: THandle;
  dwProcessId: DWORD; var pppidl: PPItemIDListArray;
  var plEvent: Longint): THandle; stdcall;
{$EXTERNALSYM SHChangeNotification_Unlock}
function SHChangeNotification_Unlock(hLock: THandle): BOOL; stdcall;
// The pidls that are given to the view via the ChangeNotifyEvents are simple Pidls,
// SHGetRealIDL() will convert them to true PIDLs.
{$EXTERNALSYM SHGetRealIDL}
function SHGetRealIDL(psf: IShellFolder; pidlSimple: PItemIDList; out ppidlReal: PItemIDList): HResult; stdcall;

{$EXTERNALSYM SHGetInstanceExplorer}
function SHGetInstanceExplorer(out ppunk: IUnknown): HResult; stdcall;

const
//
// SHGetDataFromIDListA/W
//
// SHGetDataFromIDList nFormat values TCHAR
  {$EXTERNALSYM SHGDFIL_FINDDATA}
  SHGDFIL_FINDDATA        = 1;
  {$EXTERNALSYM SHGDFIL_NETRESOURCE}
  SHGDFIL_NETRESOURCE     = 2;
  {$EXTERNALSYM SHGDFIL_DESCRIPTIONID}
  SHGDFIL_DESCRIPTIONID   = 3;

  {$EXTERNALSYM SHDID_ROOT_REGITEM}
  SHDID_ROOT_REGITEM          = 1;
  {$EXTERNALSYM SHDID_FS_FILE}
  SHDID_FS_FILE               = 2;
  {$EXTERNALSYM SHDID_FS_DIRECTORY}
  SHDID_FS_DIRECTORY          = 3;
  {$EXTERNALSYM SHDID_FS_OTHER}
  SHDID_FS_OTHER              = 4;
  {$EXTERNALSYM SHDID_COMPUTER_DRIVE35}
  SHDID_COMPUTER_DRIVE35      = 5;
  {$EXTERNALSYM SHDID_COMPUTER_DRIVE525}
  SHDID_COMPUTER_DRIVE525     = 6;
  {$EXTERNALSYM SHDID_COMPUTER_REMOVABLE}
  SHDID_COMPUTER_REMOVABLE    = 7;
  {$EXTERNALSYM SHDID_COMPUTER_FIXED}
  SHDID_COMPUTER_FIXED        = 8;
  {$EXTERNALSYM SHDID_COMPUTER_NETDRIVE}
  SHDID_COMPUTER_NETDRIVE     = 9;
  {$EXTERNALSYM SHDID_COMPUTER_CDROM}
  SHDID_COMPUTER_CDROM        = 10;
  {$EXTERNALSYM SHDID_COMPUTER_RAMDISK}
  SHDID_COMPUTER_RAMDISK      = 11;
  {$EXTERNALSYM SHDID_COMPUTER_OTHER}
  SHDID_COMPUTER_OTHER        = 12;
  {$EXTERNALSYM SHDID_NET_DOMAIN}
  SHDID_NET_DOMAIN            = 13;
  {$EXTERNALSYM SHDID_NET_SERVER}
  SHDID_NET_SERVER            = 14;
  {$EXTERNALSYM SHDID_NET_SHARE}
  SHDID_NET_SHARE             = 15;
  {$EXTERNALSYM SHDID_NET_RESTOFNET}
  SHDID_NET_RESTOFNET         = 16;
  {$EXTERNALSYM SHDID_NET_OTHER}
  SHDID_NET_OTHER             = 17;
  {$EXTERNALSYM SHDID_COMPUTER_IMAGING}
  SHDID_COMPUTER_IMAGING      = 18;
  {$EXTERNALSYM SHDID_COMPUTER_AUDIO}
  SHDID_COMPUTER_AUDIO        = 19;
  {$EXTERNALSYM SHDID_COMPUTER_SHAREDDOCS}
  SHDID_COMPUTER_SHAREDDOCS   = 20;

type
  PSHDescriptionID = ^TSHDescriptionID;
  {$EXTERNALSYM _SHDESCRIPTIONID}
  _SHDESCRIPTIONID = record
    dwDescriptionId: DWORD;
    clsid: TCLSID;
  end;
 {$EXTERNALSYM SHDESCRIPTIONID}
 SHDESCRIPTIONID = _SHDESCRIPTIONID;
 TSHDescriptionID = _SHDESCRIPTIONID;

// these delegate to IShellFolder2::GetItemData()

{$EXTERNALSYM SHGetDataFromIDListA}
function SHGetDataFromIDListA(psf: IShellFolder; pidl: PItemIDList;
  nFormat: Integer; pv: Pointer; cb: Integer): HResult; stdcall;
{$EXTERNALSYM SHGetDataFromIDListW}
function SHGetDataFromIDListW(psf: IShellFolder; pidl: PItemIDList;
  nFormat: Integer; pv: Pointer; cb: Integer): HResult; stdcall;
{$EXTERNALSYM SHGetDataFromIDList}
function SHGetDataFromIDList(psf: IShellFolder; pidl: PItemIDList;
  nFormat: Integer; pv: Pointer; cb: Integer): HResult; stdcall;

//===========================================================================

const
// PathResolve flags
  {$EXTERNALSYM PRF_VERIFYEXISTS}
  PRF_VERIFYEXISTS          = $0001;
  {$EXTERNALSYM PRF_TRYPROGRAMEXTENSIONS}
  PRF_TRYPROGRAMEXTENSIONS  = $0002 or PRF_VERIFYEXISTS;
  {$EXTERNALSYM PRF_FIRSTDIRDEF}
  PRF_FIRSTDIRDEF           = $0004;
  {$EXTERNALSYM PRF_DONTFINDLNK}
  PRF_DONTFINDLNK           = $0008;      // if PRF_TRYPROGRAMEXTENSIONS is specified

{$EXTERNALSYM RestartDialog}
function RestartDialog(hwnd: HWND; lpPrompt: PWideChar;
  dwReturn: DWORD): Integer; stdcall;
{$EXTERNALSYM RestartDialogEx}
function RestartDialogEx(hwnd: HWND; lpPrompt: PWideChar;
  dwReturn, dwReasonCode: DWORD): Integer; stdcall;

{$EXTERNALSYM SHCoCreateInstance}
function SHCoCreateInstance(pszCLSID: PWideChar; const pclsid: TCLSID; pUnkOuter: IUnknown; const iid: TIID; out ppv): HResult; stdcall;

// For CallCPLEntry16
//
type
  {$EXTERNALSYM FARPROC16}
  FARPROC16 = THandle;
  TFarProc16 = FARPROC16;

  TFormatEtcArray = array[0..65535] of TFormatEtc;
  PFormatEtcArray = ^TFormatEtcArray;

{$EXTERNALSYM CallCPLEntry16}
function CallCPLEntry16(hinst: THandle; lpfnEntry: TFarProc16; hwndCPL: HWND;
    msg: UINT; lParam1, lParam2: LPARAM): LRESULT stdcall;

{$EXTERNALSYM SHCreateStdEnumFmtEtc}
function SHCreateStdEnumFmtEtc(cfmt: UINT; afmt: PFormatEtcArray;
  out ppenumFormatEtc: IEnumFORMATETC): HResult; stdcall;
{$EXTERNALSYM SHDoDragDrop}
function SHDoDragDrop(hwnd: HWND; pdata: IDataObject; pdsrc: IDropSource;
  dwEffect: DWORD; out pdwEffect: DWORD): HResult; stdcall;

const
// stuff for doing auto scrolling
  {$EXTERNALSYM NUM_POINTS}
  NUM_POINTS = 3;

type
  PAutoScrollData = ^TAutoScrollData;
  {$EXTERNALSYM AUTO_SCROLL_DATA}
  AUTO_SCROLL_DATA = record   // asd
    iNextSample: Integer;
    dwLastScroll: DWORD;
    bFull: BOOL;
    pts: array[0..NUM_POINTS - 1] of TPoint;
    dwTimes: array[0..NUM_POINTS - 1] of DWORD;
  end;
  TAutoScrollData = AUTO_SCROLL_DATA;

{$EXTERNALSYM DAD_SetDragImage}
function DAD_SetDragImage(him: HIMAGELIST;
  var pptOffset: TPoint): BOOL; stdcall;
{$EXTERNALSYM DAD_DragEnterEx}
function DAD_DragEnterEx(hwndTarget: HWND; ptStart: TPoint): BOOL; stdcall;
{$EXTERNALSYM DAD_DragEnterEx2}
function DAD_DragEnterEx2(hwndTarget: HWND; ptStart: TPoint;
  pdtObject: IDataObject): BOOL; stdcall;
{$EXTERNALSYM DAD_ShowDragImage}
function DAD_ShowDragImage(fShow: BOOL): BOOL; stdcall;
{$EXTERNALSYM DAD_DragMove}
function DAD_DragMove(pt: TPoint): BOOL; stdcall;
{$EXTERNALSYM DAD_DragLeave}
function DAD_DragLeave: BOOL; stdcall;
{$EXTERNALSYM DAD_AutoScroll}
function DAD_AutoScroll(hwnd: HWND; var pad: TAutoScrollData;
  const pptNow: TPoint): BOOL; stdcall;

type
  TCabinetStateFlag = (fFullPathTitle, fSaveLocalView, fNotShell,
    fSimpleDefault, fDontShowDescBar, fNewWindowMode, fShowCompColor,
    fDontPrettyNames, fAdminsCreateCommonGroups);

  TCabinetStateFlags = set of TCabinetStateFlag;

type
  {$EXTERNALSYM CABINETSTATE}
  CABINETSTATE = record
    cLength: Word;
    nVersion: Word;
    Flags: Word;
{
    BOOL fFullPathTitle            : 1;
    BOOL fSaveLocalView            : 1;
    BOOL fNotShell                 : 1;
    BOOL fSimpleDefault            : 1;
    BOOL fDontShowDescBar          : 1;
    BOOL fNewWindowMode            : 1;
    BOOL fShowCompColor            : 1;  // NT: Show compressed volumes in a different colour
    BOOL fDontPrettyNames          : 1;  // NT: Do 8.3 name conversion, or not!
    BOOL fAdminsCreateCommonGroups : 1;  // NT: Administrators create comon groups
    UINT fUnusedFlags : 7;
}
    fMenuEnumFilter: UINT;
  end;

  PCabinetState = ^TCabinetState;
  TCabinetState = record
    cLength: Word;
    nVersion: Word;
    Flags: TCabinetStateFlags;
    fMenuEnumFilter: UINT;
  end;

const
  {$EXTERNALSYM CABINETSTATE_VERSION}
  CABINETSTATE_VERSION = 2;

// APIs for reading and writing the cabinet state.
{$EXTERNALSYM ReadCabinetState}
function ReadCabinetState(var lpState: TCabinetState;
  iSize: Integer): BOOL; stdcall;
{$EXTERNALSYM WriteCabinetState}
function WriteCabinetState(var lpState: TCabinetState): BOOL; stdcall;
{$EXTERNALSYM PathMakeUniqueName}
function PathMakeUniqueName(pszUniqueName: PWideChar; cchMax: UINT;
  pszTemplate, pszLongPlate, pszDir: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathQualify}
procedure PathQualify(psz: PWideChar); stdcall;
{$EXTERNALSYM PathIsExe}
function PathIsExe(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSlowA}
function PathIsSlowA(pszFile: PAnsiChar; dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlowW}
function PathIsSlowW(pszFile: PWideChar; dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlow}
function PathIsSlow(pszFile: PTSTR; dwAttr: DWORD): BOOL; stdcall;

const
//
//  Return codes from PathCleanupSpec.  Negative return values are
//  unrecoverable errors
//
  {$EXTERNALSYM PCS_FATAL}
  PCS_FATAL           = $80000000;
  {$EXTERNALSYM PCS_REPLACEDCHAR}
  PCS_REPLACEDCHAR    = $00000001;
  {$EXTERNALSYM PCS_REMOVEDCHAR}
  PCS_REMOVEDCHAR     = $00000002;
  {$EXTERNALSYM PCS_TRUNCATED}
  PCS_TRUNCATED       = $00000004;
  {$EXTERNALSYM PCS_PATHTOOLONG}
  PCS_PATHTOOLONG     = $00000008;  // Always combined with FATAL

type
  TPWideCharArray = array[0..65535] of PWideChar;
  PPWideCharArray = ^TPWideCharArray;

{$EXTERNALSYM PathCleanupSpec}
function PathCleanupSpec(pszDir, pszSpec: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathResolve}
function PathResolve(pszPath: PWideChar; dirs: PPWideCharArray;
  fFlags: UINT): Integer; stdcall;
{$EXTERNALSYM GetFileNameFromBrowse}
function GetFileNameFromBrowse(hwnd: HWND; pszFilePath: PWideChar;
  cbFilePath: UINT;
  pszWorkingDir, pszDefExt, pszFilters, pszTitle: PWideChar): BOOL; stdcall;
{$EXTERNALSYM DriveType}
function DriveType(iDrive: Integer): Integer; stdcall;
{$EXTERNALSYM RealDriveType}
function RealDriveType(iDrive: Integer; fOKToHitNet: BOOL): Integer; stdcall;
{$EXTERNALSYM IsNetDrive}
function IsNetDrive(iDrive: Integer): Integer; stdcall;

const
// Flags for Shell_MergeMenus
  {$EXTERNALSYM MM_ADDSEPARATOR}
  MM_ADDSEPARATOR    = $00000001;
  {$EXTERNALSYM MM_SUBMENUSHAVEIDS}
  MM_SUBMENUSHAVEIDS = $00000002;
  {$EXTERNALSYM MM_DONTREMOVESEPS}
  MM_DONTREMOVESEPS  = $00000004;

{$EXTERNALSYM Shell_MergeMenus}
function Shell_MergeMenus(hmDst, hmSrc: HMENU;
  uInsert, uIDAdjust, uIDAdjustMax: UINT; uFlags: ULONG): ULONG; stdcall;

(*
 * The SHObjectProperties API provides an easy way to invoke
 *   the Properties context menu command on shell objects.
 *
 *   PARAMETERS
 *
 *     hwnd    The window handle of the window which will own the dialog
 *     dwType       A SHOP_ value as defined below
 *     lpObject     Name of the object, see SHOP_ values below
 *     lpPage       The name of the property sheet page to open to or NULL.
 *
 *   RETURN
 *
 *     TRUE if the Properties command was invoked
 *)
{$EXTERNALSYM SHObjectProperties}
function SHObjectProperties(hwnd: HWND; dwType: DWORD;
  lpObject, lpPage: PWideChar): BOOL; stdcall;

const
  {$EXTERNALSYM SHOP_PRINTERNAME}
  SHOP_PRINTERNAME = $00000001;  // lpObject points to a printer friendly name
  {$EXTERNALSYM SHOP_FILEPATH}
  SHOP_FILEPATH    = $00000002;  // lpObject points to a fully qualified path+file name
  {$EXTERNALSYM SHOP_VOLUMEGUID}
  SHOP_VOLUMEGUID  = $00000004;  // lpObject points to a Volume GUID

(*
 * The SHFormatDrive API provides access to the Shell
 *   format dialog. This allows apps which want to format disks
 *   to bring up the same dialog that the Shell does to do it.
 *
 *   This dialog is not sub-classable. You cannot put custom
 *   controls in it. If you want this ability, you will have
 *   to write your own front end for the DMaint_FormatDrive
 *   engine.
 *
 *   NOTE that the user can format as many diskettes in the specified
 *   drive, or as many times, as he/she wishes to. There is no way to
 *   force any specififc number of disks to format. If you want this
 *   ability, you will have to write your own front end for the
 *   DMaint_FormatDrive engine.
 *
 *   NOTE also that the format will not start till the user pushes the
 *   start button in the dialog. There is no way to do auto start. If
 *   you want this ability, you will have to write your own front end
 *   for the DMaint_FormatDrive engine.
 *
 *   PARAMETERS
 *
 *     hwnd    = The window handle of the window which will own the dialog
 *               NOTE that unlike SHCheckDrive, hwnd == NULL does not cause
 *               this dialog to come up as a "top level application" window.
 *               This parameter should always be non-null, this dialog is
 *               only designed to be the child of another window, not a
 *               stand-alone application.
 *     drive   = The 0 based (A: == 0) drive number of the drive to format
 *     fmtID   = The ID of the physical format to format the disk with
 *               NOTE: The special value SHFMT_ID_DEFAULT means "use the
 *                     default format specified by the DMaint_FormatDrive
 *                     engine". If you want to FORCE a particular format
 *                     ID "up front" you will have to call
 *                     DMaint_GetFormatOptions yourself before calling
 *                     this to obtain the valid list of phys format IDs
 *                     (contents of the PhysFmtIDList array in the
 *                     FMTINFOSTRUCT).
 *     options = There is currently only two option bits defined
 *
 *                SHFMT_OPT_FULL
 *                SHFMT_OPT_SYSONLY
 *
 *               The normal defualt in the Shell format dialog is
 *               "Quick Format", setting this option bit indicates that
 *               the caller wants to start with FULL format selected
 *               (this is useful for folks detecting "unformatted" disks
 *               and wanting to bring up the format dialog).
 *
 *               The SHFMT_OPT_SYSONLY initializes the dialog to
 *               default to just sys the disk.
 *
 *               All other bits are reserved for future expansion and
 *               must be 0.
 *
 *               Please note that this is a bit field and not a value
 *               and treat it accordingly.
 *
 *   RETURN
 *      The return is either one of the SHFMT_* values, or if the
 *      returned DWORD value is not == to one of these values, then
 *      the return is the physical format ID of the last succesful
 *      format. The LOWORD of this value can be passed on subsequent
 *      calls as the fmtID parameter to "format the same type you did
 *      last time".
 *
 *)
{$EXTERNALSYM SHFormatDrive}
function SHFormatDrive(hwnd: HWND; drive, fmtID, options: UINT): DWORD; stdcall;

const
//
// Special value of fmtID which means "use the default format"
//
  {$EXTERNALSYM SHFMT_ID_DEFAULT}
  SHFMT_ID_DEFAULT   = $FFFF;

//
// Option bits for options parameter
//
  {$EXTERNALSYM SHFMT_OPT_FULL}
  SHFMT_OPT_FULL     = $0001;
  {$EXTERNALSYM SHFMT_OPT_SYSONLY}
  SHFMT_OPT_SYSONLY  = $0002;

//
// Special return values. PLEASE NOTE that these are DWORD values.
//
  {$EXTERNALSYM SHFMT_ERROR}
  SHFMT_ERROR     = $FFFFFFFF;     // Error on last format, drive may be formatable
  {$EXTERNALSYM SHFMT_CANCEL}
  SHFMT_CANCEL    = $FFFFFFFE;     // Last format was canceled
  {$EXTERNALSYM SHFMT_NOFORMAT}
  SHFMT_NOFORMAT  = $FFFFFFFD;     // Drive is not formatable

type
  {$EXTERNALSYM HPSXA}
  HPSXA = THandle;

{$EXTERNALSYM SHCreatePropSheetExtArray}
function SHCreatePropSheetExtArray(hKey: HKEY; pszSubKey: PWideChar;
  max_iface: UINT): HPSXA; stdcall;
{$EXTERNALSYM SHDestroyPropSheetExtArray}
procedure SHDestroyPropSheetExtArray(hpsxa: HPSXA); stdcall;
{$EXTERNALSYM SHAddFromPropSheetExtArray}
function SHAddFromPropSheetExtArray(hpsxa: HPSXA;
  lpfnAddPage: TFnAddPropSheetPage; lParam: LPARAM): UINT; stdcall;
{$EXTERNALSYM SHReplaceFromPropSheetExtArray}
function SHReplaceFromPropSheetExtArray(hpsxa: HPSXA; uPageID: UINT;
  lpfnReplaceWith: TFnAddPropSheetPage; lParam: LPARAM): UINT; stdcall;
{$EXTERNALSYM ILClone}
function ILClone(pidl: PItemIDList): PItemIDList; stdcall;
{$EXTERNALSYM ILGetNext}
function ILGetNext(pidl: PItemIDList): PItemIDList; stdcall;
{$EXTERNALSYM ILGetSize}
function ILGetSize(pidl: PItemIDList): UINT; stdcall;
{$EXTERNALSYM ILFindLastID}
function ILFindLastID(pidl: PItemIDList): PItemIDList; stdcall;
{$EXTERNALSYM ILRemoveLastID}
function ILRemoveLastID(pidl: PItemIDList): BOOL; stdcall;
{$EXTERNALSYM ILAppendID}
function ILAppendID(pidl: PItemIDList; pmkid: PSHItemID;
  fAppend: BOOL): PItemIDList; stdcall;
{$EXTERNALSYM ILFree}
procedure ILFree(pidl: PItemIDList); stdcall;
{$EXTERNALSYM ILCloneFirst}
function ILCloneFirst(pidl: PItemIDList): PItemIDList; stdcall;
{$EXTERNALSYM ILIsEqual}
function ILIsEqual(pidl1, pidl2: PItemIDList): BOOL; stdcall;
{$EXTERNALSYM ILIsParent}
function ILIsParent(pidl1, pidl2: PItemIDList; fImmediate: BOOL): BOOL; stdcall;
{$EXTERNALSYM ILFindChild}
function ILFindChild(pidlParent, pidlChild: PItemIDList): PItemIDList; stdcall;
{$EXTERNALSYM ILCombine}
function ILCombine(pidl1, pidl2: PItemIDList): PItemIDList; stdcall;


  {$EXTERNALSYM ILLoadFromStream}
function ILLoadFromStream(pstm: IStream;
    out pidl: PItemIDList): HResult stdcall;

{$EXTERNALSYM ILSaveToStream}
function ILSaveToStream(pstm: IStream; pidl: PItemIDList): HResult; stdcall;

{$EXTERNALSYM ILCreateFromPathA}
function ILCreateFromPathA(pszPath: PAnsiChar): PItemIDList; stdcall;
{$EXTERNALSYM ILCreateFromPathW}
function ILCreateFromPathW(pszPath: PWideChar): PItemIDList; stdcall;
{$EXTERNALSYM ILCreateFromPath}
function ILCreateFromPath(pszPath: PTSTR): PItemIDList; stdcall;
{$EXTERNALSYM SHILCreateFromPath}
function SHILCreateFromPath(szPath: PWideChar; out ppidl: PItemIDList;
  var rgfInOut: DWORD): HResult; stdcall;

type
  {$EXTERNALSYM IDefViewFrame}
  IDefViewFrame = interface(IUnknown)
  ['{710EB7A0-45ED-11D0-924A-0020AFC7AC4D}']
    function GetWindowLV(out phwnd: HWND): HResult; stdcall;
    function ReleaseWindowLV: HResult; stdcall;
    function GetShellFolder(out ppsf: IShellFolder): HResult; stdcall;
  end;

//===========================================================================
// Shell restrictions. (Parameter for SHRestricted)
type
  {$EXTERNALSYM RESTRICTIONS}
  RESTRICTIONS = DWORD;
  TRestrictions = DWORD;

const
  {$EXTERNALSYM REST_NONE}
  REST_NONE                        = $00000000;
  {$EXTERNALSYM REST_NORUN}
  REST_NORUN                       = $00000001;
  {$EXTERNALSYM REST_NOCLOSE}
  REST_NOCLOSE                     = $00000002;
  {$EXTERNALSYM REST_NOSAVESET}
  REST_NOSAVESET                   = $00000004;
  {$EXTERNALSYM REST_NOFILEMENU}
  REST_NOFILEMENU                  = $00000008;
  {$EXTERNALSYM REST_NOSETFOLDERS}
  REST_NOSETFOLDERS                = $00000010;
  {$EXTERNALSYM REST_NOSETTASKBAR}
  REST_NOSETTASKBAR                = $00000020;
  {$EXTERNALSYM REST_NODESKTOP}
  REST_NODESKTOP                   = $00000040;
  {$EXTERNALSYM REST_NOFIND}
  REST_NOFIND                      = $00000080;
  {$EXTERNALSYM REST_NODRIVES}
  REST_NODRIVES                    = $00000100;
  {$EXTERNALSYM REST_NODRIVEAUTORUN}
  REST_NODRIVEAUTORUN              = $00000200;
  {$EXTERNALSYM REST_NODRIVETYPEAUTORUN}
  REST_NODRIVETYPEAUTORUN          = $00000400;
  {$EXTERNALSYM REST_NONETHOOD}
  REST_NONETHOOD                   = $00000800;
  {$EXTERNALSYM REST_STARTBANNER}
  REST_STARTBANNER                 = $00001000;
  {$EXTERNALSYM REST_RESTRICTRUN}
  REST_RESTRICTRUN                 = $00002000;
  {$EXTERNALSYM REST_NOPRINTERTABS}
  REST_NOPRINTERTABS               = $00004000;
  {$EXTERNALSYM REST_NOPRINTERDELETE}
  REST_NOPRINTERDELETE             = $00008000;
  {$EXTERNALSYM REST_NOPRINTERADD}
  REST_NOPRINTERADD                = $00010000;
  {$EXTERNALSYM REST_NOSTARTMENUSUBFOLDERS}
  REST_NOSTARTMENUSUBFOLDERS       = $00020000;
  {$EXTERNALSYM REST_MYDOCSONNET}
  REST_MYDOCSONNET                 = $00040000;
  {$EXTERNALSYM REST_NOEXITTODOS}
  REST_NOEXITTODOS                 = $00080000;
  {$EXTERNALSYM REST_ENFORCESHELLEXTSECURITY}
  REST_ENFORCESHELLEXTSECURITY     = $00100000;
  {$EXTERNALSYM REST_LINKRESOLVEIGNORELINKINFO}
  REST_LINKRESOLVEIGNORELINKINFO   = $00200000;
  {$EXTERNALSYM REST_NOCOMMONGROUPS}
  REST_NOCOMMONGROUPS              = $00400000;
  {$EXTERNALSYM REST_SEPARATEDESKTOPPROCESS}
  REST_SEPARATEDESKTOPPROCESS      = $00800000;
  {$EXTERNALSYM REST_NOWEB}
  REST_NOWEB                       = $01000000;
  {$EXTERNALSYM REST_NOTRAYCONTEXTMENU}
  REST_NOTRAYCONTEXTMENU           = $02000000;
  {$EXTERNALSYM REST_NOVIEWCONTEXTMENU}
  REST_NOVIEWCONTEXTMENU           = $04000000;
  {$EXTERNALSYM REST_NONETCONNECTDISCONNECT}
  REST_NONETCONNECTDISCONNECT      = $08000000;
  {$EXTERNALSYM REST_STARTMENULOGOFF}
  REST_STARTMENULOGOFF             = $10000000;
  {$EXTERNALSYM REST_NOSETTINGSASSIST}
  REST_NOSETTINGSASSIST            = $20000000;
  {$EXTERNALSYM REST_NOINTERNETICON}
  REST_NOINTERNETICON              = $40000001;
  {$EXTERNALSYM REST_NORECENTDOCSHISTORY}
  REST_NORECENTDOCSHISTORY         = $40000002;
  {$EXTERNALSYM REST_NORECENTDOCSMENU}
  REST_NORECENTDOCSMENU            = $40000003;
  {$EXTERNALSYM REST_NOACTIVEDESKTOP}
  REST_NOACTIVEDESKTOP             = $40000004;
  {$EXTERNALSYM REST_NOACTIVEDESKTOPCHANGES}
  REST_NOACTIVEDESKTOPCHANGES      = $40000005;
  {$EXTERNALSYM REST_NOFAVORITESMENU}
  REST_NOFAVORITESMENU             = $40000006;
  {$EXTERNALSYM REST_CLEARRECENTDOCSONEXIT}
  REST_CLEARRECENTDOCSONEXIT       = $40000007;
  {$EXTERNALSYM REST_CLASSICSHELL}
  REST_CLASSICSHELL                = $40000008;
  {$EXTERNALSYM REST_NOCUSTOMIZEWEBVIEW}
  REST_NOCUSTOMIZEWEBVIEW          = $40000009;
  {$EXTERNALSYM REST_NOHTMLWALLPAPER}
  REST_NOHTMLWALLPAPER             = $40000010;
  {$EXTERNALSYM REST_NOCHANGINGWALLPAPER}
  REST_NOCHANGINGWALLPAPER         = $40000011;
  {$EXTERNALSYM REST_NODESKCOMP}
  REST_NODESKCOMP                  = $40000012;
  {$EXTERNALSYM REST_NOADDDESKCOMP}
  REST_NOADDDESKCOMP               = $40000013;
  {$EXTERNALSYM REST_NODELDESKCOMP}
  REST_NODELDESKCOMP               = $40000014;
  {$EXTERNALSYM REST_NOCLOSEDESKCOMP}
  REST_NOCLOSEDESKCOMP             = $40000015;
  {$EXTERNALSYM REST_NOCLOSE_DRAGDROPBAND}
  REST_NOCLOSE_DRAGDROPBAND        = $40000016;   // Disable Close and Drag & Drop on ALL Bands
  {$EXTERNALSYM REST_NOMOVINGBAND}
  REST_NOMOVINGBAND                = $40000017;   // Disable Moving ALL Bands
  {$EXTERNALSYM REST_NOEDITDESKCOMP}
  REST_NOEDITDESKCOMP              = $40000018;
  {$EXTERNALSYM REST_NORESOLVESEARCH}
  REST_NORESOLVESEARCH             = $40000019;
  {$EXTERNALSYM REST_NORESOLVETRACK}
  REST_NORESOLVETRACK              = $4000001A;
  {$EXTERNALSYM REST_FORCECOPYACLWITHFILE}
  REST_FORCECOPYACLWITHFILE        = $4000001B;
  {$EXTERNALSYM REST_NOLOGO3CHANNELNOTIFY}
  REST_NOLOGO3CHANNELNOTIFY        = $4000001C;
  {$EXTERNALSYM REST_NOFORGETSOFTWAREUPDATE}
  REST_NOFORGETSOFTWAREUPDATE      = $4000001D;
  {$EXTERNALSYM REST_NOSETACTIVEDESKTOP}
  REST_NOSETACTIVEDESKTOP          = $4000001E;   // No Active desktop on Settings Menu
  {$EXTERNALSYM REST_NOUPDATEWINDOWS}
  REST_NOUPDATEWINDOWS             = $4000001F;   // No Windows Update on Settings Menu
  {$EXTERNALSYM REST_NOCHANGESTARMENU}
  REST_NOCHANGESTARMENU            = $40000020;   // No Context menu or Drag and Drop on Start menu
  {$EXTERNALSYM REST_NOFOLDEROPTIONS}
  REST_NOFOLDEROPTIONS             = $40000021;   // No Folder Options on Settings Menu
  {$EXTERNALSYM REST_HASFINDCOMPUTERS}
  REST_HASFINDCOMPUTERS            = $40000022;   // Show Start/Search/Computers
  {$EXTERNALSYM REST_INTELLIMENUS}
  REST_INTELLIMENUS                = $40000023;
  {$EXTERNALSYM REST_RUNDLGMEMCHECKBOX}
  REST_RUNDLGMEMCHECKBOX           = $40000024;
  {$EXTERNALSYM REST_ARP_ShowPostSetup}
  REST_ARP_ShowPostSetup           = $40000025;   // ARP: Show Post-Setup page
  {$EXTERNALSYM REST_NOCSC}
  REST_NOCSC                       = $40000026;   // Disable the ClientSide caching on SM
  {$EXTERNALSYM REST_NOCONTROLPANEL}
  REST_NOCONTROLPANEL              = $40000027;   // Remove the Control Panel only from SM|Settings
  {$EXTERNALSYM REST_ENUMWORKGROUP}
  REST_ENUMWORKGROUP               = $40000028;   // Enumerate workgroup in root of nethood
  {$EXTERNALSYM REST_ARP_NOARP}
  REST_ARP_NOARP                   = $40000029;   // ARP: Don't Allow ARP to come up at all
  {$EXTERNALSYM REST_ARP_NOREMOVEPAGE}
  REST_ARP_NOREMOVEPAGE            = $4000002A;   // ARP: Don't allow Remove page
  {$EXTERNALSYM REST_ARP_NOADDPAGE}
  REST_ARP_NOADDPAGE               = $4000002B;   // ARP: Don't allow Add page
  {$EXTERNALSYM REST_ARP_NOWINSETUPPAGE}
  REST_ARP_NOWINSETUPPAGE          = $4000002C;   // ARP: Don't allow opt components page
  {$EXTERNALSYM REST_GREYMSIADS}
  REST_GREYMSIADS                  = $4000002D;   // SM: Allow the greying of Darwin Ads in SM
  {$EXTERNALSYM REST_NOCHANGEMAPPEDDRIVELABEL}
  REST_NOCHANGEMAPPEDDRIVELABEL    = $4000002E;   // Don't enable the UI which allows users to rename mapped drive labels
  {$EXTERNALSYM REST_NOCHANGEMAPPEDDRIVECOMMENT}
  REST_NOCHANGEMAPPEDDRIVECOMMENT  = $4000002F;   // Don't enable the UI which allows users to change mapped drive comments
  {$EXTERNALSYM REST_MaxRecentDocs}
  REST_MaxRecentDocs               = $40000030;
  {$EXTERNALSYM REST_NONETWORKCONNECTIONS}
  REST_NONETWORKCONNECTIONS        = $40000031;   // No Start Menu | Settings |Network Connections
  {$EXTERNALSYM REST_FORCESTARTMENULOGOFF}
  REST_FORCESTARTMENULOGOFF        = $40000032;   // Force logoff on the Start Menu
  {$EXTERNALSYM REST_NOWEBVIEW}
  REST_NOWEBVIEW                   = $40000033;   // Disable Web View
  {$EXTERNALSYM REST_NOCUSTOMIZETHISFOLDER}
  REST_NOCUSTOMIZETHISFOLDER       = $40000034;   // Disable Customize This Folder
  {$EXTERNALSYM REST_NOENCRYPTION}
  REST_NOENCRYPTION                = $40000035;   // Don't allow file encryption
//  Do NOT use me                     $40000036 ;
  {$EXTERNALSYM REST_DONTSHOWSUPERHIDDEN}
  REST_DONTSHOWSUPERHIDDEN         = $40000037;   // don't show super hidden files
  {$EXTERNALSYM REST_NOSHELLSEARCHBUTTON}
  REST_NOSHELLSEARCHBUTTON         = $40000038;
  {$EXTERNALSYM REST_NOHARDWARETAB}
  REST_NOHARDWARETAB               = $40000039;   // No Hardware tab on Drives or in control panel
  {$EXTERNALSYM REST_NORUNASINSTALLPROMPT}
  REST_NORUNASINSTALLPROMPT        = $4000003A;   // Don't bring up "Run As" prompt for install programs
  {$EXTERNALSYM REST_PROMPTRUNASINSTALLNETPATH}
  REST_PROMPTRUNASINSTALLNETPATH   = $4000003B;   // Force the  "Run As" prompt for install programs on unc/network shares
  {$EXTERNALSYM REST_NOMANAGEMYCOMPUTERVERB}
  REST_NOMANAGEMYCOMPUTERVERB      = $4000003C;   // No Manage verb on My Computer
  {$EXTERNALSYM REST_NORECENTDOCSNETHOOD}
  REST_NORECENTDOCSNETHOOD         = $4000003D;   // dont add the recent docs shares to nethood
  {$EXTERNALSYM REST_DISALLOWRUN}
  REST_DISALLOWRUN                 = $4000003E;   // don't allow certain apps to be run
  {$EXTERNALSYM REST_NOWELCOMESCREEN}
  REST_NOWELCOMESCREEN             = $4000003F;   // don't allow the welcome screen to be displayed.
  {$EXTERNALSYM REST_RESTRICTCPL}
  REST_RESTRICTCPL                 = $40000040;   // only allow certain cpls to be run
  {$EXTERNALSYM REST_DISALLOWCPL}
  REST_DISALLOWCPL                 = $40000041;   // don't allow certain cpls to be run
  {$EXTERNALSYM REST_NOSMBALLOONTIP}
  REST_NOSMBALLOONTIP              = $40000042;   // No Start Menu Balloon Tip
  {$EXTERNALSYM REST_NOSMHELP}
  REST_NOSMHELP                    = $40000043;   // No Help on the Start Menu
  {$EXTERNALSYM REST_NOWINKEYS}
  REST_NOWINKEYS                   = $40000044;   // No Windows-X Hot keys
  {$EXTERNALSYM REST_NOENCRYPTONMOVE}
  REST_NOENCRYPTONMOVE             = $40000045;   // Don't automatically try to encrypt files that are moved to encryped directories
  {$EXTERNALSYM REST_NOLOCALMACHINERUN}
  REST_NOLOCALMACHINERUN           = $40000046;   // ignore HKLM\sw\ms\win\cv\Run and all of it's sub keys
  {$EXTERNALSYM REST_NOCURRENTUSERRUN}
  REST_NOCURRENTUSERRUN            = $40000047;   // ignore HKCU\sw\ms\win\cv\Run and all of it's sub keys
  {$EXTERNALSYM REST_NOLOCALMACHINERUNONCE}
  REST_NOLOCALMACHINERUNONCE       = $40000048;   // ignore HKLM\sw\ms\win\cv\RunOnce and all of it's sub keys
  {$EXTERNALSYM REST_NOCURRENTUSERRUNONCE}
  REST_NOCURRENTUSERRUNONCE        = $40000049;   // ignore HKCU\sw\ms\win\cv\RunOnce and all of it's sub keys
  {$EXTERNALSYM REST_FORCEACTIVEDESKTOPON}
  REST_FORCEACTIVEDESKTOPON        = $4000004A;   // Force ActiveDesktop to be turned ON all the time.
  {$EXTERNALSYM REST_NOCOMPUTERSNEARME}
  REST_NOCOMPUTERSNEARME           = $4000004B;   // removes the "Computers near me" link
  {$EXTERNALSYM REST_NOVIEWONDRIVE}
  REST_NOVIEWONDRIVE               = $4000004C;   // disallows CreateViewObject() on specified drives (CFSFolder only)
  {$EXTERNALSYM REST_NONETCRAWL}
  REST_NONETCRAWL                  = $4000004D;   // disables the crawling of the WNet namespace.
  {$EXTERNALSYM REST_NOSHAREDDOCUMENTS}
  REST_NOSHAREDDOCUMENTS           = $4000004E;   // don't auto share the Shared Documents/create link
  {$EXTERNALSYM REST_NOSMMYDOCS}
  REST_NOSMMYDOCS                  = $4000004F;   // Don't show the My Documents item on the Start Menu.
  {$EXTERNALSYM REST_NOSMMYPICS}
  REST_NOSMMYPICS                  = $40000050;   // Don't show the My Pictures item on the Start Menu
  {$EXTERNALSYM REST_ALLOWBITBUCKDRIVES}
  REST_ALLOWBITBUCKDRIVES          = $40000051;   // Bit mask indicating which which drives have bit bucket support
  {$EXTERNALSYM REST_NONLEGACYSHELLMODE}
  REST_NONLEGACYSHELLMODE          = $40000052;   // new consumer shell modes
  {$EXTERNALSYM REST_NOCONTROLPANELBARRICADE}
  REST_NOCONTROLPANELBARRICADE     = $40000053;   // The webview barricade in Control Panel
  {$EXTERNALSYM REST_NOSTARTPAGE}
  REST_NOSTARTPAGE                 = $40000054;   // Whistler Start Page on desktop.
  {$EXTERNALSYM REST_NOAUTOTRAYNOTIFY}
  REST_NOAUTOTRAYNOTIFY            = $40000055;   // Whistler auto-tray notify feature
  {$EXTERNALSYM REST_NOTASKGROUPING}
  REST_NOTASKGROUPING              = $40000056;   // Whistler taskbar button grouping feature
  {$EXTERNALSYM REST_NOCDBURNING}
  REST_NOCDBURNING                 = $40000057;   // whistler cd burning feature
  {$EXTERNALSYM REST_MYCOMPNOPROP}
  REST_MYCOMPNOPROP                = $40000058;   // disables Properties on My Computer's context menu
  {$EXTERNALSYM REST_MYDOCSNOPROP}
  REST_MYDOCSNOPROP                = $40000059;   // disables Properties on My Documents' context menu
  {$EXTERNALSYM REST_NOSTARTPANEL}
  REST_NOSTARTPANEL                = $4000005A;   // Windows start panel (New start menu) for Whistler.
  {$EXTERNALSYM REST_NODISPLAYAPPEARANCEPAGE}
  REST_NODISPLAYAPPEARANCEPAGE     = $4000005B;   // disable Themes and Appearance tabs in the Display Control Panel.
  {$EXTERNALSYM REST_NOTHEMESTAB}
  REST_NOTHEMESTAB                 = $4000005C;   // disable the Themes tab in the Display Control Panel.
  {$EXTERNALSYM REST_NOVISUALSTYLECHOICE}
  REST_NOVISUALSTYLECHOICE         = $4000005D;   // disable the visual style drop down in the Appearance tab of the Display Control Panel.
  {$EXTERNALSYM REST_NOSIZECHOICE}
  REST_NOSIZECHOICE                = $4000005E;   // disable the size drop down in the Appearance tab of the Display Control Panel.
  {$EXTERNALSYM REST_NOCOLORCHOICE}
  REST_NOCOLORCHOICE               = $4000005F;   // disable the color drop down in the Appearance tab of the Display Control Panel.
  {$EXTERNALSYM REST_SETVISUALSTYLE}
  REST_SETVISUALSTYLE              = $40000060;   // Load the specified file as the visual style.
  {$EXTERNALSYM REST_STARTRUNNOHOMEPATH}
  REST_STARTRUNNOHOMEPATH          = $40000061;   // dont use the %HOMEPATH% env var for the Start-Run dialog
  {$EXTERNALSYM REST_NOUSERNAMEINSTARTPANEL}
  REST_NOUSERNAMEINSTARTPANEL      = $40000062;   // don't show the username is the startpanel.
  {$EXTERNALSYM REST_NOMYCOMPUTERICON}
  REST_NOMYCOMPUTERICON            = $40000063;   // don't show my computer anywhere ;  hide its contents
  {$EXTERNALSYM REST_NOSMNETWORKPLACES}
  REST_NOSMNETWORKPLACES           = $40000064;   // don't show network places in startpanel.
  {$EXTERNALSYM REST_NOSMPINNEDLIST}
  REST_NOSMPINNEDLIST              = $40000065;   // don't show the pinned list in startpanel.
  {$EXTERNALSYM REST_NOSMMYMUSIC}
  REST_NOSMMYMUSIC                 = $40000066;   // don't show MyMusic folder in startpanel
  {$EXTERNALSYM REST_NOSMEJECTPC}
  REST_NOSMEJECTPC                 = $40000067;   // don't show "Undoc PC" command in startmenu
  {$EXTERNALSYM REST_NOSMMOREPROGRAMS}
  REST_NOSMMOREPROGRAMS            = $40000068;   // don't show "More Programs" button in StartPanel.
  {$EXTERNALSYM REST_NOSMMFUPROGRAMS}
  REST_NOSMMFUPROGRAMS             = $40000069;   // don't show the MFU programs list in StartPanel.
  {$EXTERNALSYM REST_NOTRAYITEMSDISPLAY}
  REST_NOTRAYITEMSDISPLAY          = $4000006A;   // disables the display of the system tray
  {$EXTERNALSYM REST_NOTOOLBARSONTASKBAR}
  REST_NOTOOLBARSONTASKBAR         = $4000006B;   // disables toolbar display on the taskbar
  {$EXTERNALSYM REST_NOSMCONFIGUREPROGRAMS}
  REST_NOSMCONFIGUREPROGRAMS       = $4000006F;   // No Configure Programs on Settings Menu
  {$EXTERNALSYM REST_HIDECLOCK}
  REST_HIDECLOCK                   = $40000070;   // don't show the clock
  {$EXTERNALSYM REST_NOLOWDISKSPACECHECKS}
  REST_NOLOWDISKSPACECHECKS        = $40000071;   // disable the low disk space checking
  {$EXTERNALSYM REST_NOENTIRENETWORK}
  REST_NOENTIRENETWORK             = $40000072;   // removes the "Entire Network" link (i.e. from "My Network Places")
  {$EXTERNALSYM REST_NODESKTOPCLEANUP}
  REST_NODESKTOPCLEANUP            = $40000073;   // disable the desktop cleanup wizard
  {$EXTERNALSYM REST_BITBUCKNUKEONDELETE}
  REST_BITBUCKNUKEONDELETE         = $40000074;   // disables recycling of files
  {$EXTERNALSYM REST_BITBUCKCONFIRMDELETE}
  REST_BITBUCKCONFIRMDELETE        = $40000075;   // always show the delete confirmation dialog when deleting files
  {$EXTERNALSYM REST_BITBUCKNOPROP}
  REST_BITBUCKNOPROP               = $40000076;   // disables Properties on Recycle Bin's context menu
  {$EXTERNALSYM REST_NODISPBACKGROUND}
  REST_NODISPBACKGROUND            = $40000077;   // disables the Desktop tab in the Display CPL
  {$EXTERNALSYM REST_NODISPSCREENSAVEPG}
  REST_NODISPSCREENSAVEPG          = $40000078;   // disables the Screen Saver tab in the Display CPL
  {$EXTERNALSYM REST_NODISPSETTINGSPG}
  REST_NODISPSETTINGSPG            = $40000079;   // disables the Settings tab in the Display CPL
  {$EXTERNALSYM REST_NODISPSCREENSAVEPREVIEW}
  REST_NODISPSCREENSAVEPREVIEW     = $4000007A;   // disables the screen saver on the Screen Saver tab in the Display CPL
  {$EXTERNALSYM REST_NODISPLAYCPL}
  REST_NODISPLAYCPL                = $4000007B;   // disables the Display CPL
  {$EXTERNALSYM REST_HIDERUNASVERB}
  REST_HIDERUNASVERB               = $4000007C;   // hides the "Run As..." context menu item
  {$EXTERNALSYM REST_NOTHUMBNAILCACHE}
  REST_NOTHUMBNAILCACHE            = $4000007D;   // disables use of the thumbnail cache
  {$EXTERNALSYM REST_NOSTRCMPLOGICAL}
  REST_NOSTRCMPLOGICAL             = $4000007E;   // dont use StrCmpLogical() instead use default CompareString()
  {$EXTERNALSYM REST_NOPUBLISHWIZARD}
  REST_NOPUBLISHWIZARD             = $4000007F;   // disables publishing wizard (WPW)
  {$EXTERNALSYM REST_NOONLINEPRINTSWIZARD}
  REST_NOONLINEPRINTSWIZARD        = $40000080;   // disables online prints wizard (OPW)
  {$EXTERNALSYM REST_NOWEBSERVICES}
  REST_NOWEBSERVICES               = $40000081;   // disables the web specified services for both OPW and WPW
  {$EXTERNALSYM REST_ALLOWUNHASHEDWEBVIEW}
  REST_ALLOWUNHASHEDWEBVIEW        = $40000082;   // allow the user to be promted to accept web view templates that don't already have an md5 hash in the registry
  {$EXTERNALSYM REST_ALLOWLEGACYWEBVIEW}
  REST_ALLOWLEGACYWEBVIEW          = $40000083;   // allow legacy webview template to be shown.
  {$EXTERNALSYM REST_REVERTWEBVIEWSECURITY}
  REST_REVERTWEBVIEWSECURITY       = $40000084;   // disable added webview security measures (revert to w2kfunctionality).
  {$EXTERNALSYM REST_INHERITCONSOLEHANDLES}
  REST_INHERITCONSOLEHANDLES       = $40000086;   // ShellExec() will check for the current process and target process being console processes to inherit handles
  {$EXTERNALSYM REST_SORTMAXITEMCOUNT}
  REST_SORTMAXITEMCOUNT            = $40000087;   // Do not sort views with more items than this key. Useful for viewing big amount of files in one folder.
  {$EXTERNALSYM REST_NOREMOTERECURSIVEEVENTS}
  REST_NOREMOTERECURSIVEEVENTS     = $40000089;   // Dont register network change events recursively to avoid network traffic
  {$EXTERNALSYM REST_NOREMOTECHANGENOTIFY}
  REST_NOREMOTECHANGENOTIFY        = $40000091;   // Do not register for remote change notifies
  {$EXTERNALSYM REST_NOSIMPLENETIDLIST}
  REST_NOSIMPLENETIDLIST           = $40000092;   // No simple network IDLists
  {$EXTERNALSYM REST_NOENUMENTIRENETWORK}
  REST_NOENUMENTIRENETWORK         = $40000093;   // Don't enumerate entire network if we happen to get to it (in conjunction with REST_NOENTIRENETWORK)
  {$EXTERNALSYM REST_NODETAILSTHUMBNAILONNETWORK}
  REST_NODETAILSTHUMBNAILONNETWORK = $40000094;   // Disable Thumbnail for Network files in DUI Details pane
  {$EXTERNALSYM REST_NOINTERNETOPENWITH}
  REST_NOINTERNETOPENWITH          = $40000095;   // dont allow looking on the internet for file associations
  {$EXTERNALSYM REST_ALLOWLEGACYLMZBEHAVIOR}
  REST_ALLOWLEGACYLMZBEHAVIOR      = $4000009A;   // allowable LMZ behavior for ActiveX objects changed in XPSP2 ;  this policy gets the pre-XPSP2 behavior
  {$EXTERNALSYM REST_DONTRETRYBADNETNAME}
  REST_DONTRETRYBADNETNAME         = $4000009B;   // In Network Places: if provider returns ERROR_BAD_NET_NAME ;  give up
  {$EXTERNALSYM REST_ALLOWFILECLSIDJUNCTIONS}
  REST_ALLOWFILECLSIDJUNCTIONS     = $4000009C;   // re-enable legacy support for file. { guid }  junctions in FileSystem Folder
  {$EXTERNALSYM REST_NOUPNPINSTALL}
  REST_NOUPNPINSTALL               = $4000009D;   // disable "install UPnP" task in My Net Places
  {$EXTERNALSYM REST_NODISCONNECT}
  REST_NODISCONNECT                = $41000001;   // No Disconnect option in Start menu
  {$EXTERNALSYM REST_NOSECURITY}
  REST_NOSECURITY                  = $41000002;   // No Security option in start menu
  {$EXTERNALSYM REST_NOFILEASSOCIATE}
  REST_NOFILEASSOCIATE             = $41000003;   // Do not allow user to change file association
  {$EXTERNALSYM REST_ALLOWCOMMENTTOGGLE}
  REST_ALLOWCOMMENTTOGGLE          = $41000004;   // Allow the user to toggle the positions of the Comment and the Computer Name
  {$EXTERNALSYM REST_USEDESKTOPINICACHE}
  REST_USEDESKTOPINICACHE          = $41000005;   // Cache desktop.ini entries from network folders

{$EXTERNALSYM OpenRegStream}
function OpenRegStream(hkey: HKEY; pszSubkey, pszValue: PWideChar;
  grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHFindFiles}
function SHFindFiles(pidlFolder, pidlSaveFile: PItemIDList): BOOL; stdcall;
{$EXTERNALSYM PathGetShortPath}
procedure PathGetShortPath(pszLongPath: PWideChar); stdcall;
{$EXTERNALSYM PathYetAnotherMakeUniqueName}
function PathYetAnotherMakeUniqueName(
  pszUniqueName, pszPath, pszShort, pszFileSpec: PWideChar): BOOL; stdcall;
{$EXTERNALSYM Win32DeleteFile}
function Win32DeleteFile(pszPath: PWideChar): BOOL; stdcall;

const
//
// Path processingfunction
//
  {$EXTERNALSYM PPCF_ADDQUOTES}
  PPCF_ADDQUOTES               = $00000001;        // return a quoted name if required
  {$EXTERNALSYM PPCF_ADDARGUMENTS}
  PPCF_ADDARGUMENTS            = $00000003;        // appends arguments (and wraps in quotes if required)
  {$EXTERNALSYM PPCF_NODIRECTORIES}
  PPCF_NODIRECTORIES           = $00000010;        // don't match to directories
  {$EXTERNALSYM PPCF_FORCEQUALIFY}
  PPCF_FORCEQUALIFY            = $00000040;        // qualify even non-relative names
  {$EXTERNALSYM PPCF_LONGESTPOSSIBLE}
  PPCF_LONGESTPOSSIBLE         = $00000080;        // always find the longest possible name

  {$EXTERNALSYM PathProcessCommand}
function PathProcessCommand(lpSrc, lpDest: PWideChar;
    iMax: Integer; dwFlags: DWORD): Longint stdcall;
{$EXTERNALSYM SHRestricted}
function SHRestricted(rest: TRestrictions): DWORD; stdcall;
{$EXTERNALSYM SignalFileOpen}
function SignalFileOpen(pidl: PItemIDList): BOOL; stdcall;
{$EXTERNALSYM SHSimpleIDListFromPath}
function SHSimpleIDListFromPath(pszPath: PWideChar): PItemIDList; stdcall;

  {$EXTERNALSYM SHLoadOLE}
function SHLoadOLE(lParam: LPARAM): HResult stdcall;

  {$EXTERNALSYM SHStartNetConnectionDialogA}
function SHStartNetConnectionDialogA(hwnd: HWND; pszRemoteName: PAnsiChar;
    dwType: DWORD): HResult; stdcall;
  {$EXTERNALSYM SHStartNetConnectionDialogW}
function SHStartNetConnectionDialogW(hwnd: HWND; pszRemoteName: PWideChar;
    dwType: DWORD): HResult; stdcall;
  {$EXTERNALSYM SHStartNetConnectionDialog}
function SHStartNetConnectionDialog(hwnd: HWND; pszRemoteName: PTSTR;
    dwType: DWORD): HResult; stdcall;

{$EXTERNALSYM SHDefExtractIconA}
function SHDefExtractIconA(pszIconFile: PAnsiChar; iIndex: Integer;
  uFlags: UINT; out phiconLarge, phiconSmall: HICON;
  nIconSize: UINT): HResult; stdcall;
{$EXTERNALSYM SHDefExtractIconW}
function SHDefExtractIconW(pszIconFile: PWideChar; iIndex: Integer;
  uFlags: UINT; out phiconLarge, phiconSmall: HICON;
  nIconSize: UINT): HResult; stdcall;
{$EXTERNALSYM SHDefExtractIcon}
function SHDefExtractIcon(pszIconFile: PTSTR; iIndex: Integer;
  uFlags: UINT; out phiconLarge, phiconSmall: HICON;
  nIconSize: UINT): HResult; stdcall;
{$EXTERNALSYM Shell_GetImageLists}
function Shell_GetImageLists(out phiml, phimlSmall: HIMAGELIST): BOOL; stdcall;
{$EXTERNALSYM Shell_GetCachedImageIndex}
function Shell_GetCachedImageIndex(pszIconPath: PWideChar;
  iIconIndex: Integer; uIconFlags: UINT): Integer; stdcall;

type
//
// IDocViewSite
//
  {$EXTERNALSYM IDocViewSite}
  IDocViewSite = interface(IUnknown)
  ['{87D605E0-C511-11CF-89A9-00A0C9054129}']
    function OnSetTitle(pvTitle: PVARIANTARG): HResult; stdcall;
  end;

const  
  {$EXTERNALSYM VALIDATEUNC_NOUI}
  VALIDATEUNC_NOUI        = $0002;      // don't bring up UI
  {$EXTERNALSYM VALIDATEUNC_CONNECT}
  VALIDATEUNC_CONNECT     = $0001;      // connect a drive letter
  {$EXTERNALSYM VALIDATEUNC_PRINT}
  VALIDATEUNC_PRINT       = $0004;      // validate as print share instead of disk share
  {$EXTERNALSYM VALIDATEUNC_VALID}
  VALIDATEUNC_VALID       = $0007;      // valid flags

{$EXTERNALSYM SHValidateUNC}
function SHValidateUNC(hwndOwner: HWND; pszFile: PWideChar; fConnect: UINT): BOOL; stdcall;

const
  {$EXTERNALSYM OPENPROPS_NONE}
  OPENPROPS_NONE          = $0000;
  {$EXTERNALSYM OPENPROPS_INHIBITPIF}
  OPENPROPS_INHIBITPIF    = $8000;
  {$EXTERNALSYM GETPROPS_NONE}
  GETPROPS_NONE           = $0000;
  {$EXTERNALSYM SETPROPS_NONE}
  SETPROPS_NONE           = $0000;
  {$EXTERNALSYM CLOSEPROPS_NONE}
  CLOSEPROPS_NONE         = $0000;
  {$EXTERNALSYM CLOSEPROPS_DISCARD}
  CLOSEPROPS_DISCARD      = $0001;

  {$EXTERNALSYM PIFNAMESIZE}
  PIFNAMESIZE    =  30;
  {$EXTERNALSYM PIFSTARTLOCSIZE}
  PIFSTARTLOCSIZE=  63;
  {$EXTERNALSYM PIFDEFPATHSIZE}
  PIFDEFPATHSIZE =  64;
  {$EXTERNALSYM PIFPARAMSSIZE}
  PIFPARAMSSIZE  =  64;
  {$EXTERNALSYM PIFSHPROGSIZE}
  PIFSHPROGSIZE  =  64;
  {$EXTERNALSYM PIFSHDATASIZE}
  PIFSHDATASIZE  =  64;
  {$EXTERNALSYM PIFDEFFILESIZE}
  PIFDEFFILESIZE =  80;
  {$EXTERNALSYM PIFMAXFILEPATH}
  PIFMAXFILEPATH =  260;

type
  PPropPrg = ^TPropPrg;
  {$EXTERNALSYM PROPPRG}
  PROPPRG = record                    (* prg *)
    flPrg: Word;                        // see PRG_ flags
    flPrgInit: Word;                    // see PRGINIT_ flags
    achTitle: array[0..PIFNAMESIZE - 1] of AnsiChar;          // name[30]
    achCmdLine: array[0..PIFSTARTLOCSIZE + PIFPARAMSSIZE] of AnsiChar; // startfile[63] + params[64]
    achWorkDir: array[0..PIFDEFPATHSIZE - 1] of AnsiChar;     // defpath[64]
    wHotKey: Word;                      // PfHotKeyScan thru PfHotKeyVal
    achIconFile: array[0..PIFDEFFILESIZE - 1] of AnsiChar;    // name of file containing icon
    wIconIndex: Word;                   // index of icon within file
    dwEnhModeFlags: DWORD;              // reserved enh-mode flags
    dwRealModeFlags: DWORD;             // real-mode flags (see RMOPT_*)
    achOtherFile: array[0..PIFDEFFILESIZE - 1] of AnsiChar;   // name of "other" file in directory
    achPIFFile: array[0..PIFMAXFILEPATH - 1] of AnsiChar;     // name of PIF file
  end;
  TPropPrg = PROPPRG;

{$EXTERNALSYM PifMgr_OpenProperties}
function PifMgr_OpenProperties(pszApp, pszPIF: PWideChar;
  hInf, flOpt: UINT): THandle; stdcall;
{$EXTERNALSYM PifMgr_GetProperties}
function PifMgr_GetProperties(hProps: THandle; pszGroup: PAnsiChar;
  lpProps: Pointer; cbProps: Integer; flOpt: UINT): Integer; stdcall;
{$EXTERNALSYM PifMgr_SetProperties}
function PifMgr_SetProperties(hProps: THandle; pszGroup: PAnsiChar;
  lpProps: Pointer; cbProps: Integer; flOpt: UINT): Integer; stdcall;
{$EXTERNALSYM PifMgr_CloseProperties}
function PifMgr_CloseProperties(hProps: THandle;
  flOpt: UINT): THandle; stdcall;

{$EXTERNALSYM SHSetInstanceExplorer}
procedure SHSetInstanceExplorer(punk: IUnknown); stdcall;
{$EXTERNALSYM IsUserAnAdmin}
function IsUserAnAdmin: BOOL; stdcall;

type
  {$EXTERNALSYM IInitializeObject}
  IInitializeObject = interface(IUnknown)
  // ['{}'] IID_InitializeObject unknown
    function Initialize: HResult; stdcall;
  end;

const
  {$EXTERNALSYM BMICON_LARGE}
  BMICON_LARGE = 0;
  {$EXTERNALSYM BMICON_SMALL}
  BMICON_SMALL = 1;

type
  {$EXTERNALSYM IBanneredBar}
  IBanneredBar = interface(IUnknown)
  ['{596A9A94-013E-11D1-8D34-00A0C90F2719}']
    function SetIconSize(iIcon: DWORD): HResult; stdcall;
    function GetIconSize(out piIcon: DWORD): HResult; stdcall;
    function SetBitmap(hBitmap: HBITMAP): HResult; stdcall;
    function GetBitmap(out phBitmap: HBITMAP): HResult; stdcall;
  end;

{$EXTERNALSYM SHShellFolderView_Message}
function SHShellFolderView_Message(hwndMain: HWND; uMsg: UINT; lParam: LPARAM): LRESULT; stdcall;

type
//
// Callback interface for the IShellFolderView
//
  {$EXTERNALSYM IShellFolderViewCB}
  IShellFolderViewCB = interface(IUnknown)
  ['{2047E320-F2A9-11CE-AE65-08002B2E1262}']
    function MessageSFVCB(uMsg: UINT; wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
  end;

const
  {$EXTERNALSYM QCMINFO_PLACE_BEFORE}
  QCMINFO_PLACE_BEFORE   =  0;
  {$EXTERNALSYM QCMINFO_PLACE_AFTER}
  QCMINFO_PLACE_AFTER    =  1;

type
  PQCMInfoIDMapPlacement = ^TQCMInfoIDMapPlacement;
  {$EXTERNALSYM _QCMINFO_IDMAP_PLACEMENT}
  _QCMINFO_IDMAP_PLACEMENT = record
    id: UINT;
    fFlags: UINT;
  end;
  {$EXTERNALSYM QCMINFO_IDMAP_PLACEMENT}
  QCMINFO_IDMAP_PLACEMENT = _QCMINFO_IDMAP_PLACEMENT;
  TQCMInfoIDMapPlacement = _QCMINFO_IDMAP_PLACEMENT;

  PQCMInfoIDmap = ^TQCMInfoIDMap;
  {$EXTERNALSYM _QCMINFO_IDMAP}
  _QCMINFO_IDMAP = record
    nMaxIds: UINT;
    pIdList: array[0..0] of QCMINFO_IDMAP_PLACEMENT;
  end;
  {$EXTERNALSYM QCMINFO_IDMAP}
  QCMINFO_IDMAP = _QCMINFO_IDMAP;
  TQCMInfoIDMap = _QCMINFO_IDMAP;

  PQCMInfo = ^TQCMInfo;
  {$EXTERNALSYM _QCMINFO}
  _QCMINFO = record
    hmenu: HMENU;          // in
    indexMenu: UINT;       // in
    idCmdFirst: UINT;      // in/out
    idCmdLast: UINT;       // in
    pIdMap: PQCMInfoIDMap; // in / unused
  end;
  {$EXTERNALSYM QCMINFO}
  QCMINFO = _QCMINFO;
  TQCMInfo = _QCMINFO;

const
// TBINFO flags
  {$EXTERNALSYM TBIF_APPEND}
  TBIF_APPEND    =  0;
  {$EXTERNALSYM TBIF_PREPEND}
  TBIF_PREPEND   =  1;
  {$EXTERNALSYM TBIF_REPLACE}
  TBIF_REPLACE   =  2;
  {$EXTERNALSYM TBIF_DEFAULT}
  TBIF_DEFAULT      = $00000000;
  {$EXTERNALSYM TBIF_INTERNETBAR}
  TBIF_INTERNETBAR  = $00010000;
  {$EXTERNALSYM TBIF_STANDARDTOOLBAR}
  TBIF_STANDARDTOOLBAR   = $00020000;
  {$EXTERNALSYM TBIF_NOTOOLBAR}
  TBIF_NOTOOLBAR  = $00030000;

type
  PTBInfo = ^TTBInfo;
  {$EXTERNALSYM _TBINFO}
  _TBINFO = record
    cbuttons: UINT;       // out
    uFlags: UINT;         // out (one of TBIF_ flags)
  end;
  {$EXTERNALSYM TBINFO}
  TBINFO = _TBINFO;
  TTBInfo = _TBINFO;

  PDetailsInfo = ^TDetailsInfo;
  {$EXTERNALSYM _DETAILSINFO}
  _DETAILSINFO = record
    pidl: PItemIDList;
    fmt: Integer;
    cxChar: Integer;
    str: TStrRet;
    iImage: Integer;
  end;
  {$EXTERNALSYM DETAILSINFO}
  DETAILSINFO = _DETAILSINFO;
  TDetailsInfo = _DETAILSINFO;

  PSFVMPropPageData = ^TSFVMPropPageData;
  {$EXTERNALSYM _SFVM_PROPPAGE_DATA}
  _SFVM_PROPPAGE_DATA = record
    dwReserved: DWORD;
    pfn: TFnAddPropSheetPage;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM SFVM_PROPPAGE_DATA}
  SFVM_PROPPAGE_DATA = _SFVM_PROPPAGE_DATA;
  TSFVMPropPageData = _SFVM_PROPPAGE_DATA;

  PSFVMHelpTopicData = ^TSFVMHelpTopicData;
  {$EXTERNALSYM _SFVM_HELPTOPIC_DATA}
  _SFVM_HELPTOPIC_DATA = record
    wszHelpFile: array[0..MAX_PATH - 1] of WideChar;
    wszHelpTopic: array[0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM SFVM_HELPTOPIC_DATA}
  SFVM_HELPTOPIC_DATA = _SFVM_HELPTOPIC_DATA;
  TSFVMHelpTopicData = _SFVM_HELPTOPIC_DATA;

const
//                            uMsg    wParam             lParam
  {$EXTERNALSYM SFVM_MERGEMENU}
  SFVM_MERGEMENU           =  1;   // -                  LPQCMINFO
  {$EXTERNALSYM SFVM_INVOKECOMMAND}
  SFVM_INVOKECOMMAND       =  2;   // idCmd              -
  {$EXTERNALSYM SFVM_GETHELPTEXT}
  SFVM_GETHELPTEXT         =  3;   // idCmd,cchMax       pszText
  {$EXTERNALSYM SFVM_GETTOOLTIPTEXT}
  SFVM_GETTOOLTIPTEXT      =  4;   // idCmd,cchMax       pszText
  {$EXTERNALSYM SFVM_GETBUTTONINFO}
  SFVM_GETBUTTONINFO       =  5;   // -                  LPTBINFO
  {$EXTERNALSYM SFVM_GETBUTTONS}
  SFVM_GETBUTTONS          =  6;   // idCmdFirst,cbtnMax LPTBBUTTON
  {$EXTERNALSYM SFVM_INITMENUPOPUP}
  SFVM_INITMENUPOPUP       =  7;   // idCmdFirst,nIndex  hmenu
  {$EXTERNALSYM SFVM_FSNOTIFY}
  SFVM_FSNOTIFY            =  14;  // PItemIDList*       lEvent
  {$EXTERNALSYM SFVM_WINDOWCREATED}
  SFVM_WINDOWCREATED       =  15;  // hwnd               -
  {$EXTERNALSYM SFVM_GETDETAILSOF}
  SFVM_GETDETAILSOF        =  23;  // iColumn            DETAILSINFO*
  {$EXTERNALSYM SFVM_COLUMNCLICK}
  SFVM_COLUMNCLICK         =  24;  // iColumn            -
  {$EXTERNALSYM SFVM_QUERYFSNOTIFY}
  SFVM_QUERYFSNOTIFY       =  25;  // -                  SHChangeNotifyEntry *
  {$EXTERNALSYM SFVM_DEFITEMCOUNT}
  SFVM_DEFITEMCOUNT        =  26;  // -                  UINT*
  {$EXTERNALSYM SFVM_DEFVIEWMODE}
  SFVM_DEFVIEWMODE         =  27;  // -                  FOLDERVIEWMODE*
  {$EXTERNALSYM SFVM_UNMERGEMENU}
  SFVM_UNMERGEMENU         =  28;  // -                  hmenu
  {$EXTERNALSYM SFVM_UPDATESTATUSBAR}
  SFVM_UPDATESTATUSBAR     =  31;  // fInitialize        -
  {$EXTERNALSYM SFVM_BACKGROUNDENUM}
  SFVM_BACKGROUNDENUM      =  32;  // -                  -
  {$EXTERNALSYM SFVM_DIDDRAGDROP}
  SFVM_DIDDRAGDROP         =  36;  // dwEffect           IDataObject *
  {$EXTERNALSYM SFVM_SETISFV}
  SFVM_SETISFV             =  39;  // -                  IShellFolderView*
  {$EXTERNALSYM SFVM_THISIDLIST}
  SFVM_THISIDLIST          =  41;  // -                  LPITMIDLIST*
  {$EXTERNALSYM SFVM_ADDPROPERTYPAGES}
  SFVM_ADDPROPERTYPAGES    =  47;  // -                  SFVM_PROPPAGE_DATA *
  {$EXTERNALSYM SFVM_BACKGROUNDENUMDONE}
  SFVM_BACKGROUNDENUMDONE  =  48;  // -                  -
  {$EXTERNALSYM SFVM_GETNOTIFY}
  SFVM_GETNOTIFY           =  49;  // PItemIDList*       Longint*
  {$EXTERNALSYM SFVM_GETSORTDEFAULTS}
  SFVM_GETSORTDEFAULTS     =  53;  // iDirection         iParamSort
  {$EXTERNALSYM SFVM_SIZE}
  SFVM_SIZE                =  57;  // -                  -
  {$EXTERNALSYM SFVM_GETZONE}
  SFVM_GETZONE             =  58;  // -                  DWORD*
  {$EXTERNALSYM SFVM_GETPANE}
  SFVM_GETPANE             =  59;  // Pane ID            DWORD*
  {$EXTERNALSYM SFVM_GETHELPTOPIC}
  SFVM_GETHELPTOPIC        =  63;  // -                  SFVM_HELPTOPIC_DATA *
  {$EXTERNALSYM SFVM_GETANIMATION}
  SFVM_GETANIMATION        =  68;  // HINSTANCE *        WideChar *

type
// SHCreateShellFolderView struct
  PSFVCreate = ^TSFVCreate;
  {$EXTERNALSYM _SFV_CREATE}
  _SFV_CREATE = record
    cbSize: UINT;
    pshf: IShellFolder;
    psvOuter: IShellView;
    psfvcb: IShellFolderViewCB; // No callback if NULL
  end;
  {$EXTERNALSYM SFV_CREATE}
  SFV_CREATE = _SFV_CREATE;
  TSFVCreate = _SFV_CREATE;

{$EXTERNALSYM SHCreateShellFolderView}
function SHCreateShellFolderView(const pcsfv: TSFVCreate;
  out ppsv: IShellView): HResult; stdcall;

type
  {$EXTERNALSYM LPFNDFMCALLBACK}
  LPFNDFMCALLBACK = function(psf: IShellFolder; hwnd: HWND;
    pdtobj: IDataObject; uMsg: UINT; wParam: WPARAM;
    lParam: LPARAM): HResult stdcall;
  TFnDFMCallback = LPFNDFMCALLBACK;

  THKEYArray = array[0..65535] of HKEY;
  PHKEYArray = ^THKEYArray;

{$EXTERNALSYM CDefFolderMenu_Create2}
function CDefFolderMenu_Create2(pidlFolder: PItemIDList; hwnd: HWND;
  cidl: UINT; apidl: PPItemIDListArray; psf: IShellFolder;
  lpfn: TFnDFMCallback; nKeys: UINT; var ahkeyClsKeys: HKEY;
  var ppcm: IContextMenu): HResult; stdcall;

  {$EXTERNALSYM SHOpenPropSheetA}
function SHOpenPropSheetA(pszCaption: PAnsiChar; ahkeys: PHKEYArray;
    cikeys: UINT; const pclsidDefault: TCLSID; pdtobj: IDataObject;
    psb: IShellBrowser; pStartPage: PAnsiChar): BOOL; stdcall;
  {$EXTERNALSYM SHOpenPropSheetW}
function SHOpenPropSheetW(pszCaption: PWideChar; ahkeys: PHKEYArray;
    cikeys: UINT; const pclsidDefault: TCLSID; pdtobj: IDataObject;
    psb: IShellBrowser; pStartPage: PWideChar): BOOL; stdcall;
  {$EXTERNALSYM SHOpenPropSheet}
function SHOpenPropSheet(pszCaption: PTSTR; ahkeys: PHKEYArray;
    cikeys: UINT; const pclsidDefault: TCLSID; pdtobj: IDataObject;
    psb: IShellBrowser; pStartPage: PTSTR): BOOL; stdcall;

const
//                      uMsg       wParam       lParam
  {$EXTERNALSYM DFM_MERGECONTEXTMENU}
  DFM_MERGECONTEXTMENU = 1;     // uFlags       LPQCMINFO
  {$EXTERNALSYM DFM_INVOKECOMMAND}
  DFM_INVOKECOMMAND    = 2;     // idCmd        pszArgs
  {$EXTERNALSYM DFM_GETDEFSTATICID}
  DFM_GETDEFSTATICID   = 14;    // idCmd *      0
// Commands from DFM_INVOKECOMMAND when strings are passed in
  {$EXTERNALSYM DFM_CMD_PROPERTIES}
  DFM_CMD_PROPERTIES   = UINT(-5);

type
  {$EXTERNALSYM LPFNVIEWCALLBACK}
  LPFNVIEWCALLBACK = function(psvOuter: IShellView; psf: IShellFolder;
    hwndMain: HWND; uMsg: UINT; wParam: WPARAM;
    lParam: LPARAM): HResult stdcall;
  TFnViewCallback = LPFNVIEWCALLBACK;

// SHCreateShellFolderViewEx struct
  PCSFV = ^TCSFV;
  {$EXTERNALSYM _CSFV}
  _CSFV = record
    cbSize: UINT;
    pshf: IShellFolder;
    psvOuter: IShellView;
    pidl: PItemIDList;
    lEvents: Longint;
    pfnCallback: TFnViewCallback;       // No callback if NULL
    fvm: TFolderViewMode;
  end;
  {$EXTERNALSYM CSFV}
  CSFV = _CSFV;
  TCSFV = _CSFV;

const
  {$EXTERNALSYM SFVM_REARRANGE}
  SFVM_REARRANGE          = $00000001;
  {$EXTERNALSYM SFVM_ADDOBJECT}
  SFVM_ADDOBJECT          = $00000003;
  {$EXTERNALSYM SFVM_REMOVEOBJECT}
  SFVM_REMOVEOBJECT       = $00000006;
  {$EXTERNALSYM SFVM_UPDATEOBJECT}
  SFVM_UPDATEOBJECT       = $00000007;
  {$EXTERNALSYM SFVM_GETSELECTEDOBJECTS}
  SFVM_GETSELECTEDOBJECTS = $00000009;
  {$EXTERNALSYM SFVM_SETITEMPOS}
  SFVM_SETITEMPOS         = $0000000E;
  {$EXTERNALSYM SFVM_SETCLIPBOARD}
  SFVM_SETCLIPBOARD       = $00000010;
  {$EXTERNALSYM SFVM_SETPOINTS}
  SFVM_SETPOINTS          = $00000017;

// Tell the FolderView to rearrange.  The lParam will be passed to
// IShellFolder::CompareIDs
{$EXTERNALSYM ShellFolderView_ReArrange}
function ShellFolderView_ReArrange(hwnd: HWND; lparam: LPARAM): BOOL; {inline;}

// Add an OBJECT into the view
{$EXTERNALSYM ShellFolderView_AddObject}
function ShellFolderView_AddObject(hwnd: HWND; pidl: PItemIDList): LPARAM;
{  inline;}

// Remove an OBJECT into the view
{$EXTERNALSYM ShellFolderView_RemoveObject}
function ShellFolderView_RemoveObject(hwnd: HWND; pidl: PItemIDList): LPARAM;
  {inline;}

// updates an object by passing in pointer to two PIDLS, the first
// is the old pidl, the second one is the one with update information.
//
// _ppidl[1] must be a *copy* of a pidl, as control over the lifetime
// of the pidl belongs to the view after successful completion of
// this call.  (Unsuccessful completion (a -1 return) implies failure
// and the caller must free the memory.)  Win95 waits a while before
// freeing the pidl, IE4 frees the pidl immediately.
// IShellFolderView::UpdateObject does not suffer from this problem.
//
{$EXTERNALSYM ShellFolderView_UpdateObject}
function ShellFolderView_UpdateObject(hwnd: HWND;
  ppidl: PPItemIDListArray): LPARAM;

// Returns an array of the selected IDS to the caller.
//     lparam is a pointer to receive the idlists into
//     return value is the count of items in the array.
{$EXTERNALSYM ShellFolderView_GetSelectedObjects}
function ShellFolderView_GetSelectedObjects(hwnd: HWND;
  ppidl: PPItemIDListArray): LPARAM;

type
  PSFVSetItemPos = ^TSFVSetItemPos;
  {$EXTERNALSYM _SFV_SETITEMPOS}
  _SFV_SETITEMPOS = record
    pidl: PItemIDList;
    pt: TPoint;
  end;
  {$EXTERNALSYM SFV_SETITEMPOS}
  SFV_SETITEMPOS = _SFV_SETITEMPOS;
  TSFVSetItemPos = _SFV_SETITEMPOS;

// Sets the position of an item in the viewer
//     lparam is a pointer to a SVF_SETITEMPOS
//     return value is unused
{$EXTERNALSYM ShellFolderView_SetItemPos}
procedure ShellFolderView_SetItemPos(hwnd: HWND; pidl: PItemIDList;
  x, y: Integer); {inline;}

//  Notifies a ShellView when one of its objects get put on the clipboard
//  as a result of a menu command.
//
//
//     lparam is the dwEffect (DROPEFFECT_MOVE, DROPEFFECT_COPY)
//     return value is void.
{$EXTERNALSYM ShellFolderView_SetClipboard}
procedure ShellFolderView_SetClipboard(hwnd: HWND; dwEffect: DWORD); {inline;}

{$EXTERNALSYM ShellFolderView_SetPoints}
procedure ShellFolderView_SetPoints(hwnd: HWND; const pdtobj: IDataObject); {inline;}

{$EXTERNALSYM SHFind_InitMenuPopup}
function SHFind_InitMenuPopup(hmenu: HMENU; hwndOwner: HWND;
  idCmdFirst, idCmdLast: UINT): IContextMenu; stdcall;
{$EXTERNALSYM SHCreateShellFolderViewEx}
function SHCreateShellFolderViewEx(var pcsfv: TCSFV; out ppsv: IShellView): HResult; stdcall;

//
// PROPIDs for Internet Shortcuts (FMTID_Intshcut) to be used with
// IPropertySetStorage/IPropertyStorage
//
// The known property ids and their OleVariant types are:
//      PID_IS_URL          [VT_LPWSTR]   URL
//      PID_IS_NAME         [VT_LPWSTR]   Name of the internet shortcut
//      PID_IS_WORKINGDIR   [VT_LPWSTR]   Working directory for the shortcut
//      PID_IS_HOTKEY       [VT_UI2]      Hotkey for the shortcut
//      PID_IS_SHOWCMD      [VT_I4]       Show command for shortcut
//      PID_IS_ICONINDEX    [VT_I4]       Index into file that has icon
//      PID_IS_ICONFILE     [VT_LPWSTR]   File that has the icon
//      PID_IS_WHATSNEW     [VT_LPWSTR]   What's New text
//      PID_IS_AUTHOR       [VT_LPWSTR]   Author
//      PID_IS_DESCRIPTION  [VT_LPWSTR]   Description text of site
//      PID_IS_COMMENT      [VT_LPWSTR]   User annotated comment
//
const
  {$EXTERNALSYM PID_IS_URL}
  PID_IS_URL          =  2;
  {$EXTERNALSYM PID_IS_NAME}
  PID_IS_NAME         =  4;
  {$EXTERNALSYM PID_IS_WORKINGDIR}
  PID_IS_WORKINGDIR   =  5;
  {$EXTERNALSYM PID_IS_HOTKEY}
  PID_IS_HOTKEY       =  6;
  {$EXTERNALSYM PID_IS_SHOWCMD}
  PID_IS_SHOWCMD      =  7;
  {$EXTERNALSYM PID_IS_ICONINDEX}
  PID_IS_ICONINDEX    =  8;
  {$EXTERNALSYM PID_IS_ICONFILE}
  PID_IS_ICONFILE     =  9;
  {$EXTERNALSYM PID_IS_WHATSNEW}
  PID_IS_WHATSNEW     =  10;
  {$EXTERNALSYM PID_IS_AUTHOR}
  PID_IS_AUTHOR       =  11;
  {$EXTERNALSYM PID_IS_DESCRIPTION}
  PID_IS_DESCRIPTION  =  12;
  {$EXTERNALSYM PID_IS_COMMENT}
  PID_IS_COMMENT      =  13;

//
// PROPIDs for Internet Sites (FMTID_InternetSite) to be used with
// IPropertySetStorage/IPropertyStorage
//
// The known property ids and their OleVariant types are:
//      PID_INTSITE_WHATSNEW     [VT_LPWSTR]   What's New text
//      PID_INTSITE_AUTHOR       [VT_LPWSTR]   Author
//      PID_INTSITE_LASTVISIT    [VT_FILETIME] Time site was last visited
//      PID_INTSITE_LASTMOD      [VT_FILETIME] Time site was last modified
//      PID_INTSITE_VISITCOUNT   [VT_UI4]      Number of times user has visited
//      PID_INTSITE_DESCRIPTION  [VT_LPWSTR]   Description text of site
//      PID_INTSITE_COMMENT      [VT_LPWSTR]   User annotated comment
//      PID_INTSITE_RECURSE      [VT_UI4]      Levels to recurse (0-3)
//      PID_INTSITE_WATCH        [VT_UI4]      PIDISM_ flags
//      PID_INTSITE_SUBSCRIPTION [VT_UI8]      Subscription cookie
//      PID_INTSITE_URL          [VT_LPWSTR]   URL
//      PID_INTSITE_TITLE        [VT_LPWSTR]   Title
//      PID_INTSITE_CODEPAGE     [VT_UI4]      Codepage of the document
//      PID_INTSITE_TRACKING     [VT_UI4]      Tracking
//      PID_INTSITE_ICONINDEX    [VT_I4]       Retrieve the index to the icon
//      PID_INTSITE_ICONFILE     [VT_LPWSTR]   Retrieve the file containing the icon index.
//
  {$EXTERNALSYM PID_INTSITE_WHATSNEW}
  PID_INTSITE_WHATSNEW     =  2;
  {$EXTERNALSYM PID_INTSITE_AUTHOR}
  PID_INTSITE_AUTHOR       =  3;
  {$EXTERNALSYM PID_INTSITE_LASTVISIT}
  PID_INTSITE_LASTVISIT    =  4;
  {$EXTERNALSYM PID_INTSITE_LASTMOD}
  PID_INTSITE_LASTMOD      =  5;
  {$EXTERNALSYM PID_INTSITE_VISITCOUNT}
  PID_INTSITE_VISITCOUNT   =  6;
  {$EXTERNALSYM PID_INTSITE_DESCRIPTION}
  PID_INTSITE_DESCRIPTION  =  7;
  {$EXTERNALSYM PID_INTSITE_COMMENT}
  PID_INTSITE_COMMENT      =  8;
  {$EXTERNALSYM PID_INTSITE_FLAGS}
  PID_INTSITE_FLAGS        =  9;
  {$EXTERNALSYM PID_INTSITE_CONTENTLEN}
  PID_INTSITE_CONTENTLEN   =  10;
  {$EXTERNALSYM PID_INTSITE_CONTENTCODE}
  PID_INTSITE_CONTENTCODE  =  11;
  {$EXTERNALSYM PID_INTSITE_RECURSE}
  PID_INTSITE_RECURSE      =  12;
  {$EXTERNALSYM PID_INTSITE_WATCH}
  PID_INTSITE_WATCH        =  13;
  {$EXTERNALSYM PID_INTSITE_SUBSCRIPTION}
  PID_INTSITE_SUBSCRIPTION =  14;
  {$EXTERNALSYM PID_INTSITE_URL}
  PID_INTSITE_URL          =  15;
  {$EXTERNALSYM PID_INTSITE_TITLE}
  PID_INTSITE_TITLE        =  16;
  {$EXTERNALSYM PID_INTSITE_CODEPAGE}
  PID_INTSITE_CODEPAGE     =  18;
  {$EXTERNALSYM PID_INTSITE_TRACKING}
  PID_INTSITE_TRACKING     =  19;
  {$EXTERNALSYM PID_INTSITE_ICONINDEX}
  PID_INTSITE_ICONINDEX    =  20;
  {$EXTERNALSYM PID_INTSITE_ICONFILE}
  PID_INTSITE_ICONFILE     =  21;

// Flags for PID_IS_FLAGS
  {$EXTERNALSYM PIDISF_RECENTLYCHANGED}
  PIDISF_RECENTLYCHANGED  = $00000001;
  {$EXTERNALSYM PIDISF_CACHEDSTICKY}
  PIDISF_CACHEDSTICKY     = $00000002;
  {$EXTERNALSYM PIDISF_CACHEIMAGES}
  PIDISF_CACHEIMAGES      = $00000010;
  {$EXTERNALSYM PIDISF_FOLLOWALLLINKS}
  PIDISF_FOLLOWALLLINKS   = $00000020;

// Values for PID_INTSITE_WATCH
  {$EXTERNALSYM PIDISM_GLOBAL}
  PIDISM_GLOBAL          =  0; // Monitor based on global setting
  {$EXTERNALSYM PIDISM_WATCH}
  PIDISM_WATCH           =  1; // User says watch
  {$EXTERNALSYM PIDISM_DONTWATCH}
  PIDISM_DONTWATCH       =  2; // User says don't watch

////////////////////////////////////////////////////////////////////
//
// The shell keeps track of some per-user state to handle display
// options that is of major interest to ISVs.
// The key one requested right now is "DoubleClickInWebView".

type
  PShellStateA = ^TShellStateA;
  {$EXTERNALSYM SHELLSTATEA}
  SHELLSTATEA = record
    Flags: DWORD;
{
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;

    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1; // No longer used, dead bit
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    BOOL fWebView : 1;
    BOOL fFilter : 1;
    BOOL fShowSuperHidden : 1;
    BOOL fNoNetCrawling : 1;
}
    dwWin95Unused: DWORD; // Win95 only - no longer supported pszHiddenFileExts
    uWin95Unused: UINT; // Win95 only - no longer supported cbHiddenFileExts

    // Note: Not a typo!  This is a persisted structure so we cannot use LPARAM
    lParamSort: Longint;
    iSortDirection: Integer;
    version: UINT;

    // new for win2k. need notUsed var to calc the right size of ie4 struct
    // FIELD_OFFSET does not work on bit fields
    uNotUsed: UINT; // feel free to rename and use
    Flags2: Word;
{
    BOOL fSepProcess: 1;

    // new for Whistler.
    BOOL fStartPanelOn: 1;       //Indicates if the Whistler StartPanel mode is ON or OFF.

    BOOL fShowStartPage: 1;      //Indicates if the Whistler StartPage on desktop is ON or OFF.

    UINT fSpareFlags : 13;
}
  end;
  TShellStateA = SHELLSTATEA;

  PShellStateW = ^TShellStateW;
  {$EXTERNALSYM SHELLSTATEW}
  SHELLSTATEW = record
    Data: DWORD;
{
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;
    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1;
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    BOOL fWebView : 1;
    BOOL fFilter : 1;
    BOOL fShowSuperHidden : 1;
    BOOL fNoNetCrawling : 1;
}
    dwWin95Unused: DWORD; // Win95 only - no longer supported pszHiddenFileExts
    uWin95Unused: UINT; // Win95 only - no longer supported cbHiddenFileExts

    // Note: Not a typo!  This is a persisted structure so we cannot use LPARAM
    lParamSort: Longint;
    iSortDirection: Integer;
    version: UINT;

    // new for win2k. need notUsed var to calc the right size of ie4 struct
    // FIELD_OFFSET does not work on bit fields
    uNotUsed: UINT; // feel free to rename and use
    Flags2: Word;
{
    BOOL fSepProcess: 1;

    // new for Whistler.
    BOOL fStartPanelOn: 1;       //Indicates if the Whistler StartPage mode is ON or OFF.

    BOOL fShowStartPage: 1;      //Indicates if the Whistler StartPage on desktop is ON or OFF.

    // If you need a new flag, steal a bit from from fSpareFlags.
    UINT fSpareFlags : 13;
}
  end;
  TShellStateW = SHELLSTATEW;

  PShellState = PShellStateA;
  {$EXTERNALSYM SHELLSTATE}
  SHELLSTATE = SHELLSTATEA;
  TShellState = TShellStateA;

const
  {$EXTERNALSYM SHELLSTATEVERSION_IE4}
  SHELLSTATEVERSION_IE4   = 9;
  {$EXTERNALSYM SHELLSTATEVERSION_WIN2K}
  SHELLSTATEVERSION_WIN2K = 10;

var
{$EXTERNALSYM SHELLSTATE_SIZE_WIN95}
  SHELLSTATE_SIZE_WIN95: Cardinal; //call InitShellStateSizes; before
{$EXTERNALSYM SHELLSTATE_SIZE_NT4}
  SHELLSTATE_SIZE_NT4: Cardinal;   //call InitShellStateSizes; before
{$EXTERNALSYM SHELLSTATE_SIZE_IE4}
  SHELLSTATE_SIZE_IE4: Cardinal;  //call InitShellStateSizes; before
{$EXTERNALSYM SHELLSTATE_SIZE_WIN2K}
  SHELLSTATE_SIZE_WIN2K: Cardinal; //call InitShellStateSizes; before

{$EXTERNALSYM SHGetSetSettings}
procedure SHGetSetSettings(var lpss: TShellState; dwMask: DWORD;
  bSet: BOOL); stdcall;

//
//  SysFiles are these windows special files:
//      "dll sys vxd 386 drv"
//
//  hidden files are files with the FILE_ATTRIBUTE_HIDDEN attribute
//
//  system files are files with the FILE_ATTRIBUTE_SYSTEM attribute
//
//      fShowAllObjects fShowSysFiles   Result
//      --------------- -------------   ------
//      0               0               hide hidden + SysFiles + system files
//      0               1               hide hidden files.
//      1               0               show all files.
//      1               1               show all files.
//
type
  PShellFlagState = ^TShellFlagState;
  {$EXTERNALSYM SHELLFLAGSTATE}
  SHELLFLAGSTATE = record
    Data: DWORD; // C compiler says 4 bytes, so DWORD, not Word.
{
    BOOL fShowAllObjects : 1;
    BOOL fShowExtensions : 1;
    BOOL fNoConfirmRecycle : 1;
    BOOL fShowSysFiles : 1;
    BOOL fShowCompColor : 1;
    BOOL fDoubleClickInWebView : 1;
    BOOL fDesktopHTML : 1;
    BOOL fWin95Classic : 1;
    BOOL fDontPrettyPath : 1;
    BOOL fShowAttribCol : 1;
    BOOL fMapNetDrvBtn : 1;
    BOOL fShowInfoTip : 1;
    BOOL fHideIcons : 1;
    UINT fRestFlags : 3;
}
  end;
  TShellFlagState = SHELLFLAGSTATE;

const
  {$EXTERNALSYM SSF_SHOWALLOBJECTS}
  SSF_SHOWALLOBJECTS          = $00000001;
  {$EXTERNALSYM SSF_SHOWEXTENSIONS}
  SSF_SHOWEXTENSIONS          = $00000002;
  {$EXTERNALSYM SSF_HIDDENFILEEXTS}
  SSF_HIDDENFILEEXTS          = $00000004;
  {$EXTERNALSYM SSF_SERVERADMINUI}
  SSF_SERVERADMINUI           = $00000004;
  {$EXTERNALSYM SSF_SHOWCOMPCOLOR}
  SSF_SHOWCOMPCOLOR           = $00000008;
  {$EXTERNALSYM SSF_SORTCOLUMNS}
  SSF_SORTCOLUMNS             = $00000010;
  {$EXTERNALSYM SSF_SHOWSYSFILES}
  SSF_SHOWSYSFILES            = $00000020;
  {$EXTERNALSYM SSF_DOUBLECLICKINWEBVIEW}
  SSF_DOUBLECLICKINWEBVIEW    = $00000080;
  {$EXTERNALSYM SSF_SHOWATTRIBCOL}
  SSF_SHOWATTRIBCOL           = $00000100;
  {$EXTERNALSYM SSF_DESKTOPHTML}
  SSF_DESKTOPHTML             = $00000200;
  {$EXTERNALSYM SSF_WIN95CLASSIC}
  SSF_WIN95CLASSIC            = $00000400;
  {$EXTERNALSYM SSF_DONTPRETTYPATH}
  SSF_DONTPRETTYPATH          = $00000800;
  {$EXTERNALSYM SSF_SHOWINFOTIP}
  SSF_SHOWINFOTIP             = $00002000;
  {$EXTERNALSYM SSF_MAPNETDRVBUTTON}
  SSF_MAPNETDRVBUTTON         = $00001000;
  {$EXTERNALSYM SSF_NOCONFIRMRECYCLE}
  SSF_NOCONFIRMRECYCLE        = $00008000;
  {$EXTERNALSYM SSF_HIDEICONS}
  SSF_HIDEICONS               = $00004000;
  {$EXTERNALSYM SSF_FILTER}
  SSF_FILTER                  = $00010000;
  {$EXTERNALSYM SSF_WEBVIEW}
  SSF_WEBVIEW                 = $00020000;
  {$EXTERNALSYM SSF_SHOWSUPERHIDDEN}
  SSF_SHOWSUPERHIDDEN         = $00040000;
  {$EXTERNALSYM SSF_SEPPROCESS}
  SSF_SEPPROCESS              = $00080000;
  {$EXTERNALSYM SSF_NONETCRAWLING}
  SSF_NONETCRAWLING           = $00100000;
  {$EXTERNALSYM SSF_STARTPANELON}
  SSF_STARTPANELON            = $00200000;
  {$EXTERNALSYM SSF_SHOWSTARTPAGE}
  SSF_SHOWSTARTPAGE           = $00400000;

// SHGetSettings(LPSHELLFLAGSTATE lpss, DWORD dwMask)
//
// Specify the bits you are interested in in dwMask and they will be
// filled out in the lpss structure.
//
// When these settings change, a WM_SETTINGCHANGE message is sent
// with the string lParam value of "ShellState".
//
{$EXTERNALSYM SHGetSettings}
procedure SHGetSettings(out lpsfs: TShellFlagState; dwMask: DWORD); stdcall;

// SHBindToParent(PItemIDList pidl, REFIID riid, void **ppv, PItemIDList *ppidlLast)
//
// Given a pidl, you can get an interface pointer (as specified by riid) of the pidl's parent folder (in ppv)
// If ppidlLast is non-NULL, you can also get the pidl of the last item.
//
{$EXTERNALSYM SHBindToParent}
function SHBindToParent(pidl: PItemIDList; const riid: TIID;
  out ppv, ppidlLast: PPItemIDList): HResult; stdcall;

// SHSTDAPI SHParseDisplayName(PCWSTR pszName, IBindCtx *pbc, PItemIDList *ppidl, SFGAOF sfgaoIn, SFGAOF *psfgaoOut)
//
//  given a string it will call psfDesktop->ParseDisplayName() to try and create a pidl
//  if no pbc specified, it uses the preferred options for parsing.
//  this includes mapping file system paths to their appropriate aliased location (RegisterObjectParam(STR_PARSE_TRANSLATE_ALIASES))
//  psfgaoOut is optional for SFGAO attributes
//
{$EXTERNALSYM SHParseDisplayName}
function SHParseDisplayName(pszName: PWideChar; pbc: IBindCtx;
  out ppidl: PItemIDList; sfgaoIn: TSFGAOF;
  out psfgaoOut: TSFGAOF): HResult; stdcall;

// SHPathPrepareForWrite(HWND hwnd, IUnknown *punkEnableModless, LPCTSTR pszPath, DWORD dwFlags)
//
// DESCRIPTION:
//     This API will prepare the path for the caller.  This includes:
// 1. Prompting for the ejectable media to be re-inserted. (Floppy, CD-ROM, ZIP drive, etc.)
// 2. Prompting for the media to be formatted. (Floppy, hard drive, etc.)
// 3. Remount mapped drives if the connection was lost. (\\unc\share mapped to N: becomes disconnected)
// 4. If the path doesn't exist, create it.  (SHPPFW_DIRCREATE and SHPPFW_ASKDIRCREATE)
// 5. Display an error if the media is read only. (SHPPFW_NOWRITECHECK not set)
//
// PARAMETERS:
//      hwnd: Parernt window for UI.  NULL means don't display UI. OPTIONAL
//      punkEnableModless: Parent that will be set to modal during UI using IOleInPlaceActiveObject::EnableModeless(). OPTIONAL
//      pszPath: Path to verify is valid for writting.  This can be a UNC or file drive path.  The path
//               should only contain directories.  Pass SHPPFW_IGNOREFILENAME if the last path segment
//               is always filename to ignore.
//      dwFlags: SHPPFW_* Flags to modify behavior
//
//-------------------------------------------------------------------------
const
  {$EXTERNALSYM SHPPFW_NONE}
  SHPPFW_NONE             = $00000000;
  {$EXTERNALSYM SHPPFW_DIRCREATE}
  SHPPFW_DIRCREATE        = $00000001;              // Create the directory if it doesn't exist without asking the user.
  {$EXTERNALSYM SHPPFW_ASKDIRCREATE}
  SHPPFW_ASKDIRCREATE     = $00000002;              // Create the directory if it doesn't exist after asking the user.
  {$EXTERNALSYM SHPPFW_IGNOREFILENAME}
  SHPPFW_IGNOREFILENAME   = $00000004;              // Ignore the last item in pszPath because it's a file.  Example: pszPath="C:\DirA\DirB", only use "C:\DirA".
  {$EXTERNALSYM SHPPFW_NOWRITECHECK}
  SHPPFW_NOWRITECHECK     = $00000008;              // Caller only needs to read from the drive, so don't check if it's READ ONLY.
  {$EXTERNALSYM SHPPFW_MEDIACHECKONLY}
  SHPPFW_MEDIACHECKONLY   = $00000010;              // do the retrys on the media (or net path), return errors if the file can't be found
  {$EXTERNALSYM SHPPFW_DEFAULT}
  SHPPFW_DEFAULT          = SHPPFW_DIRCREATE;       // May change

{$EXTERNALSYM SHPathPrepareForWriteA}
function SHPathPrepareForWriteA(hwnd: HWND; punkEnableModless: IUnknown;
  pszPath: PAnsiChar; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM SHPathPrepareForWriteW}
function SHPathPrepareForWriteW(hwnd: HWND; punkEnableModless: IUnknown;
  pszPath: PWideChar; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM SHPathPrepareForWrite}
function SHPathPrepareForWrite(hwnd: HWND; punkEnableModless: IUnknown;
  pszPath: PTSTR; dwFlags: DWORD): HResult; stdcall;

//--------------------------------------------------------------------------
//
// Interface used for exposing the INI file methods on a shortcut file
//
//
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM INamedPropertyBag}
  INamedPropertyBag = interface(IUnknown)
  ['{FB700430-952C-11D1-946F-000000000000}']
    function ReadPropertyNPB(pszBagname, pszPropName: POleStr;
      var pVar: TPropVariant): HResult; stdcall;
    function WritePropertyNPB(pszBagname, pszPropName: POleStr;
      var pVar: TPropVariant): HResult; stdcall;
    function RemovePropertyNPB(
      pszBagname, pszPropName: POleStr): HResult; stdcall;
  end;

//    NOTE: urlmon.h must be included before shlobj.h to access thisfunction.
//
//    SoftwareUpdateMessageBox
//
//    Provides a standard message box for the alerting the user that a software
//    update is available or installed. No UI will be displayed if there is no
//    update available or if the available update version is less than or equal
//    to the Advertised update version.
//
//    hWnd                - [in] Handle of owner window
//    szDistUnit          - [in] Unique identifier string for a code distribution unit. For
//                               ActiveX controls and Active Setup installed components, this
//                               is typically a GUID string.
//    dwFlags             - [in] Must be 0.
//    psdi                - [in,out] Pointer to SOFTDISTINFO (see URLMon.h ). May be NULL.
//                                cbSize should be initialized
//                                by the caller to sizeof(SOFTDISTINFO), dwReserved should be set to 0.
//
//    RETURNS:
//
//    IDNO     - The user chose cancel. If *pbRemind is FALSE, the caller should save the
//               update version from the SOFTDISTINFO and pass it in as the Advertised
//               version in future calls.
//
//    IDYES    - The user has selected Update Now/About Update. The caller should navigate to
//               the SOFTDISTINFO's pszHREF to initiate the install or learn about it.
//               The caller should save the update version from the SOFTDISTINFO and pass
//               it in as the Advertised version in future calls.
//
//    IDIGNORE - There is no pending software update. Note: There is
//               no Ignore button in the standard UI. This occurs if the available
//               version is less than the installed version or is not present or if the
//               Advertised version is greater than or equal to the update version.
//
//    IDABORT  - An error occured. Call GetSoftwareUpdateInfo() for a more specific HRESULT.
//               Note: There is no Abort button in the standard UI.


// SHDOCAPI_(DWORD)
function SoftwareUpdateMessageBox(hWnd: HWND; szDistUnit: PWideChar;
  dwFlags: DWORD; out psdi: TSoftDistInfo): DWORD; stdcall;

//  SHPropStgCreate()
//  Wrap of IPropertySetStorage::Open/Create
//
//  Thisfunction ensures proper handling of code page retrieval/assignment
//  for the requested property set operation.
//
//  psstg,          //  Address of IPropertySetStorage vtable
//  fmtid,          //  property set ID
//  pclsid,         //  class ID associated with the set. This can be NULL
//  grfFlags,       //  PROPSETFLAG_xxx.  All sets containing ansi bytes should be created with
                    //  PROPSETFLAG_ANSI, otherwise PROPSETFLAG_DEFAULT.
//  grfMode,        //  STGM_ flags.  Must contain STGM_DIRECT|STGM_EXCLUSIVE.
//  dwDisposition,  //  OPEN_EXISTING. OPEN_ALWAYS, CREATE_NEW, or CREATE_ALWAYS
//  IPropertyStorage** ppstg,  // Address to receive requested vtable
//  puCodePage      //  Optional address to receive the code page ID for the set.
//
{$EXTERNALSYM SHPropStgCreate}
function SHPropStgCreate(psstg: IPropertySetStorage; var fmtid: TFmtID;
  pclsid: PCLSID; grfFlags: DWORD; grfMode: DWORD; dwDisposition: DWORD;
  out ppstg: IPropertyStorage; puCodePage: PUINT): HResult; stdcall;

//  SHPropStgReadMultiple()
//  IPropertyStorage::ReadMultiple wrap
//
//  The wrap ensures ANSI/UNICODE translations are handled properly for
//  legacy property sets.
//
//  pps,       // address of IPropertyStorage vtable.
//  uCodePage, //Code page value retrieved from SHCreatePropertySet
//  cpspec,    //Count of properties being read
//  rgpspec,   //Array of the properties to be read
//  rgvar      //Array of PROPVARIANTs containing the property values on return
//

type
  TPropSpecArray = array[0..65535] of TPropSpec;
  PPropSpecArray = ^TPropSpecArray;
  TPropVariantArray = array[0..65535] of TPropVariant;
  PPropVariantArray = ^TPropVariantArray;

{$EXTERNALSYM SHPropStgReadMultiple}
function SHPropStgReadMultiple(pps: IPropertyStorage; uCodePage: UINT;
  cpspec: ULONG; rgpspec: PPropSpecArray;
  rgvar: PPropVariantArray): HResult; stdcall;

//  SHPropStgWriteMultiple()
//  IPropertyStorage::WriteMultiple wrap
//
//  The wrap ensures ANSI/UNICODE translations are handled properly for
//  legacy property sets.
//
//  pps,       // address of IPropertyStorage vtable.
//  uCodePage, // code page retrieved from SHCreatePropertySet.
//  cpspec,    // The number of properties being set
//  rgpspec,   // Property specifiers
//  rgvar,     // Array of TPropVariant values
//  propidNameFirst // Minimum value for property identifiers. This value should be >= PID_FIRST_USABLE
//
{$EXTERNALSYM SHPropStgWriteMultiple}
function SHPropStgWriteMultiple(pps: IPropertyStorage; out puCodePage: UINT;
  cpspec: ULONG; rgpspec: PPropSpecArray; rgvar: PPropVariantArray;
  propidNameFirst: TPropID): HResult; stdcall;

  {$EXTERNALSYM SHCreateFileExtractIconA}
function SHCreateFileExtractIconA(pszFile: PAnsiChar; dwFileAttributes: DWORD;
    const riid: TIID; out ppv): HResult; stdcall;
  {$EXTERNALSYM SHCreateFileExtractIconW}
function SHCreateFileExtractIconW(pszFile: PWideChar; dwFileAttributes: DWORD;
    const riid: TIID; out ppv): HResult; stdcall;
  {$EXTERNALSYM SHCreateFileExtractIcon}
function SHCreateFileExtractIcon(pszFile: PTSTR; dwFileAttributes: DWORD;
    const riid: TIID; out ppv): HResult; stdcall;
    
{$EXTERNALSYM SHLimitInputEdit}
function SHLimitInputEdit(hwndEdit: HWND; psf: IShellFolder): HResult; stdcall;

//
// The SHMultiFileProperties API displays a property sheet for a
// set of files specified in an IDList Array.
//
// Parameters:
//      pdtobj  - Data object containing list of files.  The data
//                object must provide the "Shell IDList Array"
//                clipboard format.  The parent folder's implementation of
//                IShellFolder::GetDisplayNameOf must return a fully-qualified
//                filesystem path for each item in response to the
//                SHGDN_FORPARSING flag.
//
//      dwFlags - Reserved for future use.  Should be set to 0.
//
// Returns:
//      S_OK
//
{$EXTERNALSYM SHMultiFileProperties}
function SHMultiFileProperties(pdtobj: IDataObject;
  dwFlags: DWORD): HResult; stdcall;

type
  {$EXTERNALSYM PFNASYNCICONTASKBALLBACK}
  PFNASYNCICONTASKBALLBACK = procedure(pidl: PItemIDList;
    pvData, pvHint: Pointer; iIconIndex, iOpenIconIndex: Integer) stdcall;
  TFnAsyncIconTaskCallback = PFNASYNCICONTASKBALLBACK;

// HRESULT SHMapIDListToImageListIndexAsync(IShellTaskScheduler* pts, IShellFolder *psf, PItemIDList pidl, UINT flags,
//                                            PFNASYNCICONTASKBALLBACK pfn, Pointer pvData, Pointer pvHint, int *piIndex, int *piIndexSel);
// A usefullfunction for asynchronously mapping idlist into index into system
// image list.  Optionally it can also look up the index of the selected icon.
// pts          Task scheduler interface to use to create the background task
// psf          Shell folder relating to the pidl
// pidl         Item whose icon is requested
// flags        GIL_ flags
// pfn         function called back when the background task is done
// pvData       User data passed back in the (*pfn) callback
// pvHint       User data passed back in the (*pfn) callback
// piIndex      Icon index returned. This is the temporary index if thefunction returns E_PENDING. The final index will be provided thru the callback
// piIndexSel   Optional icon index for the open icon case (GIL_OPENICON).
//
// Returns S_OK if all the requested info was available. E_PENDING means that you get temporary icons, and will be called back
//              asynchronously with the final icons. Other failure code means thefunction failed.
  {$EXTERNALSYM SHMapIDListToImageListIndexAsync}
function SHMapIDListToImageListIndexAsync(pts: IShellTaskScheduler;
    psf: IShellFolder; pidl: PItemIDList; flags: UINT;
    pfn: TFNAsyncIconTaskCallback; pvData, pvHint: Pointer;
    out piIndex, piIndexSel: Integer): HResult stdcall;

// A useful function in Defview for mapping idlist into index into system
// image list.  Optionally it can also look up the index of the selected
// icon.
{$EXTERNALSYM SHMapPIDLToSystemImageListIndex}
function SHMapPIDLToSystemImageListIndex(pshf: IShellFolder;
  pidl: PItemIDList; out piIndexSel: Integer): Integer; stdcall;

// STDAPI
{$EXTERNALSYM SHCLSIDFromString}
function SHCLSIDFromString(lpsz: PWideChar;
  out pclsid: TCLSID): HResult; stdcall;

  {$EXTERNALSYM SHFlushClipboard}
function SHFlushClipboard: HResult; stdcall;

{$EXTERNALSYM SHCreateQueryCancelAutoPlayMoniker}
function SHCreateQueryCancelAutoPlayMoniker(
  out ppmoniker: IMoniker): HResult; stdcall;


  {$EXTERNALSYM SHGetShellStyleHInstance}
function SHGetShellStyleHInstance: THandle stdcall;

{$EXTERNALSYM PerUserInit}
procedure PerUserInit; stdcall;


  {$EXTERNALSYM SHRunControlPanel}
function SHRunControlPanel(lpcszCmdLine: PWideChar;
    hwndMsgParent: HWND): BOOL stdcall;

{$EXTERNALSYM PickIconDlg}
function PickIconDlg(hwnd: HWND; pszIconPath: PWideChar; cbIconPath: UINT;
  out piIconIndex: Integer): Integer; stdcall;

type
  PAAShellMenuFileName = ^TAAShellMenuFileName;
  {$EXTERNALSYM tagAAMENUFILENAME}
  tagAAMENUFILENAME = record
    cbTotal: Smallint;
    rgbReserved: array[0..11] of Byte;
    szFileName: array[0..0] of WideChar;  // variable length string
  end;
  {$EXTERNALSYM AASHELLMENUFILENAME}
  AASHELLMENUFILENAME = tagAAMENUFILENAME;
  TAAShellMenuFileName = tagAAMENUFILENAME;

  PAAShellMenuItem = ^TAAShellMenuItem;
  {$EXTERNALSYM tagAASHELLMENUITEM}
  tagAASHELLMENUITEM = record
    lpReserved1: Pointer;
    iReserved: Integer;
    uiReserved: UINT;
    lpName: PAAShellMenuFileName; // name of file
    psz: PWideChar;               // text to use if no file
  end;
  {$EXTERNALSYM AASHELLMENUITEM}
  AASHELLMENUITEM = tagAASHELLMENUITEM;
  TAAShellMenuItem = tagAASHELLMENUITEM;

{$EXTERNALSYM SHGetAttributesFromDataObject}
function SHGetAttributesFromDataObject(pdo: IDataObject;
  dwAttributeMask: DWORD; out pdwAttributes: DWORD;
  out pcItems: UINT): HResult; stdcall;

// SHDOCAPI
{$EXTERNALSYM ImportPrivacySettings}
function ImportPrivacySettings(szFilename: PWideChar;
  var pfParsePrivacyPreferences, pfParsePerSiteRules: BOOL): BOOL; stdcall;
// defined in MSHTML.h:
{$HPPEMIT '#ifndef IEnumPrivacyRecords'}
{$HPPEMIT 'typedef interface IEnumPrivacyRecords IEnumPrivacyRecords;'}
{$HPPEMIT '#endif'}
{$EXTERNALSYM DoPrivacyDlg}
//function DoPrivacyDlg(hwndParent: HWND; pszUrl: POleStr;
//  pPrivacyEnum: IEnumPrivacyRecords; fReportAllSites: BOOL): HResult; stdcall;
function DoPrivacyDlg(hwndParent: HWND; pszUrl: POleStr;
  const pPrivacyEnum; fReportAllSites: BOOL): HResult; stdcall;

procedure InitShellStateSizes;

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


// Macros

function ShellFolderView_ReArrange(hwnd: HWND; lparam: LPARAM): BOOL;
begin
  Result := BOOL(SHShellFolderView_Message(hwnd, SFVM_REARRANGE, lparam));
end;

function ShellFolderView_AddObject(hwnd: HWND; pidl: PItemIDList): LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(hwnd,
    SFVM_ADDOBJECT, LPARAM(pidl)));
end;

function ShellFolderView_RemoveObject(hwnd: HWND; pidl: PItemIDList): LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(hwnd,
    SFVM_REMOVEOBJECT, LPARAM(pidl)));
end;

function ShellFolderView_UpdateObject(hwnd: HWND;
  ppidl: PPItemIDListArray): LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(hwnd,
    SFVM_UPDATEOBJECT, LPARAM(ppidl)));
end;

function ShellFolderView_GetSelectedObjects(hwnd: HWND;
  ppidl: PPItemIDListArray): LPARAM;
begin
  Result := LPARAM(SHShellFolderView_Message(hwnd,
    SFVM_GETSELECTEDOBJECTS, LPARAM(ppidl)));
end;

procedure ShellFolderView_SetItemPos(hwnd: HWND; pidl: PItemIDList;
  x, y: Integer);
var
  sip: TSFVSetItemPos;
begin
  sip.pidl := pidl;
  sip.pt.X := x;
  sip.pt.Y := y;
  SHShellFolderView_Message(hwnd, SFVM_SETITEMPOS, LPARAM(@sip));
end;

procedure ShellFolderView_SetClipboard(hwnd: HWND; dwEffect: DWORD);
begin
  SHShellFolderView_Message(hwnd, SFVM_SETCLIPBOARD, LPARAM(dwEffect));
end;

procedure ShellFolderView_SetPoints(hwnd: HWND; const pdtobj: IDataObject);
begin
  SHShellFolderView_Message(hwnd, SFVM_SETPOINTS, LPARAM(pdtobj));
end;

{$IFNDEF DYNAMIC_LINK}

function SHGetMalloc; external shell32 name 'SHGetMalloc';
function SHAlloc; external shell32 name 'SHAlloc';
procedure SHFree; external shell32 name 'SHFree';
function SHGetIconOverlayIndexA; external shell32 name 'SHGetIconOverlayIndexA';
function SHGetIconOverlayIndexW; external shell32 name 'SHGetIconOverlayIndexW';
function SHGetIconOverlayIndex; external shell32 name 'SHGetIconOverlayIndex'+AWSuffix;
function SHGetPathFromIDListA; external shell32 name 'SHGetPathFromIDListA';
function SHGetPathFromIDListW; external shell32 name 'SHGetPathFromIDListW';
function SHGetPathFromIDList; external shell32 name 'SHGetPathFromIDList'+AWSuffix;
function SHCreateDirectory; external shell32 name 'SHCreateDirectory';
function SHCreateDirectoryExA; external shell32 name 'SHCreateDirectoryExA';
function SHCreateDirectoryExW; external shell32 name 'SHCreateDirectoryExW';
function SHCreateDirectoryEx; external shell32 name 'SHCreateDirectoryEx'+AWSuffix;
function SHOpenFolderAndSelectItems; external shell32 name 'SHOpenFolderAndSelectItems';
function SHCreateShellItem; external shell32 name 'SHCreateShellItem';
function SHGetSpecialFolderLocation; external shell32 name 'SHGetSpecialFolderLocation';
procedure SHFlushSFCache; external shell32 name 'SHFlushSFCache';
function SHCloneSpecialIDList; external shell32 name 'SHCloneSpecialIDList';
function SHGetSpecialFolderPathA; external shell32 name 'SHGetSpecialFolderPathA';
function SHGetSpecialFolderPathW; external shell32 name 'SHGetSpecialFolderPathW';
function SHGetSpecialFolderPath; external shell32 name 'SHGetSpecialFolderPath'+AWSuffix;
{$IFNDEF JWA_INCLUDEMODE}
function SHGetFolderPathA; external shell32 name 'SHGetFolderPathA';
function SHGetFolderPathW; external shell32 name 'SHGetFolderPathW';
function SHGetFolderPath; external shell32 name 'SHGetFolderPath'+AWSuffix;
{$ENDIF JWA_INCLUDEMODE}
function SHGetFolderLocation; external shell32 name 'SHGetFolderLocation';
function SHGetFolderPathAndSubDirA; external shell32 name 'SHGetFolderPathAndSubDirA';
function SHGetFolderPathAndSubDirW; external shell32 name 'SHGetFolderPathAndSubDirW';
function SHGetFolderPathAndSubDir; external shell32 name 'SHGetFolderPathAndSubDir'+AWSuffix;
function SHBrowseForFolderA; external shell32 name 'SHBrowseForFolderA';
function SHBrowseForFolderW; external shell32 name 'SHBrowseForFolderW';
function SHBrowseForFolder; external shell32 name 'SHBrowseForFolder'+AWSuffix;
function SHLoadInProc; external shell32 name 'SHLoadInProc';
function SHGetDesktopFolder; external shell32 name 'SHGetDesktopFolder';
procedure SHChangeNotify; external shell32 name 'SHChangeNotify';
procedure SHAddToRecentDocs; external shell32 name 'SHAddToRecentDocs';
function SHHandleUpdateImage; external shell32 name 'SHHandleUpdateImage';
procedure SHUpdateImageA; external shell32 name 'SHUpdateImageA';
procedure SHUpdateImageW; external shell32 name 'SHUpdateImageW';
procedure SHUpdateImage; external shell32 name 'SHUpdateImage'+AWSuffix;
function SHChangeNotifyRegister; external shell32 name 'SHChangeNotifyRegister';
function SHChangeNotifyDeregister; external shell32 name 'SHChangeNotifyDeregister';
function SHChangeNotification_Lock; external shell32 name 'SHChangeNotification_Lock';
function SHChangeNotification_Unlock; external shell32 name 'SHChangeNotification_Unlock';
function SHGetRealIDL; external shell32 name 'SHGetRealIDL';
function SHGetInstanceExplorer; external shell32 name 'SHGetInstanceExplorer';
function SHGetDataFromIDListA; external shell32 name 'SHGetDataFromIDListA';
function SHGetDataFromIDListW; external shell32 name 'SHGetDataFromIDListW';
function SHGetDataFromIDList; external shell32 name 'SHGetDataFromIDList'+AWSuffix;
function RestartDialog; external shell32 name 'RestartDialog';
function RestartDialogEx; external shell32 name 'RestartDialogEx';
function SHCoCreateInstance; external shell32 name 'SHCoCreateInstance';
function SHCreateStdEnumFmtEtc; external shell32 name 'SHCreateStdEnumFmtEtc';
function SHDoDragDrop; external shell32 name 'SHDoDragDrop';
function DAD_SetDragImage; external shell32 name 'DAD_SetDragImage';
function DAD_DragEnterEx; external shell32 name 'DAD_DragEnterEx';
function DAD_DragEnterEx2; external shell32 name 'DAD_DragEnterEx2';
function DAD_ShowDragImage; external shell32 name 'DAD_ShowDragImage';
function DAD_DragMove; external shell32 name 'DAD_DragMove';
function DAD_DragLeave; external shell32 name 'DAD_DragLeave';
function DAD_AutoScroll; external shell32 name 'DAD_AutoScroll';
function ReadCabinetState; external shell32 name 'ReadCabinetState';
function WriteCabinetState; external shell32 name 'WriteCabinetState';
function PathMakeUniqueName; external shell32 name 'PathMakeUniqueName';
procedure PathQualify; external shell32 name 'PathQualify';
function PathIsExe; external shell32 name 'PathIsExe';
function PathIsSlowA; external shell32 name 'PathIsSlowA';
function PathIsSlowW; external shell32 name 'PathIsSlowW';
function PathIsSlow; external shell32 name 'PathIsSlow'+AWSuffix;
function PathCleanupSpec; external shell32 name 'PathCleanupSpec';
function PathResolve; external shell32 name 'PathResolve';
function GetFileNameFromBrowse; external shell32 name 'GetFileNameFromBrowse';
function DriveType; external shell32 name 'DriveType';
function RealDriveType; external shell32 name 'RealDriveType';
function IsNetDrive; external shell32 name 'IsNetDrive';
function Shell_MergeMenus; external shell32 name 'Shell_MergeMenus';
function SHObjectProperties; external shell32 name 'SHObjectProperties';
function SHFormatDrive; external shell32 name 'SHFormatDrive';
function SHCreatePropSheetExtArray; external shell32 name 'SHCreatePropSheetExtArray';
procedure SHDestroyPropSheetExtArray; external shell32 name 'SHDestroyPropSheetExtArray';
function SHAddFromPropSheetExtArray; external shell32 name 'SHAddFromPropSheetExtArray';
function SHReplaceFromPropSheetExtArray; external shell32 name 'SHReplaceFromPropSheetExtArray';
function ILClone; external shell32 name 'ILClone';
function ILGetNext; external shell32 name 'ILGetNext';
function ILGetSize; external shell32 name 'ILGetSize';
function ILFindLastID; external shell32 name 'ILFindLastID';
function ILRemoveLastID; external shell32 name 'ILRemoveLastID';
function ILAppendID; external shell32 name 'ILAppendID';
procedure ILFree; external shell32 name 'ILFree';
function ILCloneFirst; external shell32 name 'ILCloneFirst';
function ILIsEqual; external shell32 name 'ILIsEqual';
function ILIsParent; external shell32 name 'ILIsParent';
function ILFindChild; external shell32 name 'ILFindChild';
function ILCombine; external shell32 name 'ILCombine';
function ILSaveToStream; external shell32 name 'ILSaveToStream';
function ILCreateFromPathA; external shell32 name 'ILCreateFromPathA';
function ILCreateFromPathW; external shell32 name 'ILCreateFromPathW';
function ILCreateFromPath; external shell32 name 'ILCreateFromPath'+AWSuffix;
function SHILCreateFromPath; external shell32 name 'SHILCreateFromPath';
function OpenRegStream; external shell32 name 'OpenRegStream';
function SHFindFiles; external shell32 name 'SHFindFiles';
procedure PathGetShortPath; external shell32 name 'PathGetShortPath';
function PathYetAnotherMakeUniqueName; external shell32 name 'PathYetAnotherMakeUniqueName';
function Win32DeleteFile; external shell32 name 'Win32DeleteFile';
function SHRestricted; external shell32 name 'SHRestricted';
function SignalFileOpen; external shell32 name 'SignalFileOpen';
function SHSimpleIDListFromPath; external shell32 name 'SHSimpleIDListFromPath';
function SHDefExtractIconA; external shell32 name 'SHDefExtractIconA';
function SHDefExtractIconW; external shell32 name 'SHDefExtractIconW';
function SHDefExtractIcon; external shell32 name 'SHDefExtractIcon'+AWSuffix;
function Shell_GetImageLists; external shell32 name 'Shell_GetImageLists';
function Shell_GetCachedImageIndex; external shell32 name 'Shell_GetCachedImageIndex';
function SHValidateUNC; external shell32 name 'SHValidateUNC';
function PifMgr_OpenProperties; external shell32 name 'PifMgr_OpenProperties';
function PifMgr_GetProperties; external shell32 name 'PifMgr_GetProperties';
function PifMgr_SetProperties; external shell32 name 'PifMgr_SetProperties';
function PifMgr_CloseProperties; external shell32 name 'PifMgr_CloseProperties';
procedure SHSetInstanceExplorer; external shell32 name 'SHSetInstanceExplorer';
function IsUserAnAdmin; external shell32 name 'IsUserAnAdmin';
function SHShellFolderView_Message; external shell32 name 'SHShellFolderView_Message';
function SHCreateShellFolderView; external shell32 name 'SHCreateShellFolderView';
function CDefFolderMenu_Create2; external shell32 name 'CDefFolderMenu_Create2';
function SHFind_InitMenuPopup; external shell32 name 'SHFind_InitMenuPopup';
function SHCreateShellFolderViewEx; external shell32 name 'SHCreateShellFolderViewEx';
procedure SHGetSetSettings; external shell32 name 'SHGetSetSettings';
procedure SHGetSettings; external shell32 name 'SHGetSettings';
function SHBindToParent; external shell32 name 'SHBindToParent';
function SHParseDisplayName; external shell32 name 'SHParseDisplayName';
function SHPathPrepareForWriteA; external shell32 name 'SHPathPrepareForWriteA';
function SHPathPrepareForWriteW; external shell32 name 'SHPathPrepareForWriteW';
function SHPathPrepareForWrite; external shell32 name 'SHPathPrepareForWrite'+AWSuffix;
function SoftwareUpdateMessageBox; external shdocvwDll name 'SoftwareUpdateMessageBox';
function SHPropStgCreate; external shell32 name 'SHPropStgCreate';
function SHPropStgReadMultiple; external shell32 name 'SHPropStgReadMultiple';
function SHPropStgWriteMultiple; external shell32 name 'SHPropStgWriteMultiple';
function SHLimitInputEdit; external shell32 name 'SHLimitInputEdit';
function SHMultiFileProperties; external shell32 name 'SHMultiFileProperties';
function SHMapPIDLToSystemImageListIndex; external shell32 name 'SHMapPIDLToSystemImageListIndex';
function SHCLSIDFromString; external shell32 name 'SHCLSIDFromString';
function SHCreateQueryCancelAutoPlayMoniker; external shell32 name 'SHCreateQueryCancelAutoPlayMoniker';
function PickIconDlg; external shell32 name 'PickIconDlg';
function SHGetAttributesFromDataObject; external shell32 name 'SHGetAttributesFromDataObject';


function ImportPrivacySettings; external shdocvwDll name 'ImportPrivacySettings';
function DoPrivacyDlg; external shdocvwDll name 'DoPrivacyDlg';

function SHEnableServiceObject; external shdocvwDll name 'SHEnableServiceObject';
function SHGetSetFolderCustomSettingsA; external shdocvwDll name 'SHGetSetFolderCustomSettingsA';
function SHGetSetFolderCustomSettingsW; external shdocvwDll name 'SHGetSetFolderCustomSettingsW';
function SHGetSetFolderCustomSettings; external shdocvwDll name 'SHGetSetFolderCustomSettings'+AWSuffix;
function CallCPLEntry16; external shdocvwDll name 'CallCPLEntry16';
function SHStartNetConnectionDialogA; external shdocvwDll name 'SHStartNetConnectionDialogA';
function SHStartNetConnectionDialogW; external shdocvwDll name 'SHStartNetConnectionDialogW';
function SHStartNetConnectionDialog; external shdocvwDll name 'SHStartNetConnectionDialog'+AWSuffix;
function SHOpenPropSheetA; external shdocvwDll name 'SHOpenPropSheetA';
function SHOpenPropSheetW; external shdocvwDll name 'SHOpenPropSheetW';
function SHOpenPropSheet; external shdocvwDll name 'SHOpenPropSheet'+AWSuffix;
function SHCreateFileExtractIconA; external shdocvwDll name 'SHCreateFileExtractIconA';
function SHCreateFileExtractIconW; external shdocvwDll name 'SHCreateFileExtractIconW';
function SHCreateFileExtractIcon; external shdocvwDll name 'SHCreateFileExtractIcon'+AWSuffix;
function ILLoadFromStream; external shdocvwDll name 'ILLoadFromStream';
function PathProcessCommand; external shdocvwDll name 'PathProcessCommand';
function SHLoadOLE; external shdocvwDll name 'SHLoadOLE';
function SHMapIDListToImageListIndexAsync; external shdocvwDll name 'SHMapIDListToImageListIndexAsync';
function SHFlushClipboard; external shdocvwDll name 'SHFlushClipboard';
function SHGetShellStyleHInstance; external shdocvwDll name 'SHGetShellStyleHInstance';
function SHRunControlPanel; external shdocvwDll name 'SHRunControlPanel';


procedure PerUserInit; external mydocs name 'PerUserInit';

{$ELSE}

var
  _SHGetMalloc: Pointer;

function SHGetMalloc;
begin
  GetProcedureAddress(_SHGetMalloc, shell32, 'SHGetMalloc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetMalloc]
  end;
end;

var
  _SHAlloc: Pointer;

function SHAlloc;
begin
  GetProcedureAddress(_SHAlloc, shell32, 'SHAlloc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHAlloc]
  end;
end;

var
  _SHFree: Pointer;

procedure SHFree;
begin
  GetProcedureAddress(_SHFree, shell32, 'SHFree');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFree]
  end;
end;

var
  _SHGetIconOverlayIndexA: Pointer;

function SHGetIconOverlayIndexA;
begin
  GetProcedureAddress(_SHGetIconOverlayIndexA, shell32, 'SHGetIconOverlayIndexA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetIconOverlayIndexA]
  end;
end;

var
  _SHGetIconOverlayIndexW: Pointer;

function SHGetIconOverlayIndexW;
begin
  GetProcedureAddress(_SHGetIconOverlayIndexW, shell32, 'SHGetIconOverlayIndexW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetIconOverlayIndexW]
  end;
end;

var
  _SHGetIconOverlayIndex: Pointer;

function SHGetIconOverlayIndex;
begin
  GetProcedureAddress(_SHGetIconOverlayIndex, shell32, 'SHGetIconOverlayIndex');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetIconOverlayIndex]
  end;
end;

var
  _SHGetPathFromIDListA: Pointer;

function SHGetPathFromIDListA;
begin
  GetProcedureAddress(_SHGetPathFromIDListA, shell32, 'SHGetPathFromIDListA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetPathFromIDListA]
  end;
end;

var
  _SHGetPathFromIDListW: Pointer;

function SHGetPathFromIDListW;
begin
  GetProcedureAddress(_SHGetPathFromIDListW, shell32, 'SHGetPathFromIDListW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetPathFromIDListW]
  end;
end;

var
  _SHGetPathFromIDList: Pointer;

function SHGetPathFromIDList;
begin
  GetProcedureAddress(_SHGetPathFromIDList, shell32, 'SHGetPathFromIDList');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetPathFromIDList]
  end;
end;

var
  _SHCreateDirectory: Pointer;

function SHCreateDirectory;
begin
  GetProcedureAddress(_SHCreateDirectory, shell32, 'SHCreateDirectory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateDirectory]
  end;
end;

var
  _SHCreateDirectoryExA: Pointer;

function SHCreateDirectoryExA;
begin
  GetProcedureAddress(_SHCreateDirectoryExA, shell32, 'SHCreateDirectoryExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateDirectoryExA]
  end;
end;

var
  _SHCreateDirectoryExW: Pointer;

function SHCreateDirectoryExW;
begin
  GetProcedureAddress(_SHCreateDirectoryExW, shell32, 'SHCreateDirectoryExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateDirectoryExW]
  end;
end;

var
  _SHCreateDirectoryEx: Pointer;

function SHCreateDirectoryEx;
begin
  GetProcedureAddress(_SHCreateDirectoryEx, shell32, 'SHCreateDirectoryEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateDirectoryEx]
  end;
end;

var
  _SHOpenFolderAndSelectItems: Pointer;

function SHOpenFolderAndSelectItems;
begin
  GetProcedureAddress(_SHOpenFolderAndSelectItems, shell32, 'SHOpenFolderAndSelectItems');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenFolderAndSelectItems]
  end;
end;

var
  _SHCreateShellItem: Pointer;

function SHCreateShellItem;
begin
  GetProcedureAddress(_SHCreateShellItem, shell32, 'SHCreateShellItem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateShellItem]
  end;
end;

var
  _SHGetSpecialFolderLocation: Pointer;

function SHGetSpecialFolderLocation;
begin
  GetProcedureAddress(_SHGetSpecialFolderLocation, shell32, 'SHGetSpecialFolderLocation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSpecialFolderLocation]
  end;
end;

var
  _SHFlushSFCache: Pointer;

procedure SHFlushSFCache;
begin
  GetProcedureAddress(_SHFlushSFCache, shell32, 'SHFlushSFCache');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFlushSFCache]
  end;
end;

var
  _SHCloneSpecialIDList: Pointer;

function SHCloneSpecialIDList;
begin
  GetProcedureAddress(_SHCloneSpecialIDList, shell32, 'SHCloneSpecialIDList');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCloneSpecialIDList]
  end;
end;

var
  _SHGetSpecialFolderPathA: Pointer;

function SHGetSpecialFolderPathA;
begin
  GetProcedureAddress(_SHGetSpecialFolderPathA, shell32, 'SHGetSpecialFolderPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSpecialFolderPathA]
  end;
end;

var
  _SHGetSpecialFolderPathW: Pointer;

function SHGetSpecialFolderPathW;
begin
  GetProcedureAddress(_SHGetSpecialFolderPathW, shell32, 'SHGetSpecialFolderPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSpecialFolderPathW]
  end;
end;

var
  _SHGetSpecialFolderPath: Pointer;

function SHGetSpecialFolderPath;
begin
  GetProcedureAddress(_SHGetSpecialFolderPath, shell32, 'SHGetSpecialFolderPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSpecialFolderPath]
  end;
end;

{$IFNDEF JWA_INCLUDEMODE}
var
  _SHGetFolderPathA: Pointer;

function SHGetFolderPathA;
begin
  GetProcedureAddress(_SHGetFolderPathA, shell32, 'SHGetFolderPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathA]
  end;
end;

var
  _SHGetFolderPathW: Pointer;

function SHGetFolderPathW;
begin
  GetProcedureAddress(_SHGetFolderPathW, shell32, 'SHGetFolderPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathW]
  end;
end;

var
  _SHGetFolderPath: Pointer;

function SHGetFolderPath;
begin
  GetProcedureAddress(_SHGetFolderPath, shell32, 'SHGetFolderPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPath]
  end;
end;
{$ENDIF JWA_INCLUDEMODE}

var
  _SHGetFolderLocation: Pointer;

function SHGetFolderLocation;
begin
  GetProcedureAddress(_SHGetFolderLocation, shell32, 'SHGetFolderLocation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderLocation]
  end;
end;

var
  _SHGetFolderPathAndSubDirA: Pointer;

function SHGetFolderPathAndSubDirA;
begin
  GetProcedureAddress(_SHGetFolderPathAndSubDirA, shell32, 'SHGetFolderPathAndSubDirA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathAndSubDirA]
  end;
end;

var
  _SHGetFolderPathAndSubDirW: Pointer;

function SHGetFolderPathAndSubDirW;
begin
  GetProcedureAddress(_SHGetFolderPathAndSubDirW, shell32, 'SHGetFolderPathAndSubDirW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathAndSubDirW]
  end;
end;

var
  _SHGetFolderPathAndSubDir: Pointer;

function SHGetFolderPathAndSubDir;
begin
  GetProcedureAddress(_SHGetFolderPathAndSubDir, shell32, 'SHGetFolderPathAndSubDir');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathAndSubDir]
  end;
end;

var
  _SHBrowseForFolderA: Pointer;

function SHBrowseForFolderA;
begin
  GetProcedureAddress(_SHBrowseForFolderA, shell32, 'SHBrowseForFolderA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHBrowseForFolderA]
  end;
end;

var
  _SHBrowseForFolderW: Pointer;

function SHBrowseForFolderW;
begin
  GetProcedureAddress(_SHBrowseForFolderW, shell32, 'SHBrowseForFolderW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHBrowseForFolderW]
  end;
end;

var
  _SHBrowseForFolder: Pointer;

function SHBrowseForFolder;
begin
  GetProcedureAddress(_SHBrowseForFolder, shell32, 'SHBrowseForFolder');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHBrowseForFolder]
  end;
end;

var
  _SHLoadInProc: Pointer;

function SHLoadInProc;
begin
  GetProcedureAddress(_SHLoadInProc, shell32, 'SHLoadInProc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHLoadInProc]
  end;
end;

var
  _SHGetDesktopFolder: Pointer;

function SHGetDesktopFolder;
begin
  GetProcedureAddress(_SHGetDesktopFolder, shell32, 'SHGetDesktopFolder');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetDesktopFolder]
  end;
end;

var
  _SHChangeNotify: Pointer;

procedure SHChangeNotify;
begin
  GetProcedureAddress(_SHChangeNotify, shell32, 'SHChangeNotify');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHChangeNotify]
  end;
end;

var
  _SHAddToRecentDocs: Pointer;

procedure SHAddToRecentDocs;
begin
  GetProcedureAddress(_SHAddToRecentDocs, shell32, 'SHAddToRecentDocs');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHAddToRecentDocs]
  end;
end;

var
  _SHHandleUpdateImage: Pointer;

function SHHandleUpdateImage;
begin
  GetProcedureAddress(_SHHandleUpdateImage, shell32, 'SHHandleUpdateImage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHHandleUpdateImage]
  end;
end;

var
  _SHUpdateImageA: Pointer;

procedure SHUpdateImageA;
begin
  GetProcedureAddress(_SHUpdateImageA, shell32, 'SHUpdateImageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHUpdateImageA]
  end;
end;

var
  _SHUpdateImageW: Pointer;

procedure SHUpdateImageW;
begin
  GetProcedureAddress(_SHUpdateImageW, shell32, 'SHUpdateImageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHUpdateImageW]
  end;
end;

var
  _SHUpdateImage: Pointer;

procedure SHUpdateImage;
begin
  GetProcedureAddress(_SHUpdateImage, shell32, 'SHUpdateImage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHUpdateImage]
  end;
end;

var
  _SHChangeNotifyRegister: Pointer;

function SHChangeNotifyRegister;
begin
  GetProcedureAddress(_SHChangeNotifyRegister, shell32, 'SHChangeNotifyRegister');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHChangeNotifyRegister]
  end;
end;

var
  _SHChangeNotifyDeregister: Pointer;

function SHChangeNotifyDeregister;
begin
  GetProcedureAddress(_SHChangeNotifyDeregister, shell32, 'SHChangeNotifyDeregister');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHChangeNotifyDeregister]
  end;
end;

var
  _SHChangeNotification_Lock: Pointer;

function SHChangeNotification_Lock;
begin
  GetProcedureAddress(_SHChangeNotification_Lock, shell32, 'SHChangeNotification_Lock');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHChangeNotification_Lock]
  end;
end;

var
  _SHChangeNotification_Unlock: Pointer;

function SHChangeNotification_Unlock;
begin
  GetProcedureAddress(_SHChangeNotification_Unlock, shell32, 'SHChangeNotification_Unlock');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHChangeNotification_Unlock]
  end;
end;

var
  _SHGetRealIDL: Pointer;

function SHGetRealIDL;
begin
  GetProcedureAddress(_SHGetRealIDL, shell32, 'SHGetRealIDL');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetRealIDL]
  end;
end;

var
  _SHGetInstanceExplorer: Pointer;

function SHGetInstanceExplorer;
begin
  GetProcedureAddress(_SHGetInstanceExplorer, shell32, 'SHGetInstanceExplorer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetInstanceExplorer]
  end;
end;

var
  _SHGetDataFromIDListA: Pointer;

function SHGetDataFromIDListA;
begin
  GetProcedureAddress(_SHGetDataFromIDListA, shell32, 'SHGetDataFromIDListA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetDataFromIDListA]
  end;
end;

var
  _SHGetDataFromIDListW: Pointer;

function SHGetDataFromIDListW;
begin
  GetProcedureAddress(_SHGetDataFromIDListW, shell32, 'SHGetDataFromIDListW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetDataFromIDListW]
  end;
end;

var
  _SHGetDataFromIDList: Pointer;

function SHGetDataFromIDList;
begin
  GetProcedureAddress(_SHGetDataFromIDList, shell32, 'SHGetDataFromIDList');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetDataFromIDList]
  end;
end;

var
  _RestartDialog: Pointer;

function RestartDialog;
begin
  GetProcedureAddress(_RestartDialog, shell32, 'RestartDialog');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RestartDialog]
  end;
end;

var
  _RestartDialogEx: Pointer;

function RestartDialogEx;
begin
  GetProcedureAddress(_RestartDialogEx, shell32, 'RestartDialogEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RestartDialogEx]
  end;
end;

var
  _SHCoCreateInstance: Pointer;

function SHCoCreateInstance;
begin
  GetProcedureAddress(_SHCoCreateInstance, shell32, 'SHCoCreateInstance');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCoCreateInstance]
  end;
end;

var
  _SHCreateStdEnumFmtEtc: Pointer;

function SHCreateStdEnumFmtEtc;
begin
  GetProcedureAddress(_SHCreateStdEnumFmtEtc, shell32, 'SHCreateStdEnumFmtEtc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateStdEnumFmtEtc]
  end;
end;

var
  _SHDoDragDrop: Pointer;

function SHDoDragDrop;
begin
  GetProcedureAddress(_SHDoDragDrop, shell32, 'SHDoDragDrop');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDoDragDrop]
  end;
end;

var
  _DAD_SetDragImage: Pointer;

function DAD_SetDragImage;
begin
  GetProcedureAddress(_DAD_SetDragImage, shell32, 'DAD_SetDragImage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_SetDragImage]
  end;
end;

var
  _DAD_DragEnterEx: Pointer;

function DAD_DragEnterEx;
begin
  GetProcedureAddress(_DAD_DragEnterEx, shell32, 'DAD_DragEnterEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_DragEnterEx]
  end;
end;

var
  _DAD_DragEnterEx2: Pointer;

function DAD_DragEnterEx2;
begin
  GetProcedureAddress(_DAD_DragEnterEx2, shell32, 'DAD_DragEnterEx2');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_DragEnterEx2]
  end;
end;

var
  _DAD_ShowDragImage: Pointer;

function DAD_ShowDragImage;
begin
  GetProcedureAddress(_DAD_ShowDragImage, shell32, 'DAD_ShowDragImage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_ShowDragImage]
  end;
end;

var
  _DAD_DragMove: Pointer;

function DAD_DragMove;
begin
  GetProcedureAddress(_DAD_DragMove, shell32, 'DAD_DragMove');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_DragMove]
  end;
end;

var
  _DAD_DragLeave: Pointer;

function DAD_DragLeave;
begin
  GetProcedureAddress(_DAD_DragLeave, shell32, 'DAD_DragLeave');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_DragLeave]
  end;
end;

var
  _DAD_AutoScroll: Pointer;

function DAD_AutoScroll;
begin
  GetProcedureAddress(_DAD_AutoScroll, shell32, 'DAD_AutoScroll');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DAD_AutoScroll]
  end;
end;

var
  _ReadCabinetState: Pointer;

function ReadCabinetState;
begin
  GetProcedureAddress(_ReadCabinetState, shell32, 'ReadCabinetState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadCabinetState]
  end;
end;

var
  _WriteCabinetState: Pointer;

function WriteCabinetState;
begin
  GetProcedureAddress(_WriteCabinetState, shell32, 'WriteCabinetState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteCabinetState]
  end;
end;

var
  _PathMakeUniqueName: Pointer;

function PathMakeUniqueName;
begin
  GetProcedureAddress(_PathMakeUniqueName, shell32, 'PathMakeUniqueName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakeUniqueName]
  end;
end;

var
  _PathQualify: Pointer;

procedure PathQualify;
begin
  GetProcedureAddress(_PathQualify, shell32, 'PathQualify');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathQualify]
  end;
end;

var
  _PathIsExe: Pointer;

function PathIsExe;
begin
  GetProcedureAddress(_PathIsExe, shell32, 'PathIsExe');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsExe]
  end;
end;

var
  _PathIsSlowA: Pointer;

function PathIsSlowA;
begin
  GetProcedureAddress(_PathIsSlowA, shell32, 'PathIsSlowA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSlowA]
  end;
end;

var
  _PathIsSlowW: Pointer;

function PathIsSlowW;
begin
  GetProcedureAddress(_PathIsSlowW, shell32, 'PathIsSlowW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSlowW]
  end;
end;

var
  _PathIsSlow: Pointer;

function PathIsSlow;
begin
  GetProcedureAddress(_PathIsSlow, shell32, 'PathIsSlow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSlow]
  end;
end;

var
  _PathCleanupSpec: Pointer;

function PathCleanupSpec;
begin
  GetProcedureAddress(_PathCleanupSpec, shell32, 'PathCleanupSpec');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCleanupSpec]
  end;
end;

var
  _PathResolve: Pointer;

function PathResolve;
begin
  GetProcedureAddress(_PathResolve, shell32, 'PathResolve');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathResolve]
  end;
end;

var
  _GetFileNameFromBrowse: Pointer;

function GetFileNameFromBrowse;
begin
  GetProcedureAddress(_GetFileNameFromBrowse, shell32, 'GetFileNameFromBrowse');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileNameFromBrowse]
  end;
end;

var
  _DriveType: Pointer;

function DriveType;
begin
  GetProcedureAddress(_DriveType, shell32, 'DriveType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DriveType]
  end;
end;

var
  _RealDriveType: Pointer;

function RealDriveType;
begin
  GetProcedureAddress(_RealDriveType, shell32, 'RealDriveType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RealDriveType]
  end;
end;

var
  _IsNetDrive: Pointer;

function IsNetDrive;
begin
  GetProcedureAddress(_IsNetDrive, shell32, 'IsNetDrive');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsNetDrive]
  end;
end;

var
  _Shell_MergeMenus: Pointer;

function Shell_MergeMenus;
begin
  GetProcedureAddress(_Shell_MergeMenus, shell32, 'Shell_MergeMenus');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_Shell_MergeMenus]
  end;
end;

var
  _SHObjectProperties: Pointer;

function SHObjectProperties;
begin
  GetProcedureAddress(_SHObjectProperties, shell32, 'SHObjectProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHObjectProperties]
  end;
end;

var
  _SHFormatDrive: Pointer;

function SHFormatDrive;
begin
  GetProcedureAddress(_SHFormatDrive, shell32, 'SHFormatDrive');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFormatDrive]
  end;
end;

var
  _SHCreatePropSheetExtArray: Pointer;

function SHCreatePropSheetExtArray;
begin
  GetProcedureAddress(_SHCreatePropSheetExtArray, shell32, 'SHCreatePropSheetExtArray');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreatePropSheetExtArray]
  end;
end;

var
  _SHDestroyPropSheetExtArray: Pointer;

procedure SHDestroyPropSheetExtArray;
begin
  GetProcedureAddress(_SHDestroyPropSheetExtArray, shell32, 'SHDestroyPropSheetExtArray');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDestroyPropSheetExtArray]
  end;
end;

var
  _SHAddFromPropSheetExtArray: Pointer;

function SHAddFromPropSheetExtArray;
begin
  GetProcedureAddress(_SHAddFromPropSheetExtArray, shell32, 'SHAddFromPropSheetExtArray');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHAddFromPropSheetExtArray]
  end;
end;

var
  _SHReplaceFromPropSheetExtArray: Pointer;

function SHReplaceFromPropSheetExtArray;
begin
  GetProcedureAddress(_SHReplaceFromPropSheetExtArray, shell32, 'SHReplaceFromPropSheetExtArray');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHReplaceFromPropSheetExtArray]
  end;
end;

var
  _ILClone: Pointer;

function ILClone;
begin
  GetProcedureAddress(_ILClone, shell32, 'ILClone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILClone]
  end;
end;

var
  _ILGetNext: Pointer;

function ILGetNext;
begin
  GetProcedureAddress(_ILGetNext, shell32, 'ILGetNext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILGetNext]
  end;
end;

var
  _ILGetSize: Pointer;

function ILGetSize;
begin
  GetProcedureAddress(_ILGetSize, shell32, 'ILGetSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILGetSize]
  end;
end;

var
  _ILFindLastID: Pointer;

function ILFindLastID;
begin
  GetProcedureAddress(_ILFindLastID, shell32, 'ILFindLastID');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILFindLastID]
  end;
end;

var
  _ILRemoveLastID: Pointer;

function ILRemoveLastID;
begin
  GetProcedureAddress(_ILRemoveLastID, shell32, 'ILRemoveLastID');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILRemoveLastID]
  end;
end;

var
  _ILAppendID: Pointer;

function ILAppendID;
begin
  GetProcedureAddress(_ILAppendID, shell32, 'ILAppendID');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILAppendID]
  end;
end;

var
  _ILFree: Pointer;

procedure ILFree;
begin
  GetProcedureAddress(_ILFree, shell32, 'ILFree');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILFree]
  end;
end;

var
  _ILCloneFirst: Pointer;

function ILCloneFirst;
begin
  GetProcedureAddress(_ILCloneFirst, shell32, 'ILCloneFirst');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILCloneFirst]
  end;
end;

var
  _ILIsEqual: Pointer;

function ILIsEqual;
begin
  GetProcedureAddress(_ILIsEqual, shell32, 'ILIsEqual');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILIsEqual]
  end;
end;

var
  _ILIsParent: Pointer;

function ILIsParent;
begin
  GetProcedureAddress(_ILIsParent, shell32, 'ILIsParent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILIsParent]
  end;
end;

var
  _ILFindChild: Pointer;

function ILFindChild;
begin
  GetProcedureAddress(_ILFindChild, shell32, 'ILFindChild');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILFindChild]
  end;
end;

var
  _ILCombine: Pointer;

function ILCombine;
begin
  GetProcedureAddress(_ILCombine, shell32, 'ILCombine');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILCombine]
  end;
end;

var
  _ILSaveToStream: Pointer;

function ILSaveToStream;
begin
  GetProcedureAddress(_ILSaveToStream, shell32, 'ILSaveToStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILSaveToStream]
  end;
end;

var
  _ILCreateFromPathA: Pointer;

function ILCreateFromPathA;
begin
  GetProcedureAddress(_ILCreateFromPathA, shell32, 'ILCreateFromPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILCreateFromPathA]
  end;
end;

var
  _ILCreateFromPathW: Pointer;

function ILCreateFromPathW;
begin
  GetProcedureAddress(_ILCreateFromPathW, shell32, 'ILCreateFromPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILCreateFromPathW]
  end;
end;

var
  _ILCreateFromPath: Pointer;

function ILCreateFromPath;
begin
  GetProcedureAddress(_ILCreateFromPath, shell32, 'ILCreateFromPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILCreateFromPath]
  end;
end;

var
  _SHILCreateFromPath: Pointer;

function SHILCreateFromPath;
begin
  GetProcedureAddress(_SHILCreateFromPath, shell32, 'SHILCreateFromPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHILCreateFromPath]
  end;
end;

var
  _OpenRegStream: Pointer;

function OpenRegStream;
begin
  GetProcedureAddress(_OpenRegStream, shell32, 'OpenRegStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenRegStream]
  end;
end;

var
  _SHFindFiles: Pointer;

function SHFindFiles;
begin
  GetProcedureAddress(_SHFindFiles, shell32, 'SHFindFiles');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFindFiles]
  end;
end;

var
  _PathGetShortPath: Pointer;

procedure PathGetShortPath;
begin
  GetProcedureAddress(_PathGetShortPath, shell32, 'PathGetShortPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetShortPath]
  end;
end;

var
  _PathYetAnotherMakeUniqueName: Pointer;

function PathYetAnotherMakeUniqueName;
begin
  GetProcedureAddress(_PathYetAnotherMakeUniqueName, shell32, 'PathYetAnotherMakeUniqueName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathYetAnotherMakeUniqueName]
  end;
end;

var
  _Win32DeleteFile: Pointer;

function Win32DeleteFile;
begin
  GetProcedureAddress(_Win32DeleteFile, shell32, 'Win32DeleteFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_Win32DeleteFile]
  end;
end;

var
  _SHRestricted: Pointer;

function SHRestricted;
begin
  GetProcedureAddress(_SHRestricted, shell32, 'SHRestricted');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRestricted]
  end;
end;

var
  _SignalFileOpen: Pointer;

function SignalFileOpen;
begin
  GetProcedureAddress(_SignalFileOpen, shell32, 'SignalFileOpen');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SignalFileOpen]
  end;
end;

var
  _SHSimpleIDListFromPath: Pointer;

function SHSimpleIDListFromPath;
begin
  GetProcedureAddress(_SHSimpleIDListFromPath, shell32, 'SHSimpleIDListFromPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSimpleIDListFromPath]
  end;
end;

var
  _SHDefExtractIconA: Pointer;

function SHDefExtractIconA;
begin
  GetProcedureAddress(_SHDefExtractIconA, shell32, 'SHDefExtractIconA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDefExtractIconA]
  end;
end;

var
  _SHDefExtractIconW: Pointer;

function SHDefExtractIconW;
begin
  GetProcedureAddress(_SHDefExtractIconW, shell32, 'SHDefExtractIconW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDefExtractIconW]
  end;
end;

var
  _SHDefExtractIcon: Pointer;

function SHDefExtractIcon;
begin
  GetProcedureAddress(_SHDefExtractIcon, shell32, 'SHDefExtractIcon');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDefExtractIcon]
  end;
end;

var
  _Shell_GetImageLists: Pointer;

function Shell_GetImageLists;
begin
  GetProcedureAddress(_Shell_GetImageLists, shell32, 'Shell_GetImageLists');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_Shell_GetImageLists]
  end;
end;

var
  _Shell_GetCachedImageIndex: Pointer;

function Shell_GetCachedImageIndex;
begin
  GetProcedureAddress(_Shell_GetCachedImageIndex, shell32, 'Shell_GetCachedImageIndex');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_Shell_GetCachedImageIndex]
  end;
end;

var
  _SHValidateUNC: Pointer;

function SHValidateUNC;
begin
  GetProcedureAddress(_SHValidateUNC, shell32, 'SHValidateUNC');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHValidateUNC]
  end;
end;

var
  _PifMgr_OpenProperties: Pointer;

function PifMgr_OpenProperties;
begin
  GetProcedureAddress(_PifMgr_OpenProperties, shell32, 'PifMgr_OpenProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PifMgr_OpenProperties]
  end;
end;

var
  _PifMgr_GetProperties: Pointer;

function  PifMgr_GetProperties;
begin
  GetProcedureAddress(_PifMgr_GetProperties, shell32, 'PifMgr_GetProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PifMgr_GetProperties]
  end;
end;

var
  _PifMgr_SetProperties: Pointer;

function  PifMgr_SetProperties;
begin
  GetProcedureAddress(_PifMgr_SetProperties, shell32, 'PifMgr_SetProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PifMgr_SetProperties]
  end;
end;

var
  _PifMgr_CloseProperties: Pointer;

function  PifMgr_CloseProperties;
begin
  GetProcedureAddress(_PifMgr_CloseProperties, shell32, 'PifMgr_CloseProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PifMgr_CloseProperties]
  end;
end;

var
  _SHSetInstanceExplorer: Pointer;

procedure SHSetInstanceExplorer;
begin
  GetProcedureAddress(_SHSetInstanceExplorer, shell32, 'SHSetInstanceExplorer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSetInstanceExplorer]
  end;
end;

var
  _IsUserAnAdmin: Pointer;

function IsUserAnAdmin;
begin
  GetProcedureAddress(_IsUserAnAdmin, shell32, 'IsUserAnAdmin');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsUserAnAdmin]
  end;
end;

var
  _SHShellFolderView_Message: Pointer;

function  SHShellFolderView_Message;
begin
  GetProcedureAddress(_SHShellFolderView_Message, shell32, 'SHShellFolderView_Message');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHShellFolderView_Message]
  end;
end;

var
  _SHCreateShellFolderView: Pointer;

function SHCreateShellFolderView;
begin
  GetProcedureAddress(_SHCreateShellFolderView, shell32, 'SHCreateShellFolderView');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateShellFolderView]
  end;
end;

var
  _CDefFolderMenu_Create2: Pointer;

function  CDefFolderMenu_Create2;
begin
  GetProcedureAddress(_CDefFolderMenu_Create2, shell32, 'CDefFolderMenu_Create2');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CDefFolderMenu_Create2]
  end;
end;

var
  _SHFind_InitMenuPopup: Pointer;

function  SHFind_InitMenuPopup;
begin
  GetProcedureAddress(_SHFind_InitMenuPopup, shell32, 'SHFind_InitMenuPopup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFind_InitMenuPopup]
  end;
end;

var
  _SHCreateShellFolderViewEx: Pointer;

function SHCreateShellFolderViewEx;
begin
  GetProcedureAddress(_SHCreateShellFolderViewEx, shell32, 'SHCreateShellFolderViewEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateShellFolderViewEx]
  end;
end;

var
  _SHGetSetSettings: Pointer;

procedure SHGetSetSettings;
begin
  GetProcedureAddress(_SHGetSetSettings, shell32, 'SHGetSetSettings');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSetSettings]
  end;
end;

var
  _SHGetSettings: Pointer;

procedure SHGetSettings;
begin
  GetProcedureAddress(_SHGetSettings, shell32, 'SHGetSettings');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSettings]
  end;
end;

var
  _SHBindToParent: Pointer;

function SHBindToParent;
begin
  GetProcedureAddress(_SHBindToParent, shell32, 'SHBindToParent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHBindToParent]
  end;
end;

var
  _SHParseDisplayName: Pointer;

function SHParseDisplayName;
begin
  GetProcedureAddress(_SHParseDisplayName, shell32, 'SHParseDisplayName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHParseDisplayName]
  end;
end;

var
  _SHPathPrepareForWriteA: Pointer;

function SHPathPrepareForWriteA;
begin
  GetProcedureAddress(_SHPathPrepareForWriteA, shell32, 'SHPathPrepareForWriteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPathPrepareForWriteA]
  end;
end;

var
  _SHPathPrepareForWriteW: Pointer;

function SHPathPrepareForWriteW;
begin
  GetProcedureAddress(_SHPathPrepareForWriteW, shell32, 'SHPathPrepareForWriteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPathPrepareForWriteW]
  end;
end;

var
  _SHPathPrepareForWrite: Pointer;

function SHPathPrepareForWrite;
begin
  GetProcedureAddress(_SHPathPrepareForWrite, shell32, 'SHPathPrepareForWrite');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPathPrepareForWrite]
  end;
end;

var
  _SoftwareUpdateMessageBox: Pointer;

function SoftwareUpdateMessageBox;
begin
  GetProcedureAddress(_SoftwareUpdateMessageBox, shell32, 'SoftwareUpdateMessageBox');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SoftwareUpdateMessageBox]
  end;
end;

var
  _SHPropStgCreate: Pointer;

function SHPropStgCreate;
begin
  GetProcedureAddress(_SHPropStgCreate, shell32, 'SHPropStgCreate');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPropStgCreate]
  end;
end;

var
  _SHPropStgReadMultiple: Pointer;

function SHPropStgReadMultiple;
begin
  GetProcedureAddress(_SHPropStgReadMultiple, shell32, 'SHPropStgReadMultiple');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPropStgReadMultiple]
  end;
end;

var
  _SHPropStgWriteMultiple: Pointer;

function SHPropStgWriteMultiple;
begin
  GetProcedureAddress(_SHPropStgWriteMultiple, shell32, 'SHPropStgWriteMultiple');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHPropStgWriteMultiple]
  end;
end;

var
  _SHLimitInputEdit: Pointer;

function SHLimitInputEdit;
begin
  GetProcedureAddress(_SHLimitInputEdit, shell32, 'SHLimitInputEdit');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHLimitInputEdit]
  end;
end;

var
  _SHMultiFileProperties: Pointer;

function SHMultiFileProperties;
begin
  GetProcedureAddress(_SHMultiFileProperties, shell32, 'SHMultiFileProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHMultiFileProperties]
  end;
end;

var
  _SHMapPIDLToSystemImageListIndex: Pointer;

function SHMapPIDLToSystemImageListIndex;
begin
  GetProcedureAddress(_SHMapPIDLToSystemImageListIndex, shell32, 'SHMapPIDLToSystemImageListIndex');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHMapPIDLToSystemImageListIndex]
  end;
end;

var
  _SHCLSIDFromString: Pointer;

function SHCLSIDFromString;
begin
  GetProcedureAddress(_SHCLSIDFromString, shell32, 'SHCLSIDFromString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCLSIDFromString]
  end;
end;

var
  //_SHCreateQueryCancelAutoPlayMoniker: Pointer;
  _SHCreateQueryCancelAM: Pointer;

function SHCreateQueryCancelAutoPlayMoniker;
begin
  GetProcedureAddress(_SHCreateQueryCancelAM, shell32, 'SHCreateQueryCancelAutoPlayMoniker');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateQueryCancelAM]
  end;
end;

var
  _PerUserInit: Pointer;

procedure PerUserInit;
begin
  GetProcedureAddress(_PerUserInit, mydocs, 'PerUserInit');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PerUserInit]
  end;
end;

var
  _PickIconDlg: Pointer;

function PickIconDlg;
begin
  GetProcedureAddress(_PickIconDlg, shell32, 'PickIconDlg');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PickIconDlg]
  end;
end;

var
  _SHGetAttributesFromDataObject: Pointer;

function SHGetAttributesFromDataObject;
begin
  GetProcedureAddress(_SHGetAttributesFromDataObject, shell32, 'SHGetAttributesFromDataObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetAttributesFromDataObject]
  end;
end;

var
  _ImportPrivacySettings: Pointer;

function ImportPrivacySettings;
begin
  GetProcedureAddress(_ImportPrivacySettings, shdocvwDll, 'ImportPrivacySettings');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ImportPrivacySettings]
  end;
end;

var
  _DoPrivacyDlg: Pointer;

function DoPrivacyDlg;
begin
  GetProcedureAddress(_DoPrivacyDlg, shdocvwDll, 'DoPrivacyDlg');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DoPrivacyDlg]
  end;
end;

var
  _SHEnableServiceObject: Pointer;

function SHEnableServiceObject;
begin
  GetProcedureAddress(_SHEnableServiceObject, shdocvwDll, 'SHEnableServiceObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnableServiceObject]
  end;
end;

var
  _SHGetSetFolderCustomSettingsA: Pointer;

function SHGetSetFolderCustomSettingsA;
begin
  GetProcedureAddress(_SHGetSetFolderCustomSettingsA, shdocvwDll, 'SHGetSetFolderCustomSettingsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSetFolderCustomSettingsA]
  end;
end;

var
  _SHGetSetFolderCustomSettingsW: Pointer;

function SHGetSetFolderCustomSettingsW;
begin
  GetProcedureAddress(_SHGetSetFolderCustomSettingsW, shdocvwDll, 'SHGetSetFolderCustomSettingsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSetFolderCustomSettingsW]
  end;
end;

var
  _SHGetSetFolderCustomSettings: Pointer;

function SHGetSetFolderCustomSettings;
begin
  GetProcedureAddress(_SHGetSetFolderCustomSettings, shdocvwDll, 'SHGetSetFolderCustomSettings');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetSetFolderCustomSettings]
  end;
end;

var
  _CallCPLEntry16: Pointer;

function CallCPLEntry16;
begin
  GetProcedureAddress(_CallCPLEntry16, shdocvwDll, 'CallCPLEntry16');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CallCPLEntry16]
  end;
end;

var
  _SHStartNetConnectionDialogA: Pointer;

function SHStartNetConnectionDialogA;
begin
  GetProcedureAddress(_SHStartNetConnectionDialogA, shdocvwDll, 'SHStartNetConnectionDialogA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStartNetConnectionDialogA]
  end;
end;

var
  _SHStartNetConnectionDialogW: Pointer;

function SHStartNetConnectionDialogW;
begin
  GetProcedureAddress(_SHStartNetConnectionDialogW, shdocvwDll, 'SHStartNetConnectionDialogW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStartNetConnectionDialogW]
  end;
end;

var
  _SHStartNetConnectionDialog: Pointer;

function SHStartNetConnectionDialog;
begin
  GetProcedureAddress(_SHStartNetConnectionDialog, shdocvwDll, 'SHStartNetConnectionDialog');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStartNetConnectionDialog]
  end;
end;

var
  _SHOpenPropSheetA: Pointer;

function SHOpenPropSheetA;
begin
  GetProcedureAddress(_SHOpenPropSheetA, shdocvwDll, 'SHOpenPropSheetA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenPropSheetA]
  end;
end;

var
  _SHOpenPropSheetW: Pointer;

function SHOpenPropSheetW;
begin
  GetProcedureAddress(_SHOpenPropSheetW, shdocvwDll, 'SHOpenPropSheetW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenPropSheetW]
  end;
end;

var
  _SHOpenPropSheet: Pointer;

function SHOpenPropSheet;
begin
  GetProcedureAddress(_SHOpenPropSheet, shdocvwDll, 'SHOpenPropSheet');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenPropSheet]
  end;
end;

var
  _SHCreateFileExtractIconA: Pointer;

function SHCreateFileExtractIconA;
begin
  GetProcedureAddress(_SHCreateFileExtractIconA, shdocvwDll, 'SHCreateFileExtractIconA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateFileExtractIconA]
  end;
end;

var
  _SHCreateFileExtractIconW: Pointer;

function SHCreateFileExtractIconW;
begin
  GetProcedureAddress(_SHCreateFileExtractIconW, shdocvwDll, 'SHCreateFileExtractIconW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateFileExtractIconW]
  end;
end;

var
  _SHCreateFileExtractIcon: Pointer;

function SHCreateFileExtractIcon;
begin
  GetProcedureAddress(_SHCreateFileExtractIcon, shdocvwDll, 'SHCreateFileExtractIcon');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateFileExtractIcon]
  end;
end;

var
  _ILLoadFromStream: Pointer;

function ILLoadFromStream;
begin
  GetProcedureAddress(_ILLoadFromStream, shdocvwDll, 'ILLoadFromStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ILLoadFromStream]
  end;
end;

var
  _PathProcessCommand: Pointer;

function PathProcessCommand;
begin
  GetProcedureAddress(_PathProcessCommand, shdocvwDll, 'PathProcessCommand');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathProcessCommand]
  end;
end;

var
  _SHLoadOLE: Pointer;

function SHLoadOLE;
begin
  GetProcedureAddress(_SHLoadOLE, shdocvwDll, 'SHLoadOLE');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHLoadOLE]
  end;
end;

var
  //_SHMapIDListToImageListIndexAsync: Pointer;
  _SHMapIDListToImageLIA: Pointer;

function SHMapIDListToImageListIndexAsync;
begin
  GetProcedureAddress(_SHMapIDListToImageLIA, shdocvwDll, 'SHMapIDListToImageListIndexAsync');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHMapIDListToImageLIA]
  end;
end;

var
  _SHFlushClipboard: Pointer;

function SHFlushClipboard;
begin
  GetProcedureAddress(_SHFlushClipboard, shdocvwDll, 'SHFlushClipboard');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFlushClipboard]
  end;
end;

var
  _SHGetShellStyleHInstance: Pointer;

function SHGetShellStyleHInstance;
begin
  GetProcedureAddress(_SHGetShellStyleHInstance, shdocvwDll, 'SHGetShellStyleHInstance');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetShellStyleHInstance]
  end;
end;

var
  _SHRunControlPanel: Pointer;

function SHRunControlPanel;
begin
  GetProcedureAddress(_SHRunControlPanel, shdocvwDll, 'SHRunControlPanel');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRunControlPanel]
  end;
end;



{$ENDIF DYNAMIC_LINK}

// DONE: check offsets in Delphi and in C.
procedure InitShellStateSizes;
var
  st: TShellState;
  start: PAnsiChar;
begin
  start := PAnsiChar(@st);
  SHELLSTATE_SIZE_WIN95 := PAnsiChar(@st.lParamSort) - start;
  SHELLSTATE_SIZE_NT4 := PAnsiChar(@st.version) - start;
  SHELLSTATE_SIZE_IE4 := PAnsiChar(@st.uNotUsed) - start;
  SHELLSTATE_SIZE_WIN2K := SizeOf(TShellState);
end;

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
