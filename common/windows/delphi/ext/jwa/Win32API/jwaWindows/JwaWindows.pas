{******************************************************************************}
{                                                                              }
{ Windows API interface Unit for Object Pascal                                 }
{ Master file for Windows applications                                         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaWindows.pas, released September 2005.        }
{ The initial developer of the Pascal code is                                  }
{ Robert Marquardt (robert_marquardt att gmx dott de).                         }
{                                                                              }
{ Portions created by Robert Marquardt are Copyright (C) 2005                  }
{ Robert Marquardt. All Rights Reserved.                                       }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 2007                  }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
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
{******************************************************************************}
{ Description: JwaWindows.pas has the goal to bring all windows conversion     }
{   units into one big unit. There is no need to include many units. Only one  }
{   unit - just this - one is needed. The resulted binary file will enlarge    }
{   about 100-300kb. This depends which parts are used. However it is also     }
{   possible to use all header units without this one. But you should not use  }
{   both versions at the same time. Delphi will fail on some cases because     }
{   types and so other things from two different units are completly different.}
{   You also have the possibility to remove some headers from the file by using}
{   compiler defines. For more information about that see the define comments  }
{   in the source.                                                             }
{ HowTo: You can compile this file by creating a new package and just put this }
{   unit into it. You also have to include the "Common" path in search path    }
{   options. If you use the default tree folder structure just use "..\Common".}
{   You should also define output paths (e.g. "output7" for D7)                }
{                                                                              }
{   You can define compiler defines to change the compilation.                 }
{   Change the compiler defines in this source code or use the compiler        }
{   condition option in the package.                                           }
{   To use dynamic linked function use this:                                   }
{     PACKAGE_CONDITIONS;DYNAMIC_LINK;JWA_INCLUDE_SETUP_API                    }
{   For static linking just remove DYNAMIC_LINK .                              }
{   You can also define debug definitions and so on.                           }
{                                                                              }
{ Warning: Compiling this unit needs a little bit more time than a delphi      }
{   programmer is used to. Therefore we recommend that you create a standalone }
{   JwaWindows.dcu file to use in projects and remove every search path to     }
{   the source codes except for debugger search path (so you can browse the    }
{   sources in the Delphi IDE). You can also create dynamic and static linked  }
{   JwaWindows.dcu seperatly and just change the search path. There can also   }
{   be a debug and a much smaller release version of the file.                 }
{   In conclusion there can be 4 JwaWindows.dcu versions:                      }
{    JwaWindows.dcu                                                            }
{       |- Static linked Debug version                                         }
{       |- Static linked Release version                                       }
{       |- Dynamic linked Debug version                                        }
{       |- Dynamic linked Release version                                      }
{   Be aware that dcu files are incompatible between delphi versions!          }
{                                                                              }
{ Hints: If you get an error in a file you don't know or need you can remove   }
{   it from the include list in this code. You must do this twice: The first   }
{   is an interface include the second one is an implementation include.       }
{   It is very likely that this unit is needed by another one behind the       }
{   excluded one. In this case you must also exclude the other one.            }
{   Units at the beginning are mostly highly necessary for others. This        }
{   includes the first ten units. Errors in them won't probably be fixed easily}
{                                                                              }
{******************************************************************************}

// $Id: JwaWindows.pas,v 1.20 2007/10/19 19:54:18 dezipaitor Exp $

unit JwaWindows;
{
The following defines can be changed to remove headers from JwaWindows.
PACKAGE_CONDITIONS can be set in the options of a project
to disable these source code compiler defines and use
the one of the project.
}

{$IFNDEF PACKAGE_CONDITIONS}

  {$DEFINE DYNAMIC_LINK}  //link most of functions dynamically ?

  {$DEFINE JWA_INCLUDE_SETUP_API} //include setup api ?

  {.$DEFINE JWA_WINSOCK_1} //use winsock.pas instead of winsock2.pas ?

  {-$DEFINE NOVCL}

  {The following directive includes the file JwaAdsTlb.pas
   This unit needs the units OleServer, OleCtrls which do install
   a windows handle in newer delphi versions.
   This handle prevents SetThreadDesktop to work on the main thread. It
   works on a new thread!

   ActiveX and COM is not supported by this lib on Freepascal yet!
  }
  {$DEFINE JWA_INCLUDE_JWAADSTLB}

  {$IFDEF FPC}
    {$UNDEF JWA_INCLUDE_JWAADSTLB}
    {$UNDEF JWA_INCLUDE_SETUP_API}
  {$ENDIF FPC}

  {$DEFINE JWA_INCLUDE_SHELLAPI}

  {Opens up Windows Vista (and above) declarations.
   See jediapilib.inc for more declarations}
  {$DEFINE WINVISTA}


  {.$DEFINE JWA_NEW_WINSTA}
{$ENDIF PACKAGE_CONDITIONS}

{Use one of the following defines to force or ignore unicode for all
packages}
{.$DEFINE UNICODE}
{.$UNDEF UNICODE}


{------ end of your business ------}



{Exclude some units that are not compilabe under FPC
  JwaSensEvts.pas - not supported
  JwaWinFax.pas - partially fixed
  JwaFaxDev.pas depends on jwaWinFax
  JwaFaxExt.pas depends on jwaWinFax
  JwaFaxMmc.pas depends on jwaWinFax
  JwaFaxRoute.pas depends on jwaWinFax
}
{$IFDEF FPC}
  {$DEFINE EXCLUDE_FPC}
{$ENDIF FPC}


{$WEAKPACKAGEUNIT}

{The following defines should not be changed!}
{$DEFINE JWAWINDOWS_PAS}   //include mode activated
{$DEFINE JWA_INCLUDEMODE}  //exclude duplicate things - do not remove!


{$IFDEF DYNAMIC_LINK}
{$DEFINE RTDL} //native api dynamic link compiler define
{$ENDIF DYNAMIC_LINK}

//exclude VCL from freepascal
{$IFDEF FPC}
  {$DEFINE NOVCL}
{$ENDIF FPC}


//extra compiler options
{$I ..\Includes\jedi.inc}
{$I ..\Includes\JediAPILib.inc} //add "..\..\includes" to source path if not found




interface


uses
{$IFDEF USE_DELPHI_TYPES}
  Windows,
{$IFDEF HAS_UNIT_DATEUTILS}
   DateUtils, //used by JwaWinSta.pas
{$ENDIF}
{$IFDEF JWA_INCLUDE_JWAADSTLB}
{The following units may not be included because of problems}

//OleUnits creates window handles that prevents SetThreadDesktop to work
  OleServer, //[warning] requires D5 or higher - required by jwaAdsTLB.pas
  OleCtrls,
//JwaAdsTlb.pas and JwaDde.pas use these units and is therfore excluded
{$ENDIF JWA_INCLUDE_JWAADSTLB}
  
  {$ENDIF USE_DELPHI_TYPES}
  SysUtils, // TODO

{$IFDEF JWA_INCLUDE_SETUP_API}
  CommCtrl, //used by SetupAPI.pas
{$ENDIF JWA_INCLUDE_SETUP_API}

  ActiveX, Classes, ComObj 
{$IFNDEF NOVCL}
  ,Graphics,
  StdVCL
{$ENDIF}

{$IFDEF JWA_INCLUDE_SHELLAPI}
{$IFDEF DELPHI6_UP}
  ,msxml
{$ENDIF DELPHI6_UP}
{$ENDIF JWA_INCLUDE_SHELLAPI}

  ;



{$IFNDEF COMPILER6_UP}
  {$ifndef FPC}
    type PCardinal = ^Cardinal;
  {$ELSE}
    {$ALIGN 8}  
  {$ENDIF}
{$ELSE}
{$ALIGN 8}
{$ENDIF}

(* To include a new converted header file
   you must use this structure:

  {$IFNDEF JWA_OMIT_SECTIONS}
  unit XXXX;
  interface
  uses ...;

  {$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
your interface declarations here
{$ENDIF JWA_IMPLEMENTATIONSECTION}

//!!!
//do no cross JWA_IMPLEMENTATIONSECTION or JWA_INTERFACESECTION with JWA_OMIT_SECTIONS here!
  {$IFNDEF JWA_OMIT_SECTIONS}
  implementation
  uses ...
  {$ENDIF JWA_OMIT_SECTIONS}
//!!!
//do no cross JWA_IMPLEMENTATIONSECTION or JWA_INTERFACESECTION with JWA_OMIT_SECTIONS here!

{$IFNDEF JWA_INTERFACESECTION}
your implementation here
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

*)                         

{$DEFINE JWA_OMIT_SECTIONS}
{$DEFINE JWA_INTERFACESECTION}

{The following constant are version and compiler control instruments to be used
in source code.
}
const
  //include mode active?
  JWA_CONST_INCLUDEMODE = {$IFDEF JWA_INCLUDEMODE} True; {$ELSE} False; {$ENDIF}
  {dynamic linking active? Warning: Not all function will be linked dynamically.
   Some functions cannot be linked dynamically because of their
  }
  JWA_CONST_DYNAMICLINK = {$IFDEF DYNAMIC_LINK} True; {$ELSE} False; {$ENDIF}
  {Like JWA_CONST_DYNAMICLINK but for native win api.}
  JWA_CONST_RDTL        = {$IFDEF RTDL} True; {$ELSE} False; {$ENDIF}
  {Use delphi types instead of our own.}
  JWA_CONST_USE_DELPHI_TYPES  = {$IFDEF USE_DELPHI_TYPES} True; {$ELSE} False; {$ENDIF}
  {Setup api headers included?}
  JWA_CONST_INCLUDE_SETUP_API = {$IFDEF JWA_INCLUDE_SETUP_API} True; {$ELSE} False; {$ENDIF}
  {Version of this header. Its not a CVS, SVN or compile version}
  JWA_CONST_VERSION = '1.5';



{Beginning of interface section.
These files must be in a correct order!
These files are included a second time in the implementation section!
}

{$I JwaWinDLLNames.pas}
{$I JwaWinType.pas}
{$I JwaBitFields.pas}
{$I JwaNtStatus.pas}
{$I JwaWinNT.pas}
{$I JwaWinBase.pas}
{$I JwaWinGDI.pas}
{$I JwaWinUser.pas}
{-$I JwaDde.pas} //do not include - see uses remarks
{$I JwaWinVer.pas}
{$I JwaWinError.pas}
{$I JwaExcpt.pas}
{$I JwaWinNLS.pas}
{$I JwaWinCon.pas}
{$I JwaReason.pas}
{$I JwaWinReg.pas}
{$I JwaWinNetWk.pas}
{$I JwaCdErr.pas}
{ I JwaDDEml.pas} // TODO convert
{$I JwaDlgs.pas}
{ I JwaMMSystem.pas} // TODO convert
{$I JwaNb30.pas}
{ I JwaShellAPI.pas} // TODO convert
{$I JwaWinPerf.pas}
{$I JwaQos.pas}
{$I JwaQosSp.pas}

{.$IFNDEF JWA_INCLUDE_JWAADSTLB}
type UUID = GUID;
{.$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaNative.pas}
{The following files cannot be included because of unfixed errors and problems.
The list has no order!}
//no members

{******** WINSOCK *******************}

{$IFDEF JWA_WINSOCK_1}
  {$I JwaWinSock.pas}
{$ELSE}
  {$DEFINE JWA_WINSOCK_2}
  {$I JwaWinSock2.pas}
  {$I JwaWS2tcpip.pas}
  {$I JwaWS2atm.pas}
  {$I JwaWinDNS.pas}
  {$I JwaNspAPI.pas}
  {$I JwaWS2dnet.pas}
  {$I JwaWS2spi.pas}
  {$I JwaWSnetbs.pas}
  {$I JwaWSNwLink.pas}
  {$I JwaWSvns.pas}
  {$I JwaAF_Irda.pas}
  {$I JwaAtalkWsh.pas}
  {$I JwaWShisotp.pas}
  {$I JwaMSWSock.pas}
  {$I JwaQosPol.pas}
  {$I JwaMSTcpIP.pas}
  {$I JwaQosName.pas}
  {$I JwaIpExport.pas}
  {$I JwaIpRtrMib.pas}
  {$I JwaIpTypes.pas}
  {$I JwaIpHlpApi.pas}
  {$I JwaIcmpApi.pas}
{$ENDIF JWA_WINSOCK_1}


{******** various headers that must be included before rest *********}

{$I JwaLpmApi.pas}
{$I JwaWinCrypt.pas}
{$I JwaRpc.pas}
{$I JwaWinEFS.pas}
{ I JwaWinScard.pas} // TODO convert
{ I JwaWinSpool.pas} // TODO convert
{ I JwaOle2.pas} // TODO convert
{ I JwaCommDlg.pas} // TODO convert
{ I JwaStrAlign.pas} // TODO convert
{$I JwaWinSvc.pas}
{ I JwaMCX.pas} // TODO convert
{ I JwaIMM.pas} // TODO convert
{$I JwaAccCtrl.pas}
{$I JwaAclApi.pas}
{$I JwaSddl.pas}
{$I JwaLmErr}
{$I JwaLmCons}
{$I JwaNtSecApi}
{$I JwaWinCred.pas}
{$I JwaWtsApi32.pas}
{$I JwaWinIoctl.pas}
{$I JwaWowNT32.pas}
{$I JwaSubAuth.pas}
{$I JwaSecurity.pas}
{$I JwaAclUI.pas}
{$I JwaImageHlp.pas}

{******** Ads TLB ************}
{$IFDEF JWA_INCLUDE_JWAADSTLB}
 {$DEFINE NOVCL}
  {$I JwaAdsTLB.pas}
 {$UNDEF NOVCL}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

 {$I JwaActiveX.pas}
 {$I JwaPrSht.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
 {$I JwaActiveDS.pas}
 {$I JwaDSClient.pas}
 {$I JwaDSAdmin.pas}
 {$I JwaAdsProp.pas}
 {$I JwaAdsHlp.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaZMOUSE.pas}
{$I JwaUxTheme.pas}
{$I JwaWinCpl.pas}
{$I JwaWinLDAP.pas}
{$I JwaSvcGuid.pas}
{$I JwaSecExt.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaSspi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{******** LAN Manager **********}
{do not include JwaLM.pas!}

{$DEFINE JWA_OMIT_SECTIONS_LM} //define special LM section omitting
 {$I JwaLmAccess.pas}   // Access, Domain, Group and User classes
 {$I JwaLmAlert.pas}    // Alerter
 {$I JwaLmShare.pas}    // Connection, File, Session and Share classes
 {$I JwaLmMsg.pas}      // Message class
 {$I JwaLmRemUtl.pas}   // Remote Utility class
 {$I JwaLmRepl.pas}     // Replicator class
 {$I JwaLmServer.pas}   // Server class
 {$I JwaLmSvc.pas}      // Service class
 {$I JwaLmUse.pas}      // Use class
 {$I JwaLmWkSta.pas}    // Workstation class
 {$I JwaLmApiBuf.pas}   // NetApiBuffer class
 {$I JwaLmErrLog.pas}   // NetErrorLog class
 {$I JwaLmConfig.pas}   // NetConfig class
 {$I JwaLmStats.pas}    // NetStats class
 {$I JwaLmAudit.pas}    // NetAudit class
 {$I JwaLmJoin.pas}     // NetJoinDomain class
 {$I JwaLmUseFlg.pas}
 {$I JwaLmAt.pas}
 {$I JwaLmDFS.pas}
 {$I JwaLmSName.pas}

{******** the rest **********}

{$I JwaNetSh.pas}
{$I JwaNtDdPar.pas}
{$I JwaSchedule.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaNtQuery.pas}
{$I JwaRpcDce.pas}
{$I JwaNtDsApi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaNtDsbCli.pas}
{$I JwaNtDsBMsg.pas}
{$I JwaNtLDAP.pas}
{$I JwaPsApi.pas}
{$I JwaBitsMsg.pas}
{$I JwaBits.pas}
{$I JwaErrorRep.pas}
{$I JWaBthSdpDef.pas}
{$I JwaBluetoothAPIs.pas}
{$I JwaBtHDef.pas}
  {$I JwaWs2Bth.pas} //depends JwaBtHDef.pas
{$I JwaCryptUIApi.pas}
{$I JwaCpl.pas}
{$I JwaCplext.pas}
{$I JwaColorDlg.pas}
{$I JwaIisCnfg.pas}
{$I JwaIAdmExt.pas}
{$I JwaMsi.pas}
{$I JwaMsiDefs.pas}
{$I JwaMsiQuery.pas}
{$I JwaDSGetDC.pas}
{$I JwaDskQuota.pas}
{$I JwaCmnQuery.pas}
{$I JwaDSQuery.pas}
{$I JwaDSRole.pas}
{$I JwaDsSec.pas}
{$I JwaDbt.pas}
{$I JwaDhcpCSDK.pas}
{$I JwaDhcpSSDK.pas}
{$I JwaDhcpsApi.pas}
{$I JwaWSipx.pas}
{$I JwaWsrm.pas}
{$I JwaWPTypes.pas}
{$I JwaWPWizMsg.pas}
{$I JwaHtmlGuid.pas}
{$I JwaHtmlHelp.pas}
{$I JwaHhError.pas}
{$I JwaRegStr.pas}
{$I JwaPdh.pas}
{$I JwaProfInfo.pas}
{$I JwaWbemCli.pas}
{$I JwaUserEnv.pas}
{$I JwaWPApiMsg.pas}
{$I JwaWPApi.pas}
{$I JwaWPSpiHlp.pas}
{$I JwaWPPstMsg.pas}
{$I JwaWPFtpMsg.pas}
{$I JwaWPCrsMsg.pas}
{$I JwaAdtGen.pas}
{$I JwaAdssts.pas}
{$I JwaAdsnms.pas}
{$I JwaAdsErr.pas}
{$I JwaAdsDb.pas}
{$I JwaAuthz.pas}
{$I JwaAuthif.pas}
{$I JwaWinAble.pas}
{$I JwaWinBer.pas}
{.$IFNDEF EXCLUDE_FPC}
  {$I JwaWinFax.pas}
{.$ENDIF EXCLUDE_FPC}
{$I JwaWinResrc.pas}
{$I JwaWinSafer.pas}
{$I JwaWinWlx.pas}
{$I JwaWmiStr.pas}
{$I JwaWowNT16.pas}
{$I JwaSens.pas}
{$I JwaSensAPI.pas}
{$IFNDEF FPC}
 {$I JwaSensEvts.pas}
{$ENDIF FPC}
{$I JwaSfc.pas}
{$I JwaShlGuid.pas}
{$I JwaSisBkUp.pas}
{$I JwaSisBkUp.pas}
{$I JwaSnmp.pas}
{$I JwaSpOrder.pas}
{$I JwaSrRestorePtApi.pas}
{$I JwaTlHelp32.pas}
{$I JwaTmSchema.pas}
{$I JwaTraffic.pas}
{$I JwaSceSvc.pas}
{$I JwaSchemaDef.pas}
{$I JwaObjSel.pas}
{$I JwaPatchApi.pas}
{$I JwaPatchWiz.pas}
{$I JwaPbt.pas}
{$I JwaPdhMsg.pas}
{$I JwaPowrProf.pas}
{$I JwaProtocol.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaRpcASync.pas}
{$I JwaRpcNsi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaRpcNtErr.pas}
{$I JwaRpcSsl.pas}
{$I JwaMciAvi.pas}
{$I JwaMprError.pas}
{$I JwaMsTask.pas}
{$I JwaCardErr.pas}
{.$IFNDEF EXCLUDE_FPC}
  {$I JwaFaxDev.pas}
  {$I JwaFaxExt.pas}
  {$I JwaFaxMmc.pas}
  {$I JwaFaxRoute.pas}
{.$ENDIF EXCLUDE_FPC}

{$I JwaGPEdit.pas}
{$I JwaIAccess.pas}
{$I JwaImapi.pas}
{$I JwaImapiError.pas}
{$I JwaIme.pas}
{$I JwaBits1_5.pas}
{$I JwaBits2_0.pas}
{$I JwaBits2_5.pas}
{$I JwaBits3_0.pas}
{$I Jwadwmapi.pas}
{$I JwaIoEvent.pas}
{$I JwaIpIfCons.pas}
{$I JwaIpInfoId.pas}
{$I JwaIsGuids.pas}
{$I JwaIssPer16.pas}
{$I JwaLoadPerf.pas}
{$I JwaBatClass.pas}
{$I JwaBLBErr.pas}
{$I JwaBugCodes.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaBitscfg.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaWinInet.pas}

{$DEFINE JWA_INCLUDEMODE}

{$IFDEF JWA_NEW_WINSTA}
  {$I JwaRpcWinsta.pas}
{$ELSE}
   {$I JwaWinSta.pas}
  {$IFDEF FPC}
   {$I JwaWinSta.pas}
  {$ENDIF}
{$ENDIF}

{$I JwaStrSafe.pas}


{$IFDEF JWA_INCLUDE_SHELLAPI}
{$I JwaUrlHist.pas}
{$I JwaUrlMon.pas}
{$I JwaSHFolder.pas}
{$I JwaSHAppMgr.pas}
{$I JwaShellAPI.pas}

{$I JwaShlDisp.pas}
{$I JwaShlObj.pas}
{$I JwaShlWAPI.pas}

{$ENDIF}

{$I JwaBCrypt.pas}
{$I JwaNCrypt.pas}

{$I JwaEventDefs.pas}
{$I JwaEventTracing.pas}
{$I JwaEvntProv.pas}
{$I JwaEvntCons.pas}

{$I JwaWabDefs.pas}
{$I JwaWabCode.pas}
{$I JwaWabIab.pas}
{$I JwaWabApi.pas}
{$I JwaWabMem.pas}
{$I JwaWabUtil.pas}
{$I JwaWabTags.pas}
{$I JwaWabNot.pas}
{$I JwaWdm.pas}

{.$I JwaWinternl.pas}  //not used anymore!

{$I JwaModuleLoader.pas}//set source path ..\..\Common if not found

{$IFDEF JWA_INCLUDE_SETUP_API}
  {$DEFINE SETUPAPI_LINKONREQUEST}
  {
  If the compiler cannot find "SaCMAPI" its
  because it should be "Setup and Config Manager API".
  But the compiler cannot include files with spaces in it.
  Simply hard link or copy the following files into the new folder.
  }
  {$I ..\SaCMAPI\SetupApi.pas}
  {$I ..\SaCMAPI\Cfg.pas}
  {$I ..\SaCMAPI\CfgMgr32.pas}
{$ENDIF JWA_INCLUDE_SETUP_API}

{$I JwaWintrust.pas} //Allignment 8
{$I JwaSoftpub.pas}  //Allignment 8

{******* Add here new units *******}


{$UNDEF JWA_INTERFACESECTION}

{$UNDEF JWA_OMIT_SECTIONS}

implementation

{$DEFINE JWA_OMIT_SECTIONS}
{$DEFINE JWA_IMPLEMENTATIONSECTION}

{Implementation section.
These files must be in a correct order!
}

{$I JwaWinDLLNames.pas}
{$I JwaWinType.pas}
{$I JwaBitFields}
{$I JwaNtStatus.pas}
{$I JwaWinNT.pas}
{$I JwaWinBase.pas}
{$I JwaWinGDI.pas}
{$I JwaWinUser.pas}
{-$I JwaDde.pas} //do not include - see uses remarks
{$I JwaWinVer.pas}
{$I JwaWinError.pas}
{$I JwaExcpt.pas}
{$I JwaWinNLS.pas}
{$I JwaWinCon.pas}
{$I JwaReason.pas}
{$I JwaWinReg.pas}
{$I JwaWinNetWk.pas}
{$I JwaCdErr.pas}
{ I JwaDDEml.pas} // TODO convert
{$I JwaDlgs.pas}
{ I JwaMMSystem.pas} // TODO convert
{$I JwaNb30.pas}
{ I JwaShellAPI.pas} // TODO convert
{$I JwaWinPerf.pas}
{$I JwaQos.pas}
{$I JwaQosSp.pas}

{$I JwaNative.pas}

{The following files cannot be included because of unfixed errors and problems.
The list has no order!}
//no members


{******** WINSOCK *******************}

{$IFDEF JWA_WINSOCK_1}
  {$I JwaWinSock.pas}
  {$ELSE}
  {$DEFINE JWA_WINSOCK_2}
  {$I JwaWinSock2.pas}
  {$I JwaWS2tcpip.pas}
  {$I JwaWS2atm.pas}
  {$I JwaWinDNS.pas}
  {$I JwaNspAPI.pas}
  {$I JwaWS2dnet.pas}
  {$I JwaWS2spi.pas}
  {$I JwaWSnetbs.pas}
  {$I JwaWSNwLink.pas}
  {$I JwaWSvns.pas}
  {$I JwaAF_Irda.pas}
  {$I JwaAtalkWsh.pas}
  {$I JwaWShisotp.pas}
  {$I JwaMSWSock.pas}
  {$I JwaQosPol.pas}
  {$I JwaMSTcpIP.pas}
  {$I JwaQosName.pas}
  {$I JwaIpExport.pas}
  {$I JwaIpRtrMib.pas}
  {$I JwaIpTypes.pas}
  {$I JwaIpHlpApi.pas}
  {$I JwaIcmpApi.pas}
{$ENDIF JWA_WINSOCK_1}

{******** various headers that must be included before rest *********}

{$I JwaLpmApi.pas}
{$I JwaWinCrypt.pas}
{$I JwaRpc.pas}
{$I JwaWinEFS.pas}
{ I JwaWinScard.pas} // TODO convert
{ I JwaWinSpool.pas} // TODO convert
{ I JwaOle2.pas} // TODO convert
{ I JwaCommDlg.pas} // TODO convert
{ I JwaStrAlign.pas} // TODO convert
{$I JwaWinSvc.pas}
{ I JwaMCX.pas} // TODO convert
{ I JwaIMM.pas} // TODO convert
{$I JwaAccCtrl.pas}
{$I JwaAclApi.pas}
{$I JwaSddl.pas}
{$I JwaLmErr}
{$I JwaLmCons}
{$I JwaNtSecApi}
{$I JwaWinCred.pas}
{$I JwaWtsApi32.pas}
{$I JwaWinIoctl.pas}
{$I JwaWowNT32.pas}
{$I JwaSubAuth.pas}
{$I JwaSecurity.pas}
{$I JwaAclUI.pas}
{$I JwaImageHlp.pas}


{$IFDEF JWA_INCLUDE_JWAADSTLB}
{******** Ads TLB ************}
{$DEFINE NOVCL}
 {$I JwaAdsTLB.pas}
{$UNDEF NOVCL}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaActiveX.pas}
{$I JwaPrSht.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaActiveDS.pas}
{$I JwaDSClient.pas}
{$I JwaDSAdmin.pas}
{$I JwaAdsProp.pas}
{$I JwaAdsHlp.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaZMOUSE.pas}
{$I JwaUxTheme.pas}
{$I JwaWinCpl.pas}
{$I JwaWinLDAP.pas}
{$I JwaSvcGuid.pas}
{$I JwaSecExt.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaSspi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{******** LAN Manager **********}
{do not include JwaLM.pas!}

{$DEFINE JWA_OMIT_SECTIONS_LM} //define special LM section omitting
{$I JwaLmAccess.pas}   // Access, Domain, Group and User classes
{$I JwaLmAlert.pas}    // Alerter
{$I JwaLmShare.pas}    // Connection, File, Session and Share classes
{$I JwaLmMsg.pas}      // Message class
{$I JwaLmRemUtl.pas}   // Remote Utility class
{$I JwaLmRepl.pas}     // Replicator class
{$I JwaLmServer.pas}   // Server class
{$I JwaLmSvc.pas}      // Service class
{$I JwaLmUse.pas}      // Use class
{$I JwaLmWkSta.pas}    // Workstation class
{$I JwaLmApiBuf.pas}   // NetApiBuffer class
{$I JwaLmErrLog.pas}   // NetErrorLog class
{$I JwaLmConfig.pas}   // NetConfig class
{$I JwaLmStats.pas}    // NetStats class
{$I JwaLmAudit.pas}    // NetAudit class
{$I JwaLmJoin.pas}     // NetJoinDomain class
{$I JwaLmUseFlg.pas}
{$I JwaLmAt.pas}
{$I JwaLmDFS.pas}
{$I JwaLmSName.pas}

{******** the rest **********}

{$I JwaNetSh.pas}
{$I JwaNtDdPar.pas}
{$I JwaSchedule.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaNtQuery.pas}
{$I JwaRpcDce.pas}
{$I JwaNtDsApi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}


{$I JwaNtDsbCli.pas}
{$I JwaNtDsBMsg.pas}
{$I JwaNtLDAP.pas}
{$I JwaPsApi.pas}
{$I JwaBitsMsg.pas}
{$I JwaBits.pas}
{$I JwaErrorRep.pas}
{$I JWaBthSdpDef.pas}
{$I JwaBluetoothAPIs.pas}
{$I JwaBtHDef.pas}
{$I JwaWs2Bth.pas} //depends JwaBtHDef.pas
{$I JwaCryptUIApi.pas}
{$I JwaCpl.pas}
{$I JwaCplext.pas}
{$I JwaColorDlg.pas}
{$I JwaIisCnfg.pas}
{$I JwaIAdmExt.pas}
{$I JwaMsi.pas}
{$I JwaMsiDefs.pas}
{$I JwaMsiQuery.pas}
{$I JwaDSGetDC.pas}
{$I JwaDskQuota.pas}
{$I JwaCmnQuery.pas}
{$I JwaDSQuery.pas}
{$I JwaDSRole.pas}
{$I JwaDsSec.pas}
{$I JwaDbt.pas}
{$I JwaDhcpCSDK.pas}
{$I JwaDhcpSSDK.pas}
{$I JwaDhcpsApi.pas}
{$I JwaWSipx.pas}
{$I JwaWsrm.pas}
{$I JwaWPTypes.pas}
{$I JwaWPWizMsg.pas}
{$I JwaHtmlGuid.pas}
{$I JwaHtmlHelp.pas}
{$I JwaHhError.pas}
{$I JwaRegStr.pas}
{$I JwaPdh.pas}
{$I JwaProfInfo.pas}
{$I JwaWbemCli.pas}
{$I JwaUserEnv.pas}
{$I JwaWPApiMsg.pas}
{$I JwaWPApi.pas}
{$I JwaWPSpiHlp.pas}
{$I JwaWPPstMsg.pas}
{$I JwaWPFtpMsg.pas}
{$I JwaWPCrsMsg.pas}
{$I JwaAdtGen.pas}
{$I JwaAdssts.pas}
{$I JwaAdsnms.pas}
{$I JwaAdsErr.pas}
{$I JwaAdsDb.pas}
{$I JwaAuthz.pas}
{$I JwaAuthif.pas}
{$I JwaWinAble.pas}
{$I JwaWinBer.pas}
{.$IFNDEF EXCLUDE_FPC}
  {$I JwaWinFax.pas}
{.$ENDIF EXCLUDE_FPC}
{$I JwaWinResrc.pas}
{$I JwaWinSafer.pas}
{$I JwaWinWlx.pas}
{$I JwaWmiStr.pas}
{$I JwaWowNT16.pas}


{$I JwaSens.pas}
{$I JwaSensAPI.pas}
{$IFNDEF FPC}
 {$I JwaSensEvts.pas}
{$ENDIF FPC}
{$I JwaSfc.pas}
{$I JwaShlGuid.pas}
{$I JwaSisBkUp.pas}
{$I JwaSisBkUp.pas}
{$I JwaSnmp.pas}
{$I JwaSpOrder.pas}
{$I JwaSrRestorePtApi.pas}
{$I JwaTlHelp32.pas}
{$I JwaTmSchema.pas}
{$I JwaTraffic.pas}
{$I JwaSceSvc.pas}
{$I JwaSchemaDef.pas} 
{$I JwaObjSel.pas}
{$I JwaPatchApi.pas}
{$I JwaPatchWiz.pas}
{$I JwaPbt.pas}
{$I JwaPdhMsg.pas}
{$I JwaPowrProf.pas}
{$I JwaProtocol.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaRpcASync.pas}
{$I JwaRpcNsi.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaRpcNtErr.pas}
{$I JwaRpcSsl.pas}
{$I JwaMciAvi.pas}
{$I JwaMprError.pas}
{$I JwaMsTask.pas}
{$I JwaCardErr.pas}
{.$IFNDEF EXCLUDE_FPC}
  {$I JwaFaxDev.pas}
  {$I JwaFaxExt.pas}
  {$I JwaFaxMmc.pas}
  {$I JwaFaxRoute.pas}
{.$ENDIF EXCLUDE_FPC}
{$I JwaGPEdit.pas}
{$I JwaIAccess.pas}
{$I JwaImapi.pas}
{$I JwaImapiError.pas}
{$I JwaIme.pas}
{$I JwaBits1_5.pas}
{$I JwaBits2_0.pas}
{$I JwaBits2_5.pas}
{$I JwaBits3_0.pas}
{$I Jwadwmapi.pas}
{$I JwaIoEvent.pas}
{$I JwaIpIfCons.pas}
{$I JwaIpInfoId.pas}
{$I JwaIsGuids.pas}
{$I JwaIssPer16.pas}
{$I JwaLoadPerf.pas}
{$I JwaBatClass.pas}
{$I JwaBLBErr.pas}
{$I JwaBugCodes.pas}

{$IFDEF JWA_INCLUDE_JWAADSTLB}
{$I JwaBitscfg.pas}
{$ENDIF JWA_INCLUDE_JWAADSTLB}

{$I JwaWinInet.pas}

{$DEFINE JWA_INCLUDEMODE}

{.$I JwaWinternl.pas} //not used anymore!

{$IFDEF JWA_NEW_WINSTA}
  {$I JwaRpcWinsta.pas}
{$ELSE}
  {.$IFDEF COMPILER6_UP}
   {$I JwaWinSta.pas} //has some unsolved probs with DateUtils
  {.$ENDIF}
  {$IFDEF FPC}
   {$I JwaWinSta.pas}
  {$ENDIF}
{$ENDIF}

{$I JwaStrSafe.pas}


{$IFDEF JWA_INCLUDE_SHELLAPI}
{$I JwaUrlHist.pas}
{$I JwaUrlMon.pas}
{$I JwaSHFolder.pas}
{$I JwaSHAppMgr.pas}
{$I JwaShellAPI.pas}

{$I JwaShlDisp.pas}
{$I JwaShlObj.pas}
{$I JwaShlWAPI.pas}
{$ENDIF}

{$I JwaBCrypt.pas}
{$I JwaNCrypt.pas}

{$I JwaEventDefs.pas}
{$I JwaEventTracing.pas}
{$I JwaEvntProv.pas}
{$I JwaEvntCons.pas}

{$I JwaWabDefs.pas}
{$I JwaWabCode.pas}
{$I JwaWabIab.pas}
{$I JwaWabApi.pas}
{$I JwaWabMem.pas}
{$I JwaWabUtil.pas}
{$I JwaWabTags.pas}
{$I JwaWabNot.pas}


{$I JwaModuleLoader.pas}
{$IFDEF JWA_INCLUDE_SETUP_API}
{$DEFINE SETUPAPI_LINKONREQUEST}

{If the compiler cannot find "SetupApi.pas" its 	
because it resides in "Setup and Config Manager API".
But the compiler cannot include folders with spaces in it.
Simply hard link or copy the following files
into the new folder named "SaCMAPI".
This issue should be fixed in newer versions of JEDI API LIB.
If you get this error you should do the things described or upgrade. 
}
{$I ..\SaCMAPI\SetupApi.pas}
{$I ..\SaCMAPI\Cfg.pas}
{$I ..\SaCMAPI\CfgMgr32.pas}
{$ENDIF JWA_INCLUDE_SETUP_API}

{$I JwaWintrust.pas} //Allignment 8
{$I JwaSoftpub.pas}  //Allignment 8

{******* Add here new units *******}

{$UNDEF JWA_IMPLEMENTATIONSECTION}

{$UNDEF JWA_OMIT_SECTIONS}

end.




