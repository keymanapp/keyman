{******************************************************************************}
{                                                                              }
{ Visual Styles (Themes) API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: dwmapi.h  The initial developer of the                 }
{ Pascal code is TUO (http://www.TheUnknownOnes.net)                           }
{                                                                              }
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


{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaDwmapi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "dwmapi.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaUxTheme,
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  DWM_BB_ENABLE                 = $00000001;  // fEnable has been specified
  {$EXTERNALSYM DWM_BB_ENABLE}
  DWM_BB_BLURREGION             = $00000002;  // hRgnBlur has been specified
  {$EXTERNALSYM DWM_BB_BLURREGION}
  DWM_BB_TRANSITIONONMAXIMIZED  = $00000004;  // fTransitionOnMaximized has been specified
  {$EXTERNALSYM DWM_BB_TRANSITIONONMAXIMIZED}
  
type
  _DWM_BLURBEHIND = record
    dwFlags                 : DWORD;
    fEnable                 : BOOL;
    hRgnBlur                : HRGN;
    fTransitionOnMaximized  : BOOL;
  end;
  {$EXTERNALSYM _DWM_BLURBEHIND}
  DWM_BLURBEHIND = _DWM_BLURBEHIND;
  {$EXTERNALSYM DWM_BLURBEHIND}
  PDWM_BLURBEHIND = ^DWM_BLURBEHIND;
  {$EXTERNALSYM PDWM_BLURBEHIND}

  // Window attributes
  DWMWINDOWATTRIBUTE = ({$IFDEF DELPHI5}DWMWA_PAD,{$ENDIF}
                        DWMWA_NCRENDERING_ENABLED{$IFDEF DELPHI6_UP} = 1{$ENDIF},      // [get] Is non-client rendering enabled/disabled
                        DWMWA_NCRENDERING_POLICY,           // [set] Non-client rendering policy
                        DWMWA_TRANSITIONS_FORCEDISABLED,    // [set] Potentially enable/forcibly disable transitions
                        DWMWA_ALLOW_NCPAINT,                // [set] Allow contents rendered in the non-client area to be visible on the DWM-drawn frame.
                        DWMWA_CAPTION_BUTTON_BOUNDS,        // [get] Bounds of the caption button area in window-relative space.
                        DWMWA_NONCLIENT_RTL_LAYOUT,         // [set] Is non-client content RTL mirrored
                        DWMWA_FORCE_ICONIC_REPRESENTATION,  // [set] Force this window to display iconic thumbnails.
                        DWMWA_FLIP3D_POLICY,                // [set] Designates how Flip3D will treat the window.
                        DWMWA_EXTENDED_FRAME_BOUNDS,        // [get] Gets the extended frame bounds rectangle in screen space
                        DWMWA_LAST);
  {$EXTERNALSYM DWMWINDOWATTRIBUTE}

  // Non-client rendering policy attribute values
  DWMNCRENDERINGPOLICY = (DWMNCRP_USEWINDOWSTYLE, // Enable/disable non-client rendering based on window style
                          DWMNCRP_DISABLED,       // Disabled non-client rendering; window style is ignored
                          DWMNCRP_ENABLED,        // Enabled non-client rendering; window style is ignored
                          DWMNCRP_LAST);
  {$EXTERNALSYM DWMNCRENDERINGPOLICY}

  // Values designating how Flip3D treats a given window.
  DWMFLIP3DWINDOWPOLICY = ( DWMFLIP3D_DEFAULT,      // Hide or include the window in Flip3D based on window style and visibility.
                            DWMFLIP3D_EXCLUDEBELOW, // Display the window under Flip3D and disabled.
                            DWMFLIP3D_EXCLUDEABOVE, // Display the window above Flip3D and enabled.
                            DWMFLIP3D_LAST);
  {$EXTERNALSYM DWMFLIP3DWINDOWPOLICY}

  // Thumbnails
  HTHUMBNAIL = type HANDLE;
  {$EXTERNALSYM DWMFLIP3DWINDOWPOLICY}
  PHTHUMBNAIL = ^HTHUMBNAIL;
  {$EXTERNALSYM DWMFLIP3DWINDOWPOLICY}

const
  DWM_TNP_RECTDESTINATION       = $00000001;
  {$EXTERNALSYM DWM_TNP_RECTDESTINATION}
  DWM_TNP_RECTSOURCE            = $00000002;
  {$EXTERNALSYM DWM_TNP_RECTSOURCE}
  DWM_TNP_OPACITY               = $00000004;
  {$EXTERNALSYM DWM_TNP_OPACITY}
  DWM_TNP_VISIBLE               = $00000008;
  {$EXTERNALSYM DWM_TNP_VISIBLE}
  DWM_TNP_SOURCECLIENTAREAONLY  = $00000010;
  {$EXTERNALSYM DWM_TNP_SOURCECLIENTAREAONLY}

type
  _DWM_THUMBNAIL_PROPERTIES = record
    dwFlags               : DWORD;
    rcDestination         : RECT;
    rcSource              : RECT;
    opacity               : BYTE;
    fVisible              : BOOL;
    fSourceClientAreaOnly : BOOL;
  end;
  {$EXTERNALSYM DWM_TNP_SOURCECLIENTAREAONLY}
  DWM_THUMBNAIL_PROPERTIES = _DWM_THUMBNAIL_PROPERTIES;
  {$EXTERNALSYM DWM_TNP_SOURCECLIENTAREAONLY}
  PDWM_THUMBNAIL_PROPERTIES = ^DWM_THUMBNAIL_PROPERTIES;
  {$EXTERNALSYM DWM_TNP_SOURCECLIENTAREAONLY}

  // Video enabling apis

  DWM_FRAME_COUNT = type ULONGLONG;
  {$EXTERNALSYM DWM_FRAME_COUNT}
  QPC_TIME = type ULONGLONG;
  {$EXTERNALSYM QPC_TIME}

  _UNSIGNED_RATIO = record
    uiNumerator   : UINT32;
    uiDenominator : UINT32;
  end;
  {$EXTERNALSYM _UNSIGNED_RATIO}
  UNSIGNED_RATIO = _UNSIGNED_RATIO;
  {$EXTERNALSYM UNSIGNED_RATIO}

  _DWM_TIMING_INFO = record
    cbSize : UINT32;

    // Data on DWM composition overall
    
    // Monitor refresh rate
    rateRefresh : UNSIGNED_RATIO;

    // Actual period
    qpcRefreshPeriod : QPC_TIME;

    // composition rate     
    rateCompose : UNSIGNED_RATIO;

    // QPC time at a VSync interupt
    qpcVBlank : QPC_TIME;

    // DWM refresh count of the last vsync
    // DWM refresh count is a 64bit number where zero is
    // the first refresh the DWM woke up to process
    cRefresh : DWM_FRAME_COUNT;

    // DX refresh count at the last Vsync Interupt
    // DX refresh count is a 32bit number with zero 
    // being the first refresh after the card was initialized
    // DX increments a counter when ever a VSync ISR is processed
    // It is possible for DX to miss VSyncs
    //
    // There is not a fixed mapping between DX and DWM refresh counts
    // because the DX will rollover and may miss VSync interupts
    cDXRefresh : UINT;

    // QPC time at a compose time.  
    qpcCompose : QPC_TIME;

    // Frame number that was composed at qpcCompose
    cFrame : DWM_FRAME_COUNT;

    // The present number DX uses to identify renderer frames
    cDXPresent : UINT;

    // Refresh count of the frame that was composed at qpcCompose
    cRefreshFrame : DWM_FRAME_COUNT;


    // DWM frame number that was last submitted
    cFrameSubmitted : DWM_FRAME_COUNT;

    // DX Present number that was last submitted
    cDXPresentSubmitted : UINT;

    // DWM frame number that was last confirmed presented
    cFrameConfirmed : DWM_FRAME_COUNT;

    // DX Present number that was last confirmed presented
    cDXPresentConfirmed : UINT;

    // The target refresh count of the last
    // frame confirmed completed by the GPU
    cRefreshConfirmed : DWM_FRAME_COUNT;

    // DX refresh count when the frame was confirmed presented
    cDXRefreshConfirmed : UINT;

    // Number of frames the DWM presented late
    // AKA Glitches
    cFramesLate : DWM_FRAME_COUNT;
    
    // the number of composition frames that 
    // have been issued but not confirmed completed
    cFramesOutstanding : UINT;


    // Following fields are only relavent when an HWND is specified
    // Display frame


    // Last frame displayed
    cFrameDisplayed : DWM_FRAME_COUNT;

    // QPC time of the composition pass when the frame was displayed
    qpcFrameDisplayed : QPC_TIME;

    // Count of the VSync when the frame should have become visible
    cRefreshFrameDisplayed : DWM_FRAME_COUNT;

    // Complete frames: DX has notified the DWM that the frame is done rendering

    // ID of the the last frame marked complete (starts at 0)
    cFrameComplete : DWM_FRAME_COUNT;

    // QPC time when the last frame was marked complete
    qpcFrameComplete : QPC_TIME;

    // Pending frames:
    // The application has been submitted to DX but not completed by the GPU
 
    // ID of the the last frame marked pending (starts at 0)
    cFramePending : DWM_FRAME_COUNT;

    // QPC time when the last frame was marked pending
    qpcFramePending : QPC_TIME;

    // number of unique frames displayed
    cFramesDisplayed : DWM_FRAME_COUNT;

    // number of new completed frames that have been received
    cFramesComplete : DWM_FRAME_COUNT;

     // number of new frames submitted to DX but not yet complete
    cFramesPending : DWM_FRAME_COUNT;

    // number of frames available but not displayed, used or dropped
    cFramesAvailable : DWM_FRAME_COUNT;

    // number of rendered frames that were never
    // displayed because composition occured too late
    cFramesDropped : DWM_FRAME_COUNT;
    
    // number of times an old frame was composed 
    // when a new frame should have been used
    // but was not available
    cFramesMissed : DWM_FRAME_COUNT;
    
    // the refresh at which the next frame is
    // scheduled to be displayed
    cRefreshNextDisplayed : DWM_FRAME_COUNT;

    // the refresh at which the next DX present is 
    // scheduled to be displayed
    cRefreshNextPresented : DWM_FRAME_COUNT;

    // The total number of refreshes worth of content
    // for this HWND that have been displayed by the DWM
    // since DwmSetPresentParameters was called
    cRefreshesDisplayed : DWM_FRAME_COUNT;
	
    // The total number of refreshes worth of content
    // that have been presented by the application
    // since DwmSetPresentParameters was called
    cRefreshesPresented : DWM_FRAME_COUNT;


    // The actual refresh # when content for this
    // window started to be displayed
    // it may be different than that requested
    // DwmSetPresentParameters
    cRefreshStarted : DWM_FRAME_COUNT;

    // Total number of pixels DX redirected
    // to the DWM.
    // If Queueing is used the full buffer
    // is transfered on each present.
    // If not queuing it is possible only 
    // a dirty region is updated
    cPixelsReceived : ULONGLONG;

    // Total number of pixels drawn.
    // Does not take into account if
    // if the window is only partial drawn
    // do to clipping or dirty rect management
    cPixelsDrawn : ULONGLONG;

    // The number of buffers in the flipchain
    // that are empty.   An application can 
    // present that number of times and guarantee
    // it won't be blocked waiting for a buffer to 
    // become empty to present to
    cBuffersEmpty : DWM_FRAME_COUNT;
  end;
  {$EXTERNALSYM _DWM_TIMING_INFO}
  DWM_TIMING_INFO = _DWM_TIMING_INFO;
  {$EXTERNALSYM DWM_TIMING_INFO}

  DWM_SOURCE_FRAME_SAMPLING = (
    // Use the first source frame that
    // includes the first refresh of the output frame
    DWM_SOURCE_FRAME_SAMPLING_POINT,

    // use the source frame that includes the most
    // refreshes of out the output frame
    // in case of multiple source frames with the
    // same coverage the last will be used
    DWM_SOURCE_FRAME_SAMPLING_COVERAGE,

       // Sentinel value
    DWM_SOURCE_FRAME_SAMPLING_LAST);
  {$EXTERNALSYM DWM_SOURCE_FRAME_SAMPLING}

const
  c_DwmMaxQueuedBuffers : UINT = 8;
  {$EXTERNALSYM c_DwmMaxQueuedBuffers}
  c_DwmMaxMonitors      : UINT = 16;
  {$EXTERNALSYM c_DwmMaxMonitors}
  c_DwmMaxAdapters      : UINT = 16;
  {$EXTERNALSYM c_DwmMaxAdapters}

type
  _DWM_PRESENT_PARAMETERS = record
    cbSize              : UINT32;
    fQueue              : BOOL;
    cRefreshStart       : DWM_FRAME_COUNT;
    cBuffer             : UINT;
    fUseSourceRate      : BOOL;
    rateSource          : UNSIGNED_RATIO;
    cRefreshesPerFrame  : UINT;
    eSampling           : DWM_SOURCE_FRAME_SAMPLING;
  end;
  {$EXTERNALSYM _DWM_PRESENT_PARAMETERS}
  DWM_PRESENT_PARAMETERS = _DWM_PRESENT_PARAMETERS;
  {$EXTERNALSYM DWM_PRESENT_PARAMETERS}

const
  DWM_FRAME_DURATION_DEFAULT  = -1;
  {$EXTERNALSYM DWM_FRAME_DURATION_DEFAULT}

  function DwmDefWindowProc(hWnd : HWND; msg : UINT; wParam : WPARAM; lParam : LPARAM; out plResult : LRESULT) : BOOL; stdcall;
  {$EXTERNALSYM DwmDefWindowProc}
  function DwmEnableBlurBehindWindow(hWnd : HWND; const pBlurBehind : DWM_BLURBEHIND) : HRESULT; stdcall;
  {$EXTERNALSYM DwmEnableBlurBehindWindow}

const
  DWM_EC_DISABLECOMPOSITION = 0;
  {$EXTERNALSYM DWM_EC_DISABLECOMPOSITION}
  DWM_EC_ENABLECOMPOSITION  = 1;
  {$EXTERNALSYM DWM_EC_ENABLECOMPOSITION}

  function DwmEnableComposition(uCompositionAction : UINT) : HRESULT; stdcall;
  {$EXTERNALSYM DwmEnableComposition}
  function DwmEnableMMCSS(fEnableMMCSS : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmEnableMMCSS}
  function DwmExtendFrameIntoClientArea(hWnd : HWND; const pMarInset : MARGINS) : HRESULT; stdcall;
  {$EXTERNALSYM DwmExtendFrameIntoClientArea}
  function DwmGetColorizationColor(out pcrColorization : DWORD; out pfOpaqueBlend : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetColorizationColor}
  function DwmGetCompositionTimingInfo(hWnd : HWND; out pTimingInfo : DWM_TIMING_INFO) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetCompositionTimingInfo}
  function DwmGetWindowAttribute(hWnd : HWND; dwAttribute : DWORD; pvAttribute : PVOID; cbAttribute : DWORD) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetWindowAttribute}
  function DwmIsCompositionEnabled(out pfEnabled : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmIsCompositionEnabled}
  function DwmModifyPreviousDxFrameDuration(hWnd : HWND; cRefreshes : INT; fRelative : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmModifyPreviousDxFrameDuration}
  function DwmQueryThumbnailSourceSize(hThumbnail : HTHUMBNAIL; out pSize : SIZE) : HRESULT; stdcall;
  {$EXTERNALSYM DwmQueryThumbnailSourceSize}
  function DwmRegisterThumbnail(hwndDestination : HWND; hwndSource : HWND; out phThumbnailId : HTHUMBNAIL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmRegisterThumbnail}
  function DwmSetDxFrameDuration(hWnd : HWND; cRefreshes : INT) : HRESULT; stdcall;
  {$EXTERNALSYM DwmSetDxFrameDuration}
  function DwmSetPresentParameters(hWnd : HWND; var pPresentParams : DWM_PRESENT_PARAMETERS) : HRESULT; stdcall;
  {$EXTERNALSYM DwmSetPresentParameters}
  function DwmSetWindowAttribute(hWnd : HWND; dwAttribute : DWORD; pvAttribute : LPCVOID; cbAttribute : DWORD) : HRESULT; stdcall;
  {$EXTERNALSYM DwmSetWindowAttribute}
  function DwmUnregisterThumbnail(hThumbnailId : HTHUMBNAIL) : HRESULT; stdcall;
  {$EXTERNALSYM DwmUnregisterThumbnail}
  function DwmUpdateThumbnailProperties(hThumbnailId : HTHUMBNAIL; const ptnProperties : DWM_THUMBNAIL_PROPERTIES) : HRESULT; stdcall;
  {$EXTERNALSYM DwmUpdateThumbnailProperties}
  function DwmAttachMilContent(hWnd : HWND) : HRESULT; stdcall;
  {$EXTERNALSYM DwmAttachMilContent}
  function DwmDetachMilContent(hWnd : HWND) : HRESULT; stdcall;
  {$EXTERNALSYM DwmDetachMilContent}
  function DwmFlush() : HRESULT; stdcall;
  {$EXTERNALSYM DwmFlush}

type
  _MIL_MATRIX3X2D = record
    S_11  : Double;
    S_12  : Double;
    S_21  : Double;
    S_22  : Double;
    DX    : Double;
    DY    : Double;
  end;
  {$EXTERNALSYM _MIL_MATRIX3X2D}
  MIL_MATRIX3X2D = _MIL_MATRIX3X2D;
  {$EXTERNALSYM MIL_MATRIX3X2D}

  function DwmGetGraphicsStreamTransformHint(uIndex : UINT; out pTransform : MIL_MATRIX3X2D) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetGraphicsStreamTransformHint}
  function DwmGetGraphicsStreamClient(uIndex : UINT; out pClientUuid : TGUID) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetGraphicsStreamClient}
  function DwmGetTransportAttributes(hWnd : HWND; out pTimingInfo : DWM_TIMING_INFO) : HRESULT; stdcall;
  {$EXTERNALSYM DwmGetTransportAttributes}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  dwmlib = 'dwmapi.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _DwmDefWindowProc : Pointer;

function DwmDefWindowProc;
begin
  GetProcedureAddress(_DwmDefWindowProc, dwmlib, 'DwmDefWindowProc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmDefWindowProc]
  end;
end;

var
  _DwmEnableBlurBehindWindow: Pointer;

function DwmEnableBlurBehindWindow;
begin
  GetProcedureAddress(_DwmEnableBlurBehindWindow, dwmlib, 'DwmEnableBlurBehindWindow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmEnableBlurBehindWindow]
  end;
end;

var
  _DwmEnableComposition: Pointer;

function DwmEnableComposition;
begin
  GetProcedureAddress(_DwmEnableComposition, dwmlib, 'DwmEnableComposition');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmEnableComposition]
  end;
end;

var
  _DwmEnableMMCSS: Pointer;

function DwmEnableMMCSS;
begin
  GetProcedureAddress(_DwmEnableMMCSS, dwmlib, 'DwmEnableMMCSS');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmEnableMMCSS]
  end;
end;

var
  _DwmExtendFrameIntoClientArea: Pointer;

function DwmExtendFrameIntoClientArea;
begin
  GetProcedureAddress(_DwmExtendFrameIntoClientArea, dwmlib, 'DwmExtendFrameIntoClientArea');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmExtendFrameIntoClientArea]
  end;
end;

var
  _DwmGetColorizationColor: Pointer;

function DwmGetColorizationColor;
begin
  GetProcedureAddress(_DwmGetColorizationColor, dwmlib, 'DwmGetColorizationColor');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmGetColorizationColor]
  end;
end;

var
  _DwmGetCompositionTimingInfo: Pointer;

function DwmGetCompositionTimingInfo;
begin
  GetProcedureAddress(_DwmGetCompositionTimingInfo, dwmlib, 'DwmGetCompositionTimingInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmGetCompositionTimingInfo]
  end;
end;

var
  _DwmGetWindowAttribute: Pointer;

function DwmGetWindowAttribute;
begin
  GetProcedureAddress(_DwmGetWindowAttribute, dwmlib, 'DwmGetWindowAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmGetWindowAttribute]
  end;
end;

var
  _DwmIsCompositionEnabled: Pointer;

function DwmIsCompositionEnabled;
begin
  GetProcedureAddress(_DwmIsCompositionEnabled, dwmlib, 'DwmIsCompositionEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmIsCompositionEnabled]
  end;
end;

var
  _DwmModifyPreviousDxFrameD: Pointer;

function DwmModifyPreviousDxFrameDuration;
begin
  GetProcedureAddress(_DwmModifyPreviousDxFrameD, dwmlib, 'DwmModifyPreviousDxFrameDuration');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmModifyPreviousDxFrameD]
  end;
end;

var
  _DwmQueryThumbnailSourceSize: Pointer;

function DwmQueryThumbnailSourceSize;
begin
  GetProcedureAddress(_DwmQueryThumbnailSourceSize, dwmlib, 'DwmQueryThumbnailSourceSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmQueryThumbnailSourceSize]
  end;
end;

var
  _DwmRegisterThumbnail: Pointer;

function DwmRegisterThumbnail;
begin
  GetProcedureAddress(_DwmRegisterThumbnail, dwmlib, 'DwmRegisterThumbnail');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmRegisterThumbnail]
  end;
end;

var
  _DwmSetDxFrameDuration: Pointer;

function DwmSetDxFrameDuration;
begin
  GetProcedureAddress(_DwmSetDxFrameDuration, dwmlib, 'DwmSetDxFrameDuration');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmSetDxFrameDuration]
  end;
end;

var
  _DwmSetPresentParameters: Pointer;

function DwmSetPresentParameters;
begin
  GetProcedureAddress(_DwmSetPresentParameters, dwmlib, 'DwmSetPresentParameters');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmSetPresentParameters]
  end;
end;

var
  _DwmSetWindowAttribute: Pointer;

function DwmSetWindowAttribute;
begin
  GetProcedureAddress(_DwmSetWindowAttribute, dwmlib, 'DwmSetWindowAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmSetWindowAttribute]
  end;
end;

var
  _DwmUnregisterThumbnail: Pointer;

function DwmUnregisterThumbnail;
begin
  GetProcedureAddress(_DwmUnregisterThumbnail, dwmlib, 'DwmUnregisterThumbnail');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmUnregisterThumbnail]
  end;
end;

var
  _DwmUpdateThumbnailProperties: Pointer;

function DwmUpdateThumbnailProperties;
begin
  GetProcedureAddress(_DwmUpdateThumbnailProperties, dwmlib, 'DwmUpdateThumbnailProperties');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmUpdateThumbnailProperties]
  end;
end;

var
  _DwmAttachMilContent: Pointer;

function DwmAttachMilContent;
begin
  GetProcedureAddress(_DwmAttachMilContent, dwmlib, 'DwmAttachMilContent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmAttachMilContent]
  end;
end;

var
  _DwmDetachMilContent: Pointer;

function DwmDetachMilContent;
begin
  GetProcedureAddress(_DwmDetachMilContent, dwmlib, 'DwmDetachMilContent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmDetachMilContent]
  end;
end;

var
  _DwmFlush: Pointer;

function DwmFlush;
begin
  GetProcedureAddress(_DwmFlush, dwmlib, 'DwmFlush');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmFlush]
  end;
end;

var
{$IFDEF SUPPORT_LONG_VARNAMES}
  _DwmGetGraphicsStreamTransformHint: Pointer;
{$ELSE}
  _DwmGetGraphicsStreamTH: Pointer;
{$ENDIF}

function DwmGetGraphicsStreamTransformHint;
begin
{$IFDEF SUPPORT_LONG_VARNAMES}
  GetProcedureAddress(_DwmGetGraphicsStreamTransformHint, dwmlib, 'DwmGetGraphicsStreamTransformHint');
{$ELSE}
  GetProcedureAddress(_DwmGetGraphicsStreamTH, dwmlib, 'DwmGetGraphicsStreamTransformHint');
{$ENDIF}
  asm
        MOV     ESP, EBP
        POP     EBP
{$IFDEF SUPPORT_LONG_VARNAMES}
        JMP     [_DwmGetGraphicsStreamTransformHint]
{$ELSE}
        JMP     [_DwmGetGraphicsStreamTH]
{$ENDIF}
  end;
end;

var
  _DwmGetGraphicsStreamClient: Pointer;

function DwmGetGraphicsStreamClient;
begin
  GetProcedureAddress(_DwmGetGraphicsStreamClient, dwmlib, 'DwmGetGraphicsStreamClient');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmGetGraphicsStreamClient]
  end;
end;

var
  _DwmGetTransportAttributes: Pointer;

function DwmGetTransportAttributes;
begin
  GetProcedureAddress(_DwmGetTransportAttributes, dwmlib, 'DwmGetTransportAttributes');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DwmGetTransportAttributes]
  end;
end;




{$ELSE}

function DwmDefWindowProc; external dwmlib name 'DwmDefWindowProc';
function DwmEnableBlurBehindWindow; external dwmlib name 'DwmEnableBlurBehindWindow';
function DwmEnableComposition; external dwmlib name 'DwmEnableComposition';
function DwmEnableMMCSS; external dwmlib name 'DwmEnableMMCSS';
function DwmExtendFrameIntoClientArea; external dwmlib name 'DwmExtendFrameIntoClientArea';
function DwmGetColorizationColor; external dwmlib name 'DwmGetColorizationColor';
function DwmGetCompositionTimingInfo; external dwmlib name 'DwmGetCompositionTimingInfo';
function DwmGetWindowAttribute; external dwmlib name 'DwmGetWindowAttribute';
function DwmIsCompositionEnabled; external dwmlib name 'DwmIsCompositionEnabled';
function DwmModifyPreviousDxFrameDuration; external dwmlib name 'DwmModifyPreviousDxFrameDuration';
function DwmQueryThumbnailSourceSize; external dwmlib name 'DwmQueryThumbnailSourceSize';
function DwmRegisterThumbnail; external dwmlib name 'DwmRegisterThumbnail';
function DwmSetDxFrameDuration; external dwmlib name 'DwmSetDxFrameDuration';
function DwmSetPresentParameters; external dwmlib name 'DwmSetPresentParameters';
function DwmSetWindowAttribute; external dwmlib name 'DwmSetWindowAttribute';
function DwmUnregisterThumbnail; external dwmlib name 'DwmUnregisterThumbnail';
function DwmUpdateThumbnailProperties; external dwmlib name 'DwmUpdateThumbnailProperties';
function DwmAttachMilContent; external dwmlib name 'DwmAttachMilContent';
function DwmDetachMilContent; external dwmlib name 'DwmDetachMilContent';
function DwmFlush; external dwmlib name 'DwmFlush';
function DwmGetGraphicsStreamTransformHint; external dwmlib name 'DwmGetGraphicsStreamTransformHint';
function DwmGetGraphicsStreamClient; external dwmlib name 'DwmGetGraphicsStreamClient';
function DwmGetTransportAttributes; external dwmlib name 'DwmGetTransportAttributes';


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
