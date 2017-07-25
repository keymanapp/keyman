//*************************************************************
//      EWB.inc - Embedded Webbrowser definitions             *
//                                                            *
//               For Delphi 5 -Delphi XE                      *
//                     Freeware unit                          *
//                            by                              *
//                     Eran Bodankin (bsalsa)                 *
//                     bsalsa@bsalsa.com                      *
//                                                            *
//     Documentation and updated versions:                    *
//               http://www.bsalsa.com                        *
//*************************************************************
{*******************************************************************************}
{

LICENSE:

THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. [YOUR NAME] DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. VSOFT SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!  
 
{*******************************************************************************}
{$A+,B-,C-,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}


{$I EWB_jedi.inc}

// Specify the modules to include:
// To remove the extras, just put a dot in front of the $DEFINE. E.g. "{.$DEFINE USE_EwbTools}"
// To include an extra, remove the dot.
// Then re-compile the package.


//---- EmbeddedWB.pas: ---------------------------------------------------------

// ** Activated by default:

// Use functions from EwbTools.pas as methods in EmbeddedWB
{$DEFINE USE_EwbTools}

// Enable Subclassing of TEmbeddedWB Child windows (focus issues, ActiveControl, TAB and ActiveControl)
{$DEFINE Enable_SubClassChildWindows}

// Enable Mouse Hook to catch WM_MOUSEWHEEL and set Focus on TEmbeddedWB while not active
{$DEFINE Enable_AutoMouseWheelFix}

// Enable FocusControl to catch CM_ACTIVATE (EWB focus issue when switching back to the application)
{$DEFINE Enable_AutoFocusControl}

// Enable FormWndProc "Hooking" to handle Dialog Boxes / Popups
{$DEFINE Enable_HookParentFormWndProc}


// ** Deactivated by default:


// Enable DDE Support EwbDDE.pas
{.$DEFINE USE_EwbDDE}

// Enable MSHTML events (MSHTMLEvents.pas)
{.$DEFINE Enable_EwbMSHTMLEvents}

// Enable Mouse (OnEnter, OnLeave) Events  in TEmbeddedWB (D5, D6, D7 only)
{.$DEFINE Enable_MouseEnterLeaveEvents}

// Enable OleCtrls Fix (experimental)
{.$DEFINE USE_OleCtrlsFix}

// Enable Debugging
{.$DEFINE Enable_Debug}


//---- EwbCore.pas ---------------------------------------------------------

{$DEFINE USE_IOLECOMMANDTARGET}

{.$DEFINE GETKEYPATH_HANDLERS}

{.$DEFINE RESEARCH_MODE}

//-------------------------------------------------------------------------


//---- TEwbControl (EwbControlComponent.pas)-------------------------------

  // Enable Property InternetFeatures in the Object Inspector.
  {$DEFINE Enable_InternetFeatures}

//-------------------------------------------------------------------------


// Do not change:

{$IFDEF Enable_AutoFocusControl}
  {$DEFINE AutoUse_EwbControl}
{$ENDIF Enable_AutoFocusControl}

{$IFDEF Enable_AutoMouseWheelFix}
  {$DEFINE AutoUse_EwbControl}
{$ENDIF Enable_AutoMouseWheelFix}