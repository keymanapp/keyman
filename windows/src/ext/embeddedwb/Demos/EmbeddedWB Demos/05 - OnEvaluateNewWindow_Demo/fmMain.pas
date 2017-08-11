//*************************************************************
//    EmbeddedWB - OnEvaluateNewWindow Event Demo             *
//                                                            *
//                            by                              *
//                     Thomas Stutz (smot)                    *
//                     smot777@yahoo.com                      *
//                                                            *
//     Documentation and updated versions:                    *
//               http://www.bsalsa.com                        *
//*************************************************************
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the demo under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please,  consider donation in our web site!
{*******************************************************************************}

unit fmMain;

interface

uses
  Classes, Windows, Controls, Forms, ComCtrls, Dialogs, ExtCtrls, StdCtrls, OleCtrls,
  IEAddress, EwbCore, EmbeddedWB, ShDocVw_Ewb;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    btnGo: TButton;
    IEAddress1: TIEAddress;
    StatusBar1: TStatusBar;
    EmbeddedWB1: TEmbeddedWB;
    Label1: TLabel;
    Label2: TLabel;
    procedure EmbeddedWB1EvaluateNewWindow(Sender: TCustomEmbeddedWB;
      pszUrl, pszName, pszUrlContext, pszFeatures: PWideChar;
      fReplace: LongBool; dwFlags, dwUserActionTime: Cardinal;
      var Rezult: HRESULT);
  private
    { Private declarations }

  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.EmbeddedWB1EvaluateNewWindow(Sender: TCustomEmbeddedWB;
  pszUrl, pszName, pszUrlContext, pszFeatures: PWideChar;
  fReplace: LongBool; dwFlags, dwUserActionTime: Cardinal;
  var Rezult: HRESULT);
(*
pszUrl
- A pointer to a buffer that contains the URL of the content that will be displayed in the new window.
pszName
- A pointer to a buffer that contains the name of the new window. This parameter can be NULL.
pszUrlContext
- A pointer to a buffer that contains the URL that has issued the command to open the new window.
pszFeatures
- A pointer to a buffer that contains the feature string for the new window. This value can be NULL.
fReplace
- A boolean value used when the new content specified in pszUrl is loaded into the existing window instead of creating a new one. TRUE if the new document should replace the current document in the history list; FALSE if the new document should be given a new entry.
dwFlags
- A flag or flags from the NWMF enumeration that provide situational information about the call to open the new window. This value can be 0 if no flags are needed.
dwUserActionTime
- The tick count when the last user action occurred. To find out how long ago the action occurred, call GetTickCount and compare the result with the value in this parameter.
*)
begin
  if GetTickCount - dwUserActionTime > 1000 then // Check if last User Action was longer than 1 Sec. ago
  begin
    ShowMessage('Popup blocked! ' + 'URL: ' + pszUrl + ' - Name: ' + pszName);
    Rezult := S_FALSE; //Block display of the window;
  end else
  begin
    Rezult := S_OK; // Allow display of the window.
  end;
end;

end.

