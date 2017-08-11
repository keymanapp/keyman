//***********************************************************
//                          TEdithost                       *
//                                                          *
//                     Freeware Component                   *
//                       For Delphi                         *
//                            by                            *
//                     Per Lindsø Larsen                    *                                                       //                   per.lindsoe@larsen.dk                  *
//                  and bsalsa@gmail.com                    *
//  Documentation and updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************
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

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: Edithost.pas,v 1.3.2.1 2006/11/29 22:13:00 sergev Exp $

unit Edithost;

interface

{$I EWB.inc}

uses
  Windows, MSHTML_EWB, Classes, Controls;

const
  S_OK = 0;
{$EXTERNALSYM S_OK}
  S_FALSE = $00000001;
{$EXTERNALSYM S_FALSE}
  SID_SHTMLEditHost: TGUID = (D1: $3050F6A0; D2: $98B5; D3: $11CF; D4: ($BB,
    $82, $00, $AA, $00, $BD, $CE, $0B));

type

  TSnapRect = function(const pIElement: IHTMLElement; var prcNew: TRECT;
    eHandle: _ELEMENT_CORNER): HResult of object;
  TPreDrag = function: HResult of object;

  TEditHost = class(TComponent,
      IUnknown, // http://msdn.microsoft.com/en-us/library/ms680509(VS.85).aspx
      IHTMLEditHost, // http://msdn.microsoft.com/en-us/library/aa704054.aspx
      IHTMLEditHost2 // http://msdn.microsoft.com/en-us/library/aa704052(VS.85).aspx
      )

  private
    { Private declarations }
    FAbout: string;
    FSnapRect: TSnapRect;
    FPreDrag: TPreDrag;
    FEnabled: Boolean;
    procedure SetAbout(const Value: string);
  protected
    { Protected declarations }
     {IHTMLEditHost2}
    function PreDrag: HResult; stdcall;
    {IHTMLEditHost}
    function SnapRect(const pIElement: IHTMLElement; var prcNew: TRECT; eHandle:
      _ELEMENT_CORNER): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    { Published declarations }
    property About: string read FAbout write SetAbout;
    property OnSnapRect: TSnapRect read FSnapRect write FSnapRect;
    property OnPreDrag: TPreDrag read FPreDrag write FPreDrag;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

implementation

{ TEditHost }

constructor TEditHost.Create;
begin
  FAbout := 'TEditHost - Help & Support http://www.bsalsa.com/';
  FEnabled := True;
  inherited;
end;

destructor TEditHost.Destroy;
begin
  inherited Destroy;
end;

function TEditHost.SnapRect(const pIElement: IHTMLElement;
  var prcNew: TRECT; eHandle: _ELEMENT_CORNER): HResult;
begin
  Result := S_OK;
  if FEnabled then
    if Assigned(FSnapRect) and FEnabled then
      Result := FSnapRect(pIElement, prcNew, eHandle);
end;

function TEditHost.PreDrag: HResult;
begin
  Result := S_OK;
  if FEnabled then
    if Assigned(FPreDrag) and FEnabled then
      Result := FPreDrag;
end;

procedure TEditHost.SetAbout(const Value: string);
begin
  Exit;
end;

end.
