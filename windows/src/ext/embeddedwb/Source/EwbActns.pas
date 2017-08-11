//*************************************************************
//                          EwbActns                          *
//                                                            *
//                     Freeware unit                          *
//                       For Delphi                           *
//                            by                              *
//                     Serge Voloshenyuk                      *
//      Developing Team:                                      *
//          Serge Voloshenyuk (SergeV@bsalsa.com)             *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)       *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
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

You may use/ change/ modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: EwbActns.pas,v 1.1.2.1 2006/11/29 22:13:00 sergev Exp $

unit EwbActns;

{$I EWB.inc}

interface

uses
  Classes, ActnList, EwbCore;

type
  TEwbAction = class(TCustomAction)
  private
    FControl: TEwbCore;
    procedure SetControl(Value: TEwbCore);
  protected
    function GetControl(Target: TObject): TEwbCore; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    property Control: TEwbCore read FControl write SetControl;
  end;

  TEwbLinkAction = class(TEwbAction)
  private
    FURL: WideString;
    procedure setURL(const Value: WideString);
  public
    procedure ExecuteTarget(Target: TObject); override;
    constructor Create(AOwner: TComponent); override;
  published
    property URL: WideString read FURL write setURL;
    property Caption;
    property HelpContext;
{$IFDEF DELPHI6_UP}
    property HelpKeyword;
    property HelpType;
    property SecondaryShortCuts;
{$ENDIF}
    property Hint;
    property ImageIndex;
    property ShortCut;
    property Visible;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

implementation

{ TEwbAction }

destructor TEwbAction.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  inherited;
end;

function TEwbAction.GetControl(Target: TObject): TEwbCore;
begin
  Result := Target as TEwbCore;
end;

function TEwbAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TEwbCore))
    { and TEwbCore(Target).Focused}; //FIXME
end;

procedure TEwbAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TEwbAction.SetControl(Value: TEwbCore);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

{ TEwbLinkAction }

constructor TEwbLinkAction.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := False;
end;

procedure TEwbLinkAction.ExecuteTarget(Target: TObject);
begin
  GetControl(Target).Navigate(Self.URL);
end;

procedure TEwbLinkAction.setURL(const Value: WideString);
begin
  FURL := Value;
  Enabled := FURL <> '';
end;

end.
