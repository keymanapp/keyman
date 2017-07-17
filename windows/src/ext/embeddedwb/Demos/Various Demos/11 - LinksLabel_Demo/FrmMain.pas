//***********************************************************
//                       TLinksLabel  demo                  *
//                                                          *
//               For Delphi 5 - 2010                        *
//                     Freeware demo                        *
// By:  Eran Bodankin (bsalsa)   bsalsa@bsalsa.com          *
//           Documentation and updated versions:            *
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
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit FrmMain;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, LinksLabel, ImgList;

type
  TForm2 = class(TForm)
    LinksLabel1: TLinksLabel;
    LinksLabel2: TLinksLabel;
    LinksLabel3: TLinksLabel;
    LinksLabel4: TLinksLabel;
    ImageList1: TImageList;
    LinksLabel5: TLinksLabel;
    procedure LinksLabel1Launch(var Success: Boolean; const ErrorText: string;
      var Result: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.LinksLabel1Launch(var Success: Boolean;
  const ErrorText: string; var Result: Cardinal);
begin
  if not Success then
    ShowMessage(ErrorText);
end;


end.

