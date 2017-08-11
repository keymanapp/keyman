//***********************************************************
//                      URL  History  demo                  *
//                                                          *
//               For Delphi 5 - 2009                        *
//                     Freeware demo                        *
//                                                          *
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
4. Please consider donation in our web site!
{*******************************************************************************}

unit UrlhistoryDemo_U;

interface

uses
  Windows, Messages, EwbAcc, SysUtils, Forms, StdCtrls, Grids, UrlHistory,
  Controls, Classes, ExtCtrls;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    UrlHistory1: TUrlHistory;
    CheckBox1: TCheckBox;
    Panel2: TPanel;
    ComboSortBy: TComboBox;
    Label4: TLabel;
    Button1: TButton;
    Label3: TLabel;
    edSearchText: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure UrlHistory1Accept(Title, Url: string; LastVisited,
      LastUpdated, Expires: TDateTime; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  iTotalURLs: Integer;
  iRow: Integer;
begin
  with UrlHistory1 do begin
    case ComboSortBy.ItemIndex of
      0: SortField := sfLastvisited;
      1: SortField := sfTitle;
      2: SortField := sfUrl;
      3: SortField := sfLastUpdated;
      4: SortField := sfExpires;
    end;
  end;

  with StringGrid1 do
  begin
    Perform(WM_SETREDRAW, 0, 0); // prevent updating
    RowCount := 2;
    try
      UrlHistory1.Search := edSearchText.text;
      Cells[0, 0] := 'Last Visited';
      Cells[1, 0] := 'Title';
      Cells[2, 0] := 'Url';
      Cells[3, 0] := 'Last Updated';
      Cells[4, 0] := 'Expires';

      iTotalURLs := UrlHistory1.Enumerate;
      for iRow := 0 to iTotalURLs - 1 do
      begin
        RowCount := iRow + 2;
        Cells[0, iRow + 1] := DateTimeToStr(PEntry(Urlhistory1.Items[iRow]).LastVisited);
        Cells[1, iRow + 1] := PEntry(Urlhistory1.Items[iRow]).Title;
        Cells[2, iRow + 1] := PEntry(Urlhistory1.Items[iRow]).Url;
        Cells[3, iRow + 1] := DateTimeToStr(PEntry(Urlhistory1.Items[iRow]).LastUpdated);
        Cells[4, iRow + 1] := DateTimeToStr(PEntry(Urlhistory1.Items[iRow]).Expires);
      end;
    finally
      Perform(WM_SETREDRAW, 1, 0);
      Invalidate;
    end;
  end;
end;

procedure TForm1.UrlHistory1Accept(Title, Url: string; LastVisited,
  LastUpdated, Expires: TDateTime; var Accept: Boolean);
begin
  if Checkbox1.checked and (LastVisited < Now - 1) then Accept := False;
end;

end.

