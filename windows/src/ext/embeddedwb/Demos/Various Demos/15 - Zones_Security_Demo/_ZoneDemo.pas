//*************************************************************
//                    Zones & Security Demo                   *
//                                                            *
//                       For Delphi                           *
//                            by                              *
//                     Per Lindsø Larsen                      *
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
4. Please, consider donation in our web site!
{*******************************************************************************}

unit _ZoneDemo;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IEConst,
  Registry, ActiveX, SecurityManager, ShellApi, Imglist, Urlmon, StdCtrls,
  shdocvw_EWB, ComCtrls, OleCtrls, Grids, ExtCtrls;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    Label1: TLabel;
    CurrentImage: TImage;
    CurrentDisplay: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    MinimumImage: TImage;
    MinimumDisplay: TLabel;
    Label6: TLabel;
    Panel3: TPanel;
    RecommImage: TImage;
    RecommDisplay: TLabel;
    Panel4: TPanel;
    MemoURLPatterns: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    ListView1: TListView;
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var
  ZoneManager: IInternetZoneManager;
  SecManager: IInternetSecurityManager;
  ZoneAttrib: TZoneAttributes;
  ZoneEnum: DWORD;
  ImageList: TImageList;
  MinimumIcon: TIcon;
  RecommIcon: TIcon;
  CurrentIcon: TIcon;

procedure GetIcon(IconPath: string; var Icon: TIcon);
var
  FName, ImageName: string;
  h: hInst;
begin
  FName := Copy(IconPath, 1, Pos('#', IconPath) - 1);
  ImageName := Copy(IconPath, Pos('#', IconPath), Length(IconPath));
  h := LoadLibrary(Pchar(FName));
  try
    if h <> 0 then
      Icon.Handle := LoadImage(h, Pchar(ImageName), IMAGE_ICON, 16, 16, 0);
  finally
    FreeLibrary(h);
  end;
end;

procedure GetTemplateInfo(dwTemplate: DWORD; var IconPath: string; var DisplayName: string);
const
  KEY_TEMPLATES_POLICIES = 'Software\Microsoft\Windows\CurrentVersion\Internet Settings\TemplatePolicies\';
var
  Reg: TRegistry;
  Template: string;
begin
  Template := '';
  if dwTemplate = URLTEMPLATE_HIGH then
    Template := 'High'
  else
    if dwTemplate = URLTEMPLATE_MEDLOW then
      Template := 'MedLow'
    else
      if dwTemplate = URLTEMPLATE_LOW then
        Template := 'Low'
      else
        if dwTemplate = URLTEMPLATE_MEDIUM then
          Template := 'Medium';
  if Template <> '' then
 // We need to use the registry to get information about urltemplates
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey(KEY_TEMPLATES_POLICIES + Template, FALSE);
      IconPath := Reg.ReadString('Icon');
      DisplayName := Reg.ReadString('DisplayName');
    finally
      Reg.Free;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Icon: TIcon;
  ListItem: TListItem;
  Zone, ZoneCounter, TotalZones: DWord;
begin
  MinimumIcon := TIcon.Create;
  RecommIcon := TIcon.Create;
  CurrentIcon := TIcon.Create;
  ImageList := TImagelist.Create(self);
  ListView1.ViewStyle := vsReport;
  // Create instance of InternetSecurityManager
  CoInternetCreateSecuritymanager(nil, SecManager, 0);
  // Create instance of InternetSecurityZoneManager
  CoInternetCreateZoneManager(nil, ZoneManager, 0);
  // Enumerate securityzones
  ZoneManager.CreateZoneEnumerator(ZoneEnum, TotalZones, 0);
  Icon := TIcon.Create;
  try
    for ZoneCounter := 0 to Pred(TotalZones) do
    begin
      ZoneManager.GetZoneAt(ZoneEnum, ZoneCounter, Zone);
      // GetZoneAttributes retrieves icon and displaytext for the selected zone
      ZoneManager.GetZoneAttributes(Zone, ZoneAttrib);
      ListItem := Listview1.Items.Add;
      ListItem.Caption := ZoneAttrib.szDisplayname;
      Listitem.ImageIndex := ZoneCounter;
      if ZoneCounter = 0 then
        Listview1.Selected := listitem;
      GetIcon(ZoneAttrib.szIconPath, Icon);
      ImageList.AddIcon(Icon);
    end;
  finally
    Icon.Free;
  end;
  Listview1.SmallImages := Imagelist;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImageList.Free;
  MinimumIcon.Free;
  RecommIcon.Free;
  CurrentIcon.Free;
   //Destroy the ZoneEnumerator
  ZoneManager.DestroyZoneEnumerator(ZoneEnum);
end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  DisplayName, IconPath: string;
  Row: Integer;
  cbPolicy, Counter, Policy: Dword;
  Enum: IEnumString;
  Fetched: UInt;
  Zone: DWord;
  Pattern: POleStr;
begin
  Application.ProcessMessages;
  if selected then
  begin
    ZoneManager.GetZoneAt(ZoneEnum, Listview1.Items.IndexOf(Item), Zone);
    MemoURLPatterns.Lines.BeginUpdate;
    try
      MemoURLPatterns.Lines.Clear;
    //GetZoneMappings retrieve urlpatterns or sites added to the selected zone
      SecManager.GetZoneMappings(Zone, enum, 0);
      while Succeeded(Enum.Next(1, Pattern, @fetched)) and (fetched = 1) do
        MemoURLPatterns.lines.Add(Pattern);
    finally
      MemoURLPatterns.Lines.EndUpdate;
    end;
    Enum := nil;
    Pattern := nil;
    Row := 0;
    cbPolicy := SizeOf(dWord);
    //GetZoneActionPolicy retrieves Urlpolicy for the 25 default UrlActions in each template.
    for Counter := 0 to 24 do
    begin
      ZoneManager.GetZoneActionPolicy(Zone,
        DefaultActions[Counter], @policy, cbPolicy, 0);
      StringGrid1.RowCount := Row + 1;
      Stringgrid1.Cells[0, Row] := DisplayAction(DefaultActions[Counter]);
      Stringgrid1.Cells[1, Row] := DisplayPolicy(DefaultActions[Counter], Policy);
      Inc(Row);
    end;
    ZoneManager.GetZoneAttributes(Zone, ZoneAttrib);
    // If avaiable then get icon and displaytext for the urltemplate
    //Minimum urltemplate
    GetTemplateInfo(ZoneAttrib.dwTemplateMinLevel, IconPath, DisplayName);
    GetIcon(IconPath, MinimumIcon);
    MinimumImage.Picture.Icon := MinimumIcon;
    MinimumDisplay.Caption := Displayname;
    // Current Urltemplate
    GetTemplateInfo(ZoneAttrib.dwTemplateCurrentLevel, IconPath, DisplayName);
    GetIcon(IconPath, CurrentIcon);
    CurrentImage.Picture.Icon := CurrentIcon;
    CurrentDisplay.Caption := Displayname;
    //Recommended urltemplate
    GetTemplateInfo(ZoneAttrib.dwTemplateRecommended, IconPath, DisplayName);
    GetIcon(IconPath, RecommIcon);
    RecommImage.Picture.Icon := RecommIcon;
    recommDisplay.Caption := Displayname;
  end;
end;

end.

