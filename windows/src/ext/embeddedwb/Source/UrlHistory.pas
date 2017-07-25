//**************************************************************
//                                                             *
//                      UrlHistory                             *                                                      *
//                     For Delphi 5 to XE                      *
//                     Freeware Component                      *
//                            by                               *
//                     Per Lindsø Larsen                       *
//                   per.lindsoe@larsen.dk                     *
//                                                             *
//  Contributions:                                             *
//  Eran Bodankin (bsalsa) bsalsa@gmail.com                    *
//                                                             *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************

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
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: UrlHistory.pas,v 1.2 2006/11/15 21:01:44 sergev Exp $

unit UrlHistory;

interface

{$I EWB.inc}

uses
  Windows, SysUtils, Classes, ActiveX, ComObj, EwbAcc;


type
  TSortDirectionOption = (sdAscending, sdDescending);
  TSortFieldOption = (sfTitle, sfURL, sfLastVisited, sfLastUpdated, sfExpires);
  TSearchFieldOption = (seBoth, seTitle, seURL);

type

  TOnAcceptEvent = procedure(Title, Url: string; LastVisited, LastUpdated, Expires: TDateTime;
    var Accept: Boolean) of object;

  TOnDeleteEvent = procedure(Title, Url: string; LastVisited, LastUpdated, Expires: TDateTime;
    var Delete: Boolean) of object;

  TUrlHistory = class(TComponent)
  private
    { Private declarations }
    FSearch: string;
    FAccept: Boolean;
    FDelete: Boolean;
    FOnDelete: TOnDeleteEvent;
    FOnAccept: TOnAcceptEvent;
    FSortDirection: TSortDirectionOption;
    FSortField: TSortFieldOption;
    FSearchField: TSearchFieldOption;
    Stg: IUrlHistoryStg2;
    Enum: IEnumStatUrl;
  protected
    { Protected declarations }
    procedure ClearList;
    procedure Accept(Title, Url: string; LastVisited, LastUpdated, Expires: TDateTime;
      var Accept: Boolean);
    procedure Delete(Title, Url: string; LastVisited, LastUpdated, Expires: TDateTime;
      var Delete: Boolean);
    procedure Loaded; override;
  public
    Items: TList;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Enumerate: Integer;
    function DeleteEntries: Integer;
    function AddUrl(Url: PWideChar; Title: PWideChar): HRESULT;
    function DeleteUrl(Url: PWideChar): HRESULT;
    function QueryUrl(Url: PWideChar; var Entry: TEntry): HRESULT;
    function ClearHistory: HRESULT;

  published
    { Published declarations }
    property OnAccept: TOnAcceptEvent read FOnAccept write FOnAccept;
    property OnDelete: TOnDeleteEvent read FOnDelete write FOnDelete;
    property SortField: TSortFieldOption read FSortField write FSortField;
    property SearchField: TSearchFieldOption read FSearchField write FSearchField;
    property Search: string read FSearch write FSearch;
    property SortDirection: TSortDirectionOption read FSortDirection write FSortDirection;
  end;

implementation

var
  Ascending: Boolean;

function TitleSortFunc(Item1, Item2: Pointer): Integer;
begin
  if ((PEntry(Item1).Title < PEntry(Item2).Title) and Ascending)
    or ((PEntry(Item1).Title > PEntry(Item2).Title) and not Ascending) then
    Result := -1
  else
    if PEntry(Item1).Title = PEntry(Item2).Title then
      Result := 0
    else
      Result := 1;
end;

function UrlSortFunc(Item1, Item2: Pointer): Integer;
begin
  if ((PEntry(Item1).Url < PEntry(Item2).Url) and Ascending)
    or ((PEntry(Item1).Url > PEntry(Item2).Url) and not Ascending) then
    Result := -1
  else
    if PEntry(Item1).Url = PEntry(Item2).Url then
      Result := 0
    else
      Result := 1;
end;

function LastVisitedSortFunc(Item1, Item2: Pointer): Integer;
begin
  if ((PEntry(Item1).LastVisited < PEntry(Item2).LastVisited) and Ascending)
    or ((PEntry(Item1).LastVisited > PEntry(Item2).LastVisited) and not Ascending)
    then
    Result := -1
  else
    if PEntry(Item1).LastVisited = PEntry(Item2).LastVisited then
      Result := 0
    else
      Result := 1;
end;

function LastUpdatedSortFunc(Item1, Item2: Pointer): Integer;
begin
  if ((PEntry(Item1).LastUpdated < PEntry(Item2).LastUpdated) and Ascending)
    or ((PEntry(Item1).LastUpdated > PEntry(Item2).LastUpdated) and not Ascending) then
    Result := -1
  else
    if PEntry(Item1).LastUpdated = PEntry(Item2).LastUpdated then
      Result := 0
    else
      Result := 1;
end;

function ExpiresSortFunc(Item1, Item2: Pointer): Integer;
begin
  if ((PEntry(Item1).Expires < PEntry(Item2).Expires) and Ascending)
    or ((PEntry(Item1).Expires > PEntry(Item2).Expires) and not Ascending) then
    Result := -1
  else
    if PEntry(Item1).Expires = PEntry(Item2).Expires then
      Result := 0
    else
      Result := 1;
end;

function FileTimeToDt(Ft: TFileTime): TDateTime;
var
  l: Integer;
  lft: TFileTime;
begin
  FileTimeToLocalFiletime(Ft, lft);
  if FileTimeToDosDateTime(lft, Longrec(l).Hi, Longrec(l).Lo) then
    Result := FileDateToDatetime(l)
  else
    Result := 0;
end;

{ TUrlHistory }

function TUrlHistory.AddUrl(Url, Title: PWideChar): HRESULT;
begin
  Result := Stg.AddUrl(Url, Title, 0);
end;

function TUrlHistory.ClearHistory: HRESULT;
begin
  Result := Stg.ClearHistory;
end;

constructor TUrlHistory.Create(AOwner: TComponent);
begin
  inherited;
end;

function TUrlHistory.DeleteUrl(Url: PWideChar): HRESULT;
begin
  Result := stg.DeleteUrl(Url, 0);
end;

procedure TUrlHistory.ClearList;
var
  I: Integer;
begin
  if Items <> nil then
  begin
    for I := 0 to Pred(Items.Count) do
      Dispose(PEntry(Items[i]));
    Items.Clear;
  end;
end;

function TUrlHistory.Enumerate: Integer;
var
  staturl: TStaturl;
  title, Url: string;
  Entry: PEntry;
  Fetched: Integer;
begin
  ClearList;
  Stg.EnumUrls(Enum);
  while Enum.Next(1, StatUrl, @Fetched) = S_OK do
  begin
    Url := PWideChar(Pointer(Staturl.pwcsUrl));
    Title := PWideChar(Pointer(Staturl.pwcsTitle));
    if FSearch <> '' then
      if ((FSearchField = seUrl) and (Pos(FSearch, Url) = 0)) or
        ((FSearchField = seTitle) and (Pos(FSearch, Title) = 0)) or
        ((FSearchField = seBoth) and ((Pos(FSearch, Url) = 0)) or (Pos(FSearch, Title) = 0)) then
        Continue;
    Entry := New(PEntry);
    Entry.Url := Url;
    Entry.Title := Title;
    Entry.Lastvisited := FileTimeToDt(Staturl.ftLastVisited);
    Entry.LastUpdated := FileTimeToDt(Staturl.ftLastUpdated);
    Entry.Expires := FileTimeToDt(Staturl.ftExpires);
    FAccept := True;
    if Assigned(FOnAccept) then
      FOnAccept(Entry.Title, Entry.Url, Entry.LastVisited, Entry.LastUpdated, Entry.Expires, FAccept);
    if FAccept then
      Items.Add(Entry);
  end;
  Ascending := BOOL(FSortDirection = sdAscending);
  case FSortField of
    sfTitle: Items.Sort(TitleSortFunc);
    sfUrl: Items.Sort(UrlSortFunc);
    sfLastVisited: Items.Sort(LastVisitedSortFunc);
    sfLastUpdated: Items.Sort(LastUpdatedSortFunc);
    sfExpires: Items.Sort(ExpiresSortFunc);
  end;
  Result := Items.Count;
end;

procedure TUrlHistory.Loaded;
const
  CLSID_CUrlHistory: TGUID = '{3C374A40-BAE4-11CF-BF7D-00AA006946EE}';
begin
  inherited;
  Items := TList.Create;
  Stg := CreateComObject(CLSID_CUrlHistory) as IUrlHistoryStg2;
end;

destructor TUrlHistory.Destroy;
begin
  Clearlist;
  if Items <> nil then
    Items.Free;
  inherited;
end;

procedure TUrlHistory.Accept(Title, Url: string; LastVisited, LastUpdated,
  Expires: TDateTime; var Accept: Boolean);
begin
//
end;

procedure TUrlHistory.Delete(Title, Url: string; LastVisited, LastUpdated,
  Expires: TDateTime; var Delete: Boolean);
begin
//
end;

function TUrlHistory.DeleteEntries: Integer;
var
  StatUrl: TStatUrl;
  Fetched: Integer;
begin
  Result := 0;
  Stg.EnumUrls(Enum);
  while enum.Next(1, StatUrl, @Fetched) = S_OK do
  begin
    FDelete := False;
    if Assigned(FOnDelete) then
      FOnDelete(PWideChar(Pointer(Staturl.pwcsUrl)),
        PWideChar(Pointer(Staturl.pwcsTitle)),
        FileTimeToDt(Staturl.ftLastVisited),
        FileTimeToDt(Staturl.ftLastUpdated),
        FileTimeToDt(Staturl.ftExpires),
        FDelete);
    if FDelete then
    begin
      Stg.DeleteUrl(PWideChar(Pointer(Staturl.pwcsUrl)), 0);
      Inc(Result);
    end;
  end;
end;

function TUrlHistory.QueryUrl(Url: PWideChar; var Entry: TEntry): HRESULT;
var
  StatUrl: TSTATURL;
begin
  Result := Stg.QueryUrl(Url, 0, StatUrl);
  if Result = S_OK then
  begin
    Entry.Url := PWideChar(Pointer(Staturl.pwcsUrl));
    Entry.Title := PWideChar(Pointer(Staturl.pwcsTitle));
    Entry.LastVisited := FileTimeToDt(Staturl.ftLastVisited);
    Entry.LastUpdated := FileTimeToDt(Staturl.ftLastUpdated);
    Entry.Expires := FileTimeToDt(Staturl.ftExpires);
  end;
end;

end.

