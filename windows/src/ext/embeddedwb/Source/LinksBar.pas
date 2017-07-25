//***********************************************************
//                        LinksBar                          *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                     Eran Bodankin (bsalsa)               *
//                     bsalsa@gmail.com                     *
//                                                          *
//                                                          *
//     Documentation and updated versions:                  *
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
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: LinksBar.pas,v 1.2 2006/11/15 21:01:43 sergev Exp $

unit LinksBar;

interface

uses
  Classes, Controls, ComCtrls, SHDocVw_EWB, EmbeddedWB;

type
  TLinksBar = class(TToolbar)
    TLinksBar: TToolBar;

  private
    FAbout: string;
    FMaxCaptionLength: integer;
    FCount: Integer;
    FShown: Boolean;
    FShowImages: Boolean;
    FTitle: string;
    FUrl: string;
    FAutoNavigate: Boolean;
    LinkList: TStrings;
    URLlist: TStringList;
    FWebBrowser: TEmbeddedWB;
    FStatusBar: TStatusBar;
    LinksButton: TToolButton;
    procedure OnLinkClick(Sender: TObject);
    procedure SetShown(Value: Boolean);
    procedure SetAbout(Value: string);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(aOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure AddToLinksList(Title, URL: string);
    procedure RemoveFromLinksList(Title: string);
    procedure GetTheLinksList(LinkList: Tstrings);
    procedure GetTheLinksURLs(URLList: Tstrings);
    procedure NavigateToItem(Title: string);
    procedure CreateLinkButtons(ImageIdx: integer);
    procedure ClearTheLinksList;
  published
    property AutoNavigate: Boolean read fAutoNavigate write fAutoNavigate default True;
    property About: string read fAbout write SetAbout;
    property MaxCaptionLength: Integer read fMaxCaptionLength write fMaxCaptionLength;
    property Count: Integer read fCount;
    property ShowImages: boolean read fShowImages write fShowImages default True;
    property Shown: Boolean read fShown write SetShown default True;
    property WebBrowser: TEmbeddedWB read FWebBrowser write FWebBrowser;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
  end;

var
  fMaxL: integer;

implementation

uses
  Windows, Messages, Forms, Dialogs, Registry;

var
  FLockClientUpdateCount: Integer = 0;

// *****************************************************************************

constructor TLinksBar.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAbout := 'LinksBar by bsalsa : bsalsa@gmail.com';
  Visible := True;
  FShown := True;
  FShowImages := True;
  FMaxCaptionLength := 15;
  ShowCaptions := True;
  ShowHint := True;
  AutoSize := True;
  Flat := True;
  FAutoNavigate := True;
  Height := 26;
  LinkList := TStringList.Create;
  with LinkList do
  begin
{$IFDEF DELPHI6_UP}
    CaseSensitive := False;
{$ENDIF}
    Capacity := 30;
  end;
  URLlist := TStringList.Create;
end;

procedure TLinksBar.Loaded;
begin
  inherited;
  CreateLinkButtons(0);
end;

destructor TLinksBar.Destroy;
begin
  LinkList.Free;
  URLList.Free;
  inherited Destroy;
end;

procedure TLinksBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TLinksBar.SetAbout(Value: string);
begin
  Exit;
end;

procedure TLinksBar.OnlinkClick(Sender: TObject);
var
  navUrl: WideString;
begin
  if (Sender.ClassName = 'TToolButton') then
  begin
    navUrl := (Sender as TToolButton).Hint;
    if FAutoNavigate then
      if Assigned(FWebBrowser) then
        FWebBrowser.Navigate(navUrl);
    if Assigned(FStatusBar) then
      FStatusBar.SimpleText := navUrl;
  end;
end;

procedure TLinksBar.SetShown(Value: Boolean);
begin
  Visible := True;
  if FShown <> Value then
  begin
    Visible := not FShown;
    FShown := Value;
  end;
end;

procedure TLinksBar.CreateLinkButtons(ImageIdx: integer);
var
  LinkTitle: string;
  CaptionLength, i: integer;
begin
  for i := ButtonCount - 1 downto 0 do
    Buttons[i].Hide;

  LinkList.Clear;
  LinkList.BeginUpdate;
  GetTheLinksList(LinkList);
  LinkList.EndUpdate;
  for i := 0 to LinkList.Count - 1 do
  begin
    FTitle := LinkList.Names[i];
    FUrl := LinkList.Values[LinkList.Names[i]];
    LinkTitle := FTitle;
    CaptionLength := FMaxCaptionLength;
    if Length(LinkTitle) > CaptionLength then
      LinkTitle := Copy(LinkTitle, 1, CaptionLength) + '...'
    else
      LinkTitle := LinkTitle;

    with TLinksBar do
    begin
      Shown := True;
      LinksButton := TToolButton.create(TLinksBar);
      with LinksButton do
      begin
        if fShowImages then
          ImageIndex := ImageIdx;
        if ImageIndex > -1 then
          Height := 26
        else
          Height := 16;
        ShowHint := True;
        AutoSize := True;
        Grouped := True;
        Visible := True;
        Parent := Self;
        Style := tbsCheck;
        Caption := LinkTitle;
        Hint := FUrl;
        OnClick := OnlinkClick;
       //   Left        := Width * (self.ControlCount-2);
      end;
    end;
  end;
end;

{procedure LockClientUpdate;
begin
  if FLockClientUpdateCount = 0 then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
  Inc(FLockClientUpdateCount);
end;

procedure UnlockClientUpdate;
begin
  Dec(FLockClientUpdateCount);
  if FLockClientUpdateCount = 0 then
  begin
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Application.MainForm.ClientHandle, nil, 0, RDW_FRAME or
      RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_NOINTERNALPAINT);
  end;
end; }

// PUBLIC *****************************************************************************

procedure TLinksBar.GetTheLinksList(LinkList: Tstrings);
var
  RegPath: string;
begin
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title;
    if OpenKey(RegPath, False) then
    begin
      try
        LinkList.Clear;
        LinkList.BeginUpdate;
        ReadSectionValues('Links', LinkList);
        LinkList.EndUpdate;
        if LinkList.Text = '' then
        begin
          Visible := False;
      //   MessageDlg('The links list is empty.',mtError,[mbOK], 0);
        end;
      except
        MessageDlg('Error while reading the links from the registry!', mtError, [mbOK], 0);
      end;
      CloseKey;
      Free;
    end;
  end;
end;

procedure TLinksBar.GetTheLinksURLs(URLList: Tstrings);
var
  Linklist: TstringList;
  RegPath: string;
  i: integer;
begin
  Linklist := TstringList.Create;
  LinkList.Clear;
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title;
    if OpenKey(RegPath, False) then
    begin
      try
        ReadSectionValues('Links', LinkList);
        for i := 0 to LinkList.Count - 1 do
        begin
          FUrl := LinkList.Values[LinkList.Names[i]];
          URLList.Add(FUrl);
        end;
        CloseKey;
        Free;
      except
        MessageDlg('Error while reading the links from the registry!', mtError, [mbOK], 0);
      end;
    end;
  end;
  LinkList.Free;
end;

procedure TLinksBar.AddToLinksList(Title, URL: string);
var
  RegPath: string;
begin
  with TRegIniFile.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title;
    if OpenKey(RegPath, True) then
    try
      WriteString('Links', Title, URL);
      MessageDlg('The site : ' + Title + #10 + #13 + ' has been added to your links list.',
        mtInformation, [mbOK], 0);
    except

      MessageDlg('Error while adding the site to the registry!', mtError,
        [mbOK], 0);
    end;
    CloseKey;
  finally
    Free;
  end;
  Visible := True;
  CreateLinkButtons(0);
  Repaint;
end;

procedure TLinksBar.RemoveFromLinksList(Title: string);
var
  RegPath: string;
begin
  with TRegIniFile.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title + '\Links';
    if OpenKey(RegPath, False) then
    try
      if DeleteValue(Title) then
        MessageDlg('The site : ' + Title + #10 + #13 + 'Was removed from your links list.',
          mtInformation, [mbOK], 0)
      else
        MessageDlg('The site could not be found in the list.', mtError, [mbOK], 0);
    except
      MessageDlg('Error while removing the site from the registry!', mtError,
        [mbOK], 0);
      CloseKey;
    end;
  finally
    Free;
  end;
  CreateLinkButtons(0);
end;

procedure TLinksBar.ClearTheLinksList;
var
  del: Integer;
  RegPath: string;
begin
  del := MessageDlg('You are about to delete all your links. Are you sure?',
    mtWarning, [mbOK, mbCancel], 0);
  if del = mrCancel then
    Exit
  else
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      RegPath := 'SOFTWARE\' + Forms.Application.Title;
      if OpenKey(RegPath, False) then
      try
        if DeleteKey('Links') then
          MessageDlg('The link list has been removed from the registry.', mtInformation,
            [mbOK], 0)
        else
          MessageDlg('The list could not be located.', mtError, [mbOK], 0);
        CloseKey;
      except
        MessageDlg('Error while removing the list from the registry!', mtError,
          [mbOK], 0);
      end;
    finally
      Free;
    end;
  end;
  CreateLinkButtons(-1);
end;


procedure TLinksBar.NavigateToItem(Title: string);
var
  navUrl, RegPath: string;
  i: Integer;
begin
  with TRegIniFile.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title;
    if OpenKey(RegPath, False) then
    begin
      try
        ReadSectionValues('Links', LinkList);
        i := LinkList.IndexOfName(Title);
        navUrl := LinkList.Values[LinkList.Names[i]];
        closekey;
        if (i = -1) or (navUrl = '') then
        begin
          MessageDlg('Your links list is empty.',
            mtError, [mbOK], 0);
          Exit;
        end
        else
          if Assigned(FWebBrowser) then
            FWebBrowser.Navigate(navUrl);
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;

end.
