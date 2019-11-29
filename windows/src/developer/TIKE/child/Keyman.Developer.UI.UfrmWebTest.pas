unit Keyman.Developer.UI.UfrmWebTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmMDIChild,
  Keyman.Developer.UI.UframeCEFHost;

type
  TfrmWebTest = class(TfrmTikeChild)
    procedure FormCreate(Sender: TObject);
  private
    cef: TframeCEFHost;

    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; params: TStringList; wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; out Handled: Boolean);
    procedure WebCommand(Command: WideString; Params: TStringList);
    function DoNavigate(URL: string): Boolean;
    procedure RefreshHTML;
    procedure RefreshCaption;
    function ShouldHandleNavigation(URL: string): Boolean;
    { Private declarations }
  public
    procedure SetFocus; override;
    procedure StartClose; override;
  end;

var
  frmWebTest: TfrmWebTest;

implementation

uses
  UfrmMain,
  UmodWebHttpServer;

{$R *.dfm}

{ TfrmWebTest }

procedure TfrmWebTest.cefBeforeBrowse(Sender: TObject; const Url: string;
  params: TStringList; wasHandled: Boolean);
begin
  DoNavigate(Url);
end;

procedure TfrmWebTest.cefBeforeBrowseSync(Sender: TObject; const Url: string;
  out Handled: Boolean);
begin
  Handled := ShouldHandleNavigation(Url);
end;

function TfrmWebTest.ShouldHandleNavigation(URL: string): Boolean;
begin
  Result := False;
  if Copy(URL, 1, 7) = 'keyman:' then
  begin
    Result := True;
  end
  else if Copy(URL, 1, 5) = 'help:' then
  begin
    Result := True;
  end
  else if not URL.StartsWith(modWebHttpServer.GetLocalhostURL) and (Copy(URL, 1, 4) = 'http') then
  begin
    Result := True;
  end
end;

procedure TfrmWebTest.cefLoadEnd(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;

  if (frmKeymanDeveloper.ActiveChild = Self) and (Screen.ActiveForm = frmKeymanDeveloper) then
  begin
    cef.SetFocus;
  end;
end;

function TfrmWebTest.DoNavigate(URL: string): Boolean;
begin
  // TODO'
  Result := FALSE;
end;

procedure TfrmWebTest.FormCreate(Sender: TObject);
begin
  inherited;
  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnLoadEnd := cefLoadEnd;
  RefreshHTML;
end;

procedure TfrmWebTest.RefreshCaption;
begin
  //TODO
end;

procedure TfrmWebTest.RefreshHTML;
begin
  cef.Navigate(modWebHttpServer.GetLocalhostURL);
  RefreshCaption;
end;

procedure TfrmWebTest.SetFocus;
begin
  inherited;
  cef.SetFocus;
end;

procedure TfrmWebTest.StartClose;
begin
  Visible := False;
  cef.StartClose;
end;

procedure TfrmWebTest.WebCommand(Command: WideString; Params: TStringList);
begin
  // TODO
end;

end.
