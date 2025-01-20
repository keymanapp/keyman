unit UfrmHelp;

interface

uses
  System.Actions,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.ActnList,
  Vcl.AppEvnts,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  JvComponentBase,
  JvDockControlForm,
  uCEFWindowParent,
  uCEFInterfaces,
  uCEFTypes,
  uCEFChromiumWindow,

  Keyman.UI.UframeCEFHost,
  sentry,
  Sentry.Client,
  TempFileManager,
  UfrmTike,
  UfrmTikeDock;

type
  TfrmHelp = class(TTIKEDockForm) // I2721
    ActionList1: TActionList;
    actHelpContextRefresh: TAction;
    procedure actHelpContextRefreshUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRefreshQueued: Boolean;
    FHelpControl: TWinControl;
    FDocumentLoaded: Boolean;
    FTempFile: TTempFile;
    cef: TframeCEFHost;
    procedure AddUnmatchedContext(FormName, ControlName: string);
    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean);
  protected
    function GetHelpTopic: string; override;
    procedure FormShown; override;
  public
    procedure LoadHelp(ControlName, FormName: string);
    procedure QueueRefresh;
  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.dfm}

uses
  System.Types,
  Winapi.ShlObj,

  uCEFApplication,

  Keyman.Developer.System.HelpTopics,
  KeymanDeveloperOptions,
  Keyman.System.KeymanSentryClient,
  RedistFiles,
  RegistryKeys,
  UframeTextEditor,
  UfrmMain,
  UmodWebHTTPServer,
  utilsystem;

procedure TfrmHelp.AddUnmatchedContext(FormName, ControlName: string);
begin
  if not FKeymanDeveloperOptions.ReportUsage then Exit;
  if TKeymanSentryClient.Instance = nil then Exit;
  if TKeymanSentryClient.Instance.Client = nil then Exit;

  if FormName = '' then FormName := '<empty>';
  if ControlName = '' then ControlName := '<empty>';

  sentry_set_tag('keyman.help.form', PAnsiChar(UTF8Encode(FormName)));
  sentry_set_tag('keyman.help.control', PAnsiChar(UTF8Encode(ControlName)));

  TKeymanSentryClient.Instance.Client.MessageEvent(SENTRY_LEVEL_INFO,
    'Context sensitive help missing', False);

  sentry_remove_tag('keyman.help.form');
  sentry_remove_tag('keyman.help.control');
end;

procedure TfrmHelp.actHelpContextRefreshUpdate(Sender: TObject);
var
  FormName, ControlName: string;
  o: TComponent;
begin
  if not FDocumentLoaded then
    Exit;

  if (Screen.ActiveControl <> FHelpControl) or FRefreshQueued then
    try
      FormName := '';
      ControlName := '';

      if Screen.ActiveControl <> nil then
      begin
        // Don't get help document focus
        if (Screen.ActiveControl = Self) or (Screen.ActiveControl.Owner = Self)
        then
        begin
          if not FRefreshQueued then
            Exit;
        end
        else
          FHelpControl := Screen.ActiveControl;

        if FHelpControl is TTIKEForm then
          FormName := (FHelpControl as TTIKEForm).HelpTopic
        else if FHelpControl is TCustomForm then
          FormName := FHelpControl.ClassName
        else if (FHelpControl <> nil) then
        begin
          o := FHelpControl.Owner;
          while ((o is TframeCEFHost) or not(o is TTIKEForm)) and (o <> nil) do
            o := o.Owner;

          if o is TTIKEForm then
            FormName := (o as TTIKEForm).HelpTopic
          else if FHelpControl.Owner <> nil then
            FormName := FHelpControl.Owner.ClassName;

          ControlName := FHelpControl.Name;
        end;
      end;

      LoadHelp(ControlName, FormName);
    finally
      FRefreshQueued := False;
    end;
end;

procedure TfrmHelp.LoadHelp(ControlName, FormName: string);
begin
  // We don't want to show help on this form itself -- that's kinda circular
  if FormName = 'context/help' then
    Exit; // I2823

  if not FDocumentLoaded then
    Exit;

  // Call into the web browser control.
  try
    // TODO: we do no validation of ControlName and FormName. Probably should.
    cef.cef.ExecuteJavaScript('ActivatePage("' + FormName + '", "' + ControlName + '")', '');
  except
    ;
  end;
end;

procedure TfrmHelp.QueueRefresh;
begin
  FRefreshQueued := True;
end;

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  inherited;
  FTempFile := nil;
end;

procedure TfrmHelp.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTempFile);
end;

procedure TfrmHelp.FormShown;
begin
  inherited;
  cef := TframeCEFHost.Create(Self);
  cef.cef.DefaultUrl := modWebHttpServer.GetAppURL('help/');
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnLoadEnd := cefLoadEnd;
end;

function TfrmHelp.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Help;
end;

procedure TfrmHelp.cefBeforeBrowse(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean);
var
  elems: TArray<string>;
begin
  AssertVclThread;
  if Url.StartsWith('help:') then
  begin
    frmKeymanDeveloper.HelpTopic(Url.SubString('help:'.Length));
  end
  else if Url.StartsWith('missing:') then
  begin
    elems := Url.Split([':']);
    // expecting to see ['missing', form, control]
    if Length(elems) > 1 then
      if Length(elems) = 2
        then AddUnmatchedContext(elems[1], '')
        else AddUnmatchedContext(elems[1], elems[2]);
  end;
end;

procedure TfrmHelp.cefBeforeBrowseSync(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean);
begin
  AssertCefThread;
  Handled := Url.StartsWith('help:') or Url.StartsWith('missing:') or Url.StartsWith('found:');
end;

procedure TfrmHelp.cefLoadEnd(Sender: TObject);
begin
  AssertVclThread;

  if csDestroying in ComponentState then
    Exit;

  FDocumentLoaded := True;
  QueueRefresh;
end;

end.
