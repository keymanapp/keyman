unit UfrmHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AppEvnts, StdCtrls, ActnList,
  UfrmTike,
  Xml.XMLDoc,
  Xml.XMLIntf,

  Keyman.UI.UframeCEFHost,
  TempFileManager, UfrmTikeDock,
  System.Actions, JvComponentBase, JvDockControlForm, uCEFWindowParent,
  uCEFInterfaces, uCEFTypes,
  uCEFChromiumWindow, Vcl.ExtCtrls;

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
    FHelpMissingFile: IXMLDocument;
    FHelpFileName: string;
    FHMFRoot: IXMLNode;
    FTempFile: TTempFile;
    cef: TframeCEFHost;
    procedure AddUnmatchedContext(FormName, ControlName: string);
    procedure DeleteMatchedContext(FormName, ControlName: string);
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

  Keyman.Developer.System.HelpTopics,

  RedistFiles,
  RegistryKeys,
  uCEFApplication,
  UframeTextEditor,
  UfrmMain,
  UmodWebHTTPServer,
  utilsystem;

procedure TfrmHelp.AddUnmatchedContext(FormName, ControlName: string);
var
  i: Integer;
  n: IXMLNode;
begin
  for i := 0 to FHMFRoot.ChildNodes.Count - 1 do
    if (FHMFRoot.ChildNodes[i].Attributes['FormName'] = FormName) and
      (FHMFRoot.ChildNodes[i].Attributes['ControlName'] = ControlName) then
    begin
      FHMFRoot.ChildNodes[i].Attributes['LastVisited'] := Now;
      if FHMFRoot.ChildNodes[i].AttributeNodes.IndexOf('VisitCount') < 0 then
        FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := 1
      else
        FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := FHMFRoot.ChildNodes
          [i].Attributes['VisitCount'] + 1;
      Exit;
    end;

  n := FHMFRoot.AddChild('MissingTopic');
  n.Attributes['FormName'] := FormName;
  n.Attributes['ControlName'] := ControlName;
  n.Attributes['LastVisited'] := Now;
  n.Attributes['VisitCount'] := 1;
end;

procedure TfrmHelp.DeleteMatchedContext(FormName, ControlName: string);
var
  i: Integer;
begin
  for i := 0 to FHMFRoot.ChildNodes.Count - 1 do
    if (FHMFRoot.ChildNodes[i].Attributes['FormName'] = FormName) and
      (FHMFRoot.ChildNodes[i].Attributes['ControlName'] = ControlName) then
    begin
      FHMFRoot.ChildNodes.Delete(i);
      Exit;
    end;
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
  FHelpFileName := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper +
    '\helpmissing.xml';
  ForceDirectories(ExtractFileDir(FHelpFileName));
  if FileExists(FHelpFileName) then
  begin
    FHelpMissingFile := LoadXMLDocument(FHelpFileName);
    FHMFRoot := FHelpMissingFile.ChildNodes['MissingTopics'];
  end
  else
  begin
    FHelpMissingFile := NewXMLDocument;
    FHMFRoot := FHelpMissingFile.AddChild('MissingTopics');
  end;
end;

procedure TfrmHelp.FormDestroy(Sender: TObject);
begin
  FHelpMissingFile.SaveToFile(FHelpFileName);
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
  end
  else if Url.StartsWith('found:') then
  begin
    elems := Url.Split([':']);
    // expecting to see ['found', form, control]
    if Length(elems) > 1 then
      if Length(elems) = 2
        then DeleteMatchedContext(elems[1], '')
        else DeleteMatchedContext(elems[1], elems[2]);
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
