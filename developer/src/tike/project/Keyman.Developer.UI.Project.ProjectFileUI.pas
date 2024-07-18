(*
  Name:             Keyman.Developer.UI.Project.ProjectFileUI
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2015

  Modified Date:    27 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    27 May 2015 - mcdurdin - I4703 - Loading a non-project file as a project crashes Developer [CrashID:tike.exe_9.0.497.0_00813E09_EProjectLoader]
                    27 May 2015 - mcdurdin - I4654 - Project display crashes if project render output file is locked [CrashID:tike.exe_9.0.489.0_0045876A_EFCreateError]
                    
*)
unit Keyman.Developer.UI.Project.ProjectFileUI;   // I4687

interface

uses
  System.Classes,
  VCL.Menus,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectLog,
  TempFileManager;

type
  TProjectFileAction = (pfaCompile, pfaInstall, pfaUninstall, pfaDebug,
    pfaTestKeymanWeb, pfaFontHelper, pfaFontDialog, pfaClean);   // I4057

  TProjectUI = class(TProject)
  private
    FRenderFileName: TTempFile;
    FIsTemporary: Boolean;
    function GetRenderFileName: string;
    procedure Refresh;

  protected
    procedure DoRefresh; override;
    procedure DoRefreshCaption; override;

  public
    constructor Create(AProjectType: TProjectType; AFileName: string; ALoad: Boolean); override;
    destructor Destroy; override;

    procedure Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer); override;   // I4706

    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;

    function DisplayFileName: string;
    function Render: WideString;

    function Save: Boolean; override;   // I4694
    function Load: Boolean; override;   // I4694

    property RenderFileName: string read GetRenderFileName;   // I4181

    property IsTemporary: Boolean read FIsTemporary write FIsTemporary;
  end;

  TProjectFileUI = class
  protected
    FOwner: TProjectFile;
    procedure OpenFile; virtual; abstract;
    function WindowOpen: Boolean; virtual;
  public
    constructor Create(AOwner: TProjectFile);
    destructor Destroy; override;

    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; virtual; abstract;

    procedure NewFile; virtual; abstract;
    procedure DefaultEvent(Sender: TObject); virtual; abstract;
  end;

  TProjectFileUIClass = class of TProjectFileUI;

implementation

uses
  System.Win.ComObj,
  Vcl.Dialogs,
  Winapi.msxml,
  Xml.Win.msxmldom,
  Xml.XMLDoc,
  Xml.Xmldom,
  Xml.xmlintf,

  System.SysUtils,

  utilhttp,

  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectLoader,
  UfrmMessages;

{ MRU functions }

function TProjectUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Files.Count - 1 do
    if not (Files[i].UI as TProjectFileUI).DoAction(action, FSilent) then Exit;

  case action of
    pfaCompile: Log(plsSuccess, FileName, 'All files compiled successfully.', 0, 0);   // I4706
  end;

  Result := True;
end;

procedure TProjectUI.DoRefresh;
begin
  Refresh;
end;

procedure TProjectUI.DoRefreshCaption;
begin
  if Assigned(FGlobalProjectRefreshCaption) then
    FGlobalProjectRefreshCaption(Self);
end;

{ TProjectUI }

constructor TProjectUI.Create(AProjectType: TProjectType; AFileName: string; ALoad: Boolean);
begin
  inherited;
  FRenderFileName := TTempFileManager.Get('.html');   // I4181
end;

destructor TProjectUI.Destroy;
begin
  FreeAndNil(FRenderFileName);   // I4181

  if FIsTemporary then
  begin
    if FileExists(FileName) then System.SysUtils.DeleteFile(FileName);
    if FileExists(UserFileName) then System.SysUtils.DeleteFile(UserFileName);
  end;

  inherited Destroy;
end;

function TProjectUI.DisplayFileName: string;
begin
  if IsTemporary
    then Result := 'Temporary Project'
    else Result := ExtractFileName(FileName);
end;

function TProjectUI.Load: Boolean;   // I4694
begin
  try
    Result := inherited Load;
  except
    on E:EOleException do
    begin
      ShowMessage(E.Message);
      Result := False;
    end;
    on E:EProjectLoader do   // I4703
    begin
      ShowMessage(E.Message);
      Result := False;
    end;
  end;
end;

procedure TProjectUI.Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer);   // I4706
begin
  frmMessages.Add(AState, Filename, Msg, MsgCode, line);
end;

procedure TProjectUI.Refresh;
begin
  if State = psReady then
    if Assigned(FGlobalProjectRefresh) then FGlobalProjectRefresh(Self);
end;

function TProjectUI.Render: WideString;
var
  doc, userdoc, xsl: IXMLDomDocument;
  output: WideString;
  FLastDir: string;
  i: Integer;
begin
  if not FileExists(FileName) then Save;

  Result := FRenderFileName.Name;   // I4181
  FLastDir := GetCurrentDir;
  SetCurrentDir(StringsTemplatePath);
  try
    doc := MSXMLDOMDocumentFactory.CreateDOMDocument;
    try
      doc.async := False;
      doc.load(FileName);

      //
      // Inject the user settings to the loaded file
      //

      if FileExists(UserFileName) then   // I4698
      begin
        userdoc := MSXMLDOMDocumentFactory.CreateDOMDocument;
        try
          userdoc.async := False;
          userdoc.load(UserFileName);
          for i := 0 to userdoc.documentElement.childNodes.length - 1 do
            doc.documentElement.appendChild(userdoc.documentElement.childNodes.item[i].cloneNode(true));
        finally
          userdoc := nil;
        end;
      end;

      xsl := MSXMLDOMDocumentFactory.CreateDOMDocument;
      try
        xsl.async := False;
        xsl.resolveExternals := True;
        xsl.validateOnParse := False;
        xsl.load(StringsTemplatePath + 'project.xsl');
        output := doc.transformNode(xsl);
      finally
        xsl := nil;
      end;
    finally
      doc := nil;
    end;
  finally
    SetCurrentDir(FLastDir);
  end;

  with TStringList.Create do
  try
    Text := output;
    try
      SaveToFile(Result, TEncoding.UTF8);  // I3310
    except
      on E:EFCreateError do   // I4654
      begin
        // Try again once, in case of file lock
        FreeAndNil(FRenderFileName);
        FRenderFileName := TTempFileManager.Get('.html');
        Result := FRenderFileName.Name;
        SaveToFile(Result, TEncoding.UTF8);  // I3310
      end;
    end;
  finally
    Free;
  end;
end;

function TProjectUI.Save: Boolean;   // I4694
begin
  try
    Result := inherited Save;
  except
    on E:EOleException do
    begin
      ShowMessage(E.Message);
      Result := False;
    end;
  end;
end;

function TProjectUI.GetRenderFileName: string;
begin
  Result := FRenderFileName.Name;   // I4181
end;

{ TProjectFileUI }

constructor TProjectFileUI.Create(AOwner: TProjectFile);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TProjectFileUI.Destroy;
begin
  inherited Destroy;
end;

function TProjectFileUI.WindowOpen: Boolean;
begin
  Result := False;
end;

end.
