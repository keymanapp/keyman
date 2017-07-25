(*
  Name:             UfrmHelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    28 Sep 2006 - mcdurdin - Refactor help loader into LoadHelp method for external access
                    06 Oct 2006 - mcdurdin - Test WB.Document when updating as well as WB
                    04 Dec 2006 - mcdurdin - Localize
                    04 Jan 2007 - mcdurdin - Add help support
                    30 May 2007 - mcdurdin - I727 - Fixed drag and drop of files into Help Pane
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click web browser
                    17 Dec 2010 - mcdurdin - I2570 - Use new EmbeddedWB
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetText
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 Mar 2011 - mcdurdin - I2823 - Don't show instant help for the help window when it gets focused
                    26 Jun 2012 - mcdurdin - I3382 - KM9 - Fix invalid events for web browser controls with updated EmbeddedWB
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit UfrmHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, EmbeddedWB, AppEvnts, StdCtrls, ActnList, XMLDoc,
  XMLIntf, UfrmTike, ActiveX, SHDocVw_EWB, EwbCore, KeymanEmbeddedWB,
  TempFileManager, UfrmTikeDock,
  System.Actions, JvComponentBase, JvDockControlForm;

type
  TfrmHelp = class(TTIKEDockForm)
    web: TKeymanEmbeddedWB;  // I2721
    ActionList1: TActionList;
    actHelpContextRefresh: TAction;
    procedure actHelpContextRefreshUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
    procedure webGetDropTarget2(Sender: TCustomEmbeddedWB;
      var DropTarget: IDropTarget);
    function webShowMessage(Sender: TObject; HWND: NativeUInt; lpstrText,
      lpstrCaption: PWideChar; dwType: Integer; lpstrHelpFile: PWideChar;
      dwHelpContext: Integer; var plResult: NativeInt): HRESULT;
    procedure webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
  private
    FHelpControl: TWinControl;
    FStandardTemplatePath: WideString;
    FDocumentLoaded: Boolean;
    FHelpMissingFile: IXMLDocument;
    FHelpFileName: string;
    FHMFRoot: IXMLNode;
    FTempFile: TTempFile;
    procedure AddUnmatchedContext(FormName, ControlName: string);
    procedure DeleteMatchedContext(FormName, ControlName: string);
    procedure TransformXMLToHTML;
  public
    procedure LoadHelp(ControlName, FormName: string);
  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.dfm}

uses
  MSHTML_TLB,
  RedistFiles,
  RegistryKeys,
  ShlObj,
  UframeTextEditor,
  UfrmMain,
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
      if FHMFRoot.ChildNodes[i].AttributeNodes.IndexOf('VisitCount') < 0
        then FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := 1
        else FHMFRoot.ChildNodes[i].Attributes['VisitCount'] := FHMFRoot.ChildNodes[i].Attributes['VisitCount'] + 1;
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
begin
  if not FDocumentLoaded then Exit;

  FormName := '';
  ControlName := '';
  if Screen.ActiveControl <> FHelpControl then
  begin
    if Screen.ActiveControl = nil then
    else
    begin
      FHelpControl := Screen.ActiveControl;
      if FHelpControl is TCustomForm then FormName := (FHelpControl as TCustomForm).ClassName
      else
      begin
        if FHelpControl.Owner <> nil then
          FormName := FHelpControl.Owner.ClassName;
        ControlName := FHelpControl.Name;
      end;

      if (ControlName = 'memo') and (FormName = 'TframeTextEditor') then
      begin
        ControlName := (FHelpControl.Owner as TframeTextEditor).GetHelpToken;
        FormName := 'KMN';
      end;
    end;

    LoadHelp(ControlName, FormName);
  end;
end;

procedure TfrmHelp.LoadHelp(ControlName, FormName: string);
var
  wb: IWebBrowser2;
  doc2: IHTMLDocument2;
  doc3: IHTMLDocument3;
  win: IHTMLWindow2;
  elem: IHTMLElement;
  lang: OleVariant;
begin
  if (ControlName = 'web') and (FormName = 'TfrmHelp') then Exit;  // I2823

  try
    wb := web.DefaultInterface;
    if (WB <> nil) and (WB.Document <> nil) then
    begin
      WB.Document.QueryInterface(IHTMLDocument2, doc2);
      WB.Document.QueryInterface(IHTMLDocument3, doc3);
      if (doc2 <> nil) and (doc3 <> nil) then
      begin
        elem := doc3.getElementById(FormName+'-'+ControlName);
        if elem = nil
          then AddUnmatchedContext(FormName, ControlName)
          else DeleteMatchedContext(FormName, ControlName);
        win := doc2.parentWindow;
        if win <> nil then
        begin
          elem := doc3.getElementById('nohelp');
          if elem <> nil then
            win.execScript('ActivatePage("'+FormName+'", "'+ControlName+'")', lang);
        end;
      end;  { idoc <> nil }
    end; { wb <> nil }
  except
    ;
  end;
end;

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  inherited;
  FTempFile := nil;
  FHelpFileName := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper+'\helpmissing.xml';
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

procedure TfrmHelp.TransformXMLToHTML;
var
  doc, xsl: IXMLDocument;
  s: WideString;
begin
  FreeAndNil(FTempFile);
  FTempFile := TTempFileManager.Get('.html');

  doc := TXMLDocument.Create(nil);
  doc.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses
  doc.LoadFromFile(FStandardTemplatePath + 'contexthelp.xml');

  xsl := TXMLDocument.Create(nil);
//  xml.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses

  xsl.LoadFromFile(FStandardTemplatePath + 'help.xsl');
  doc.Node.transformNode(xsl.Node, s);
  with TStringStream.Create(s, TEncoding.UTF8) do
  try
    SaveToFile(FTempFile.Name);
  finally
    Free;
  end;
end;

procedure TfrmHelp.FormShow(Sender: TObject);
begin
  FDocumentLoaded := False;

  if FStandardTemplatePath = '' then
  begin
    FStandardTemplatePath := ExtractFilePath(ParamStr(0)) + 'locale\' + 'en'; // I2595

    if FileExists(FStandardTemplatePath + '\xml\help\contexthelp.xml') then
      FStandardTemplatePath := FStandardTemplatePath + '\xml\help\'
    else
    begin
      FStandardTemplatePath := ExtractFilePath(ParamStr(0)) + 'locale\' + 'en'; // I2595
      if FileExists(FStandardTemplatePath + '\xml\help\contexthelp.xml')
        then FStandardTemplatePath := FStandardTemplatePath + '\xml\help\'
        else FStandardTemplatePath := GetXMLTemplatePath + 'help\';
    end;
  end;

  TransformXMLToHTML;
//  if FStandardTemplatePath = '' then
  //  FStandardTemplatePath := GetXMLTemplatePath + 'help\';


  web.Navigate(FTempFile.Name); // + 'contexthelp.xml');
end;

procedure TfrmHelp.webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
  var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
  var Cancel: WordBool);
var
  FormName: string;
begin
  if Copy(URL, 1, 5) = 'help:' then
  begin
    Cancel := True;
    if FHelpControl is TCustomForm then FormName := (FHelpControl as TCustomForm).ClassName
    else if FHelpControl.Owner <> nil then FormName := FHelpControl.Owner.ClassName
    else FormName := '';
    if FormName = ''
      then frmKeymanDeveloper.HelpTopic('index')
      else frmKeymanDeveloper.HelpTopic('context_'+Copy(FormName,2,MAXINT));
  end;
end;

procedure TfrmHelp.webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
  var URL: OleVariant);
begin
  FDocumentLoaded := True;
end;

procedure TfrmHelp.webGetDropTarget2(Sender: TCustomEmbeddedWB;
  var DropTarget: IDropTarget);
begin
  DropTarget := frmKeymanDeveloper.DropTargetIntf;
end;

procedure TfrmHelp.webKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
  Shift: TShiftState);
begin
  if Key <> VK_CONTROL then
  begin
    if SendMessage(Application.Handle, CM_APPKEYDOWN, Key, 0) = 1 then
    begin
      Key := 0;
    end;
  end;
end;

procedure TfrmHelp.webNewWindow3(ASender: TObject; var ppDisp: IDispatch;
  var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
  bstrUrl: WideString);
var
  FormName: string;
begin
  Cancel := True;
  if Copy(bstrURL, 1, 5) = 'help:' then
  begin
    if FHelpControl is TCustomForm then FormName := (FHelpControl as TCustomForm).ClassName
    else if FHelpControl.Owner <> nil then FormName := FHelpControl.Owner.ClassName
    else FormName := '';
    if FormName = ''
      then frmKeymanDeveloper.HelpTopic('index')
      else frmKeymanDeveloper.HelpTopic('context_'+Copy(FormName,2,MAXINT));
  end
  else
    web.Go(bstrUrl);
end;

function TfrmHelp.webShowMessage(Sender: TObject; HWND: NativeUInt; lpstrText,
  lpstrCaption: PWideChar; dwType: Integer; lpstrHelpFile: PWideChar;
  dwHelpContext: Integer; var plResult: NativeInt): HRESULT;
begin
  Result := S_OK;
end;

end.
