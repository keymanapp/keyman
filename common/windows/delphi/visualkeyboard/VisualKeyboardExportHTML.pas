(*
  Name:             VisualKeyboardExportHTML
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    23 Aug 2006 - mcdurdin - Initial refactor for new visual keyboard
                    04 Dec 2006 - mcdurdin - Support new XML+XSLT OSK export
                    22 Jan 2007 - mcdurdin - Fix XSLT not loading for loading include files
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    29 Sep 2008 - mcdurdin - I1658 - Add support for export graphics options
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
*)
unit VisualKeyboardExportHTML;  // I3306

interface

uses
  System.UITypes,
  Windows, VisualKeyboard, Classes, SysUtils, Graphics, VisualKeyboardParameters;

type
  TVisualKeyboardExportHTML = class(TVisualKeyboardExport)
  private
    FFolders, FGraphical: Boolean;
  public
    constructor Create(AKbd: TVisualKeyboard; AFolders: Boolean = True; AGraphical: Boolean = True); reintroduce;
    procedure ExportToFile(FileName: WideString); override;
  end;

implementation

uses
  ActiveX,
  ComObj,
  Controls,
  DebugPaths,
  Dialogs,
  ErrorControlledRegistry,
  KeymanPaths,
  RegistryKeys,
  Unicode,
  utildir,
  VersionInfo,
  VisualKeyboardExportXML,
  MSXML2_TLB,
  xmldoc,
  xmlintf;

{ TVisualKeyboardExportHTML }

function GetOSKXSLPath: string;
var
  keyman_root: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'xml\osk\';
  if not DirectoryExists(Result) then
  begin
    if TKeymanPaths.RunningFromSource(keyman_root) then
    begin
      Result := keyman_root + 'windows\src\engine\xml\osk\';
    end
    else
    begin
      with TRegistryErrorControlled.Create do  // I2890
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_RootPath) then
          Result := IncludeTrailingPathDelimiter(ReadString(SRegValue_RootPath)) + 'xml\osk\';
      finally
        Free;
      end;

      if not DirectoryExists(Result) then
        Result := '';
    end;
  end;
  Result := GetDebugPath('Debug_OSKXSLPath', Result, True);
end;

const
  { GUID's from MSXML2_TLB.pas }
  //CLASS_DOMDocument26: TGUID = '{F5078F1B-C551-11D3-89B9-0000F81FE221}';
  CLASS_DOMDocument30: TGUID = '{F5078F32-C551-11D3-89B9-0000F81FE221}';
  CLASS_DOMDocument40: TGUID = '{88D969C0-F192-11D4-A65F-0040963251E5}';

function TryObjectCreate(const GuidList: array of TGuid): IUnknown;
var
  I: Integer;
  Status: HResult;
begin
  Status := S_OK;
  for I := Low(GuidList) to High(GuidList) do
  begin
    Status := CoCreateInstance(GuidList[I], nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IDispatch, Result);
    if Status = S_OK then Exit;
  end;
  OleCheck(Status);
end;

function CreateDOMDocument: IXMLDOMDocument2;
begin
  Result := TryObjectCreate([CLASS_DOMDocument40, CLASS_DOMDocument30]) as IXMLDOMDocument2;
  if not Assigned(Result) then
    raise Exception.Create('MS XML DOM 3.0 not installed');
end;

constructor TVisualKeyboardExportHTML.Create(AKbd: TVisualKeyboard; AFolders,
  AGraphical: Boolean);
begin
  inherited Create(AKbd);
  FFolders := AFolders;
  FGraphical := AGraphical;
end;

procedure TVisualKeyboardExportHTML.ExportToFile(FileName: WideString);
var
  doc: IXMLDomDocument2;
  xsldoc: IXMLDOMDocument2;
  xslproc: IXSLProcessor;
  xslt: IXSLTemplate;
  nodes: IXMLDOMNodeList;
  FOutput: WideString;
  //ss: TStringStream;
  f: TSearchRec;
  s: string;
  files: TStringList;
  FSubdir: WideString;
  stemp: WideString;
  i: Integer;
begin
  { Structure for HTML keyboard: file + subdirectory with images; always output as UTF-8? }

  stemp := ChangeFileExt(FileName, '') + '.xml';
  try

    with TVisualKeyboardExportXML.Create(FKbd) do
    try
      ExportToFile(stemp);
    finally
      Free;
    end;

    files := TStringList.Create;
    try
      doc := CreateDOMDocument;
      try
        doc.async := False;
        doc.validateOnParse := False;
        if not doc.load(stemp) then
        begin
          if doc.parseError <> nil then
          begin
            ShowMessage('Could not load XML: '+doc.parseError.reason);
            Exit;
          end;
        end;
        xsldoc := ComsFreeThreadedDOMDocument.Create;
        try
          xsldoc.async := False;
          xsldoc.resolveExternals := True;
          xsldoc.validateOnParse := False;
          if not xsldoc.load(GetOSKXSLPath + 'osk.xsl') then
          begin
            if xsldoc.parseError <> nil then
            begin
              ShowMessage('Could not load transform '+GetOSKXSLPath + 'osk.xsl: '+xsldoc.parseError.reason);
              Exit;
            end;
          end;

          xsldoc.setProperty('SelectionNamespaces', 'xmlns:oskexportdetails=''http://www.tavultesoft.com/xml/oskexportdetails''');
          xsldoc.setProperty('SelectionLanguage', 'XPath');

          nodes := xsldoc.documentElement.selectNodes('//oskexportdetails:includefile');
          for i := 0 to nodes.length - 1 do
            files.Add(nodes.item[i].text);

          xslt := ComsXSLTemplate.Create;
          xslt.stylesheet := xsldoc;

          xslproc := xslt.createProcessor;

          xslproc.input := doc;
          xslproc.addParameter('graphical', FGraphical, '');
          xslproc.addParameter('folders', FFolders, '');

          xslproc.transform;
          FOutput := xslproc.output; // doc.transformNode(xsldoc);
        finally
          xsldoc := nil;
        end;
      finally
        doc := nil;
      end;

      with TStringList.Create do
      try
        Text := FOutput;
        SaveToFile(FileName, TEncoding.UTF8);  // I3337
      finally
        Free;
      end;

      if FFolders then
      begin
        FSubdir := ChangeFileExt(FileName, '')+'_files';

        if DirectoryExists(FSubdir) and not DirectoryEmpty(FSubdir) then
        begin
          if MessageDlg('The subdirectory "'+FSubdir+'" already exists.  Images for the HTML file will be placed in this '+
            'subdirectory.  If you continue, any files currently in the directory will be deleted.'#13#10#13#10+
            'Continue exporting and delete all existing files in the subdirectory?', mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
          if not EmptyDirectory(FSubdir) then
            if MessageDlg('The subdirectory "'+FSubdir+'" was not able to be emptied.  Continue exporting anyway?',
              mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
        end;

        CreateDir(FSubdir);
      end
      else
        FSubdir := ExtractFileDir(FileName);

      s := GetOSKXSLPath;
      if FGraphical then
        for i := 0 to files.Count - 1 do
          CopyFile(PChar(s+files[i]), PChar(FSubDir+'\'+files[i]), True);
    finally
      files.Free;
    end;

    s := ChangeFileExt(stemp, '')+'_xml_files\';
    if FGraphical then
      if FindFirst(s + '*', 0, f) = 0 then
      begin
        repeat
          CopyFile(PChar(s+f.Name), PChar(FSubDir+'\'+f.Name), True);
        until FindNext(f) <> 0;
        FindClose(f);
      end;
    RecursiveDelete(ExcludeTrailingPathDelimiter(s));

  finally
    DeleteFile(stemp);
  end;
end;

end.
