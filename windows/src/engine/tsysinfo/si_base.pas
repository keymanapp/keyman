(*
  Name:             si_base
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Base classes for system info functions
  Create Date:      13 May 2005

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    01 Aug 2006 - mcdurdin - Initial version for Keyman 7
                    23 Apr 2009 - mcdurdin - I1941 - Collect minidump files, diagnostic log files and zip them with the diagnostic report
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    03 Oct 2011 - mcdurdin - I2919 - Unicode support, xslts
                    23 Dec 2011 - mcdurdin - I3180 - x64 process details for tsysinfo and manage visibility
                    08 Nov 2012 - mcdurdin - I3556 - tsysinfo fails to load tabs and crashes
                    01 Dec 2012 - mcdurdin - I3556 - tsysinfo fails to load tabs and crashes
                    15 Apr 2015 - mcdurdin - I4659 - V9.0 - Add more detailed keyboard diagnostics
*)
unit si_base;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, msxml, Contnrs, ComCtrls, StdCtrls;

const
  WM_USER_Load = WM_USER + 100;

type
  TSI_Base = class;
  TSIList = class;

  TSIClasses = class(TClassList);

  TSINewParentEvent = procedure(Sender: TSIList; const Caption: string; var Parent: TWinControl) of object;

  TSIList = class(TObjectList)
  private
    FOwner: TComponent;
    FOnNewParent: TSINewParentEvent;
    FFiles: TStrings;
    FUnzippedFilesPath: string;
    function GetItem(Index: Integer): TSI_Base;
    procedure SetItem(Index: Integer; const Value: TSI_Base);
    function GetNewParent(const Caption: string): TWinControl;
    procedure ExecuteCollect(Progress: TProgressBar; Caption: TLabel);
    procedure DeleteTempFiles;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Collect;
    procedure Load(const AFileName: string);
    procedure Save(const AFileName: string);
    property Files: TStrings read FFiles;
    property Items[Index: Integer]: TSI_Base read GetItem write SetItem; default;
    property OnNewParent: TSINewParentEvent read FOnNewParent write FOnNewParent;
  end;

  TSI_Base = class(TComponent)
  private
    FXMLData: WideString;
    FParent: TWinControl;   // I3556
    FXSLTFile: string;   // I3556
    //class function FrameClass: TSIFrameClass;
    function GetHasXSLT: Boolean;  // I2919   // I3556
  protected
    rootnode: IXMLDOMNode;
    doc: IXMLDOMDocument;
    class procedure Register;
    class function GetCaption: string; virtual; abstract;
    function GetUseGlobalData: Boolean; virtual;
    function DoCollect: Boolean; virtual; abstract;
  public
    constructor Create(AParent: TWinControl; AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    function Collect: Boolean;
    class function Caption: string;
    property XMLData: WideString read FXMLData;
    property XSLTFile: string read FXSLTFile;   // I3556
    property HasXSLT: Boolean read GetHasXSLT;  // I2919   // I3556
    property Parent: TWinControl read FParent;   // I3556
    property UseGlobalData: Boolean read GetUseGlobalData;
  end;

  TSI_BaseClass = class of TSI_Base;

function SIClasses: TSIClasses;

implementation

uses
  System.Win.ComObj,
  System.Math,
  System.Zip,
  UfrmProgress,
  sysinfo_util,
  utildir;

var
  FSIClasses: TSIClasses = nil;

{ TSI_Base }

class function TSI_Base.Caption: string;
begin
  Result := GetCaption
end;

function TSI_Base.Collect: Boolean;
var
  node: IXMLDOMNode;
begin
  doc := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
  rootnode := doc.createNode(1, 'KeymanDiagnosticRecord', '');
  doc.appendChild(rootnode);
  try
    try
      Result := DoCollect;
    except
      on E:Exception do
      begin
        node := doc.createNode(1, 'Exception', '');
        node.text := XMLTextEncode(E.ClassName+': '+E.Message);
        rootnode.appendChild(node);
        Result := False;
      end;
    end;
  finally
    FXMLData := doc.xml;
    rootnode := nil;
    doc := nil;
  end;
end;

constructor TSI_Base.Create(AParent: TWinControl; AOwner: TComponent);
begin
  FParent := AParent;   // I3556
  FXSLTFile := ExtractFilePath(ParamStr(0)) + Copy(ClassName, 2, 255) + '.xslt';   // I3556
  inherited Create(AOwner);
end;

destructor TSI_Base.Destroy;
begin
  inherited Destroy;
end;

function TSI_Base.GetHasXSLT: Boolean;   // I3556
begin
  Result := FileExists(XSLTFile);
end;

function TSI_Base.GetUseGlobalData: Boolean;
begin
  Result := False;
end;

{class function TSI_Base.FrameClass: TSIFrameClass;
begin
  Result := GetFrameClass;
end;}

class procedure TSI_Base.Register;
begin
  if not Assigned(FSIClasses) then
    FSIClasses := TSIClasses.Create;
  FSIClasses.Add(Self);
end;

function SIClasses: TSIClasses;
begin
  if not Assigned(FSIClasses) then
    FSIClasses := TSIClasses.Create;
  Result := FSIClasses;
end;

{ TSIList }

procedure TSIList.Collect;
begin
  DeleteTempFiles;

  Clear;   // I3556

  with TfrmProgress.Create(nil) do
  try
    Caption := 'Collecting System Information';
    OnExecute := ExecuteCollect;
    if ShowModal = mrCancel then Exit;
  finally
    Free;
  end;
end;

procedure TSIList.ExecuteCollect(Progress: TProgressBar; Caption: TLabel);
var
  si: TSI_Base;
  i: Integer;
begin
  Screen.Cursor := crHourglass;
  try
    Progress.Max := SIClasses.Count;
    for i := 0 to SIClasses.Count - 1 do
    begin
      Progress.Position := i;
      Caption.Caption := TSI_BaseClass(SIClasses[i]).Caption;
      Caption.Update;
      Progress.Update;
      Application.ProcessMessages;
      si := TSI_BaseClass(SIClasses[i]).Create(GetNewParent(TSI_BaseClass(SIClasses[i]).Caption), FOwner);
      Add(si);
      si.Collect;
    end;
    Progress.Position := SIClasses.Count;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSIList.Load(const AFileName: string);
var
  FCompressed: Boolean;
  FTempFileName: string;
  i, j: Integer;
  s: string;
  si: TSI_Base;
  doc: IXMLDOMDocument;
begin
  DeleteTempFiles;

  FCompressed := AnsiCompareText(ExtractFileExt(AFileName), '.tsi') = 0;
  if FCompressed
    then FTempFileName := ChangeFileExt(AFileName, '.xml')
    else FTempFileName := AFileName;

  if FCompressed then
  begin
    FUnzippedFilesPath := IncludeTrailingPathDelimiter(CreateTempPath);

    with TZipFile.Create do
    try
      Open(AFileName, TZipMode.zmRead);
      if FileCount = 0 then
      begin
        ShowMessage('Invalid compressed diagnostic archive - no files found');
        Exit;
      end;
      ExtractAll(ExtractFileDir(FUnzippedFilesPath));

      FTempFileName := FUnzippedFilesPath + ChangeFileExt(ExtractFileName(AFileName), '.xml');

      for i := 0 to FileCount - 1 do
        if SameText(ExtractFileExt(FileName[i]), '.xml')
          then FTempFileName := FUnzippedFilesPath + FileName[i]
          else FFiles.Add(FUnzippedFilesPath + Filename[i]);
    finally
      Free;
    end;
  end
  else
    FTempFileName := AFileName;

  Clear;
  doc := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
  doc.load(FTempFileName);

  for i := 0 to doc.documentElement.childNodes.length - 1 do
    begin
      s := doc.documentElement.childNodes[i].nodeName;
      for j := 0 to FSIClasses.Count - 1 do
        if FSIClasses[j].ClassNameIs(s) then
        begin
          si := TSI_BaseClass(FSIClasses[j]).Create(GetNewParent(TSI_BaseClass(FSIClasses[j]).Caption), FOwner);
          Add(si);   // I3556
          si.FXMLData := doc.DocumentElement.ChildNodes[i].ChildNodes[0].XML;
          Break;
        end;
    end;
  doc := nil;

  if FTempFileName <> AFileName then
    DeleteFile(FTempFileName);
end;

procedure TSIList.Save(const AFileName: string);
var
  FCompressed: Boolean;
  i: Integer;
  s: WideString;
  FTempFileName: string;
  t: AnsiString;
begin
  s := '<?xml version="1.0" encoding="utf-8" ?>'#13#10;  // I2919
  s := s + '<TavultesoftSystemInformation>';
  for i := 0 to Count - 1 do
    s := s + '<'+Items[i].ClassName+'>'+Items[i].FXMLData+'</'+Items[i].ClassName+'>'#13#10;
  s := s + '</TavultesoftSystemInformation>';

  FCompressed := AnsiCompareText(ExtractFileExt(AFileName), '.tsi') = 0;
  if FCompressed
    then FTempFileName := ChangeFileExt(AFileName, '.xml')
    else FTempFileName := AFileName;

  with TFileStream.Create(FTempFileName, fmCreate) do
  try
    t := UTF8Encode(s);  // I2919
    Write(PAnsiChar(t)^, Length(t));
  finally
    Free;
  end;

  if FCompressed then
  begin
    with TZipFile.Create do
    try
      Open(AFileName, TZipMode.zmWrite);
      Add(FTempFileName);
      for i := 0 to FFiles.Count - 1 do
      try
        Add(FFiles[i]);
      except
        on E:EFOpenError do ;
      end;
      Close;
    finally
      Free;
    end;
    DeleteFile(FTempFileName);
  end;
end;

function TSIList.GetItem(Index: Integer): TSI_Base;
begin
  Result := inherited GetItem(Index) as TSI_Base;
end;

procedure TSIList.SetItem(Index: Integer; const Value: TSI_Base);
begin
  inherited SetItem(Index, Value);
end;

function TSIList.GetNewParent(const Caption: string): TWinControl;
begin
  if not Assigned(FOnNewParent)
    then raise Exception.Create('OnNewParent not assigned')
    else FOnNewParent(Self, Caption, Result)
end;

constructor TSIList.Create(AOwner: TComponent);
begin
  inherited Create;
  FFiles := TStringList.Create;
  FOwner := AOwner;
end;

procedure TSIList.DeleteTempFiles;
begin
  if FUnzippedFilesPath <> '' then
  begin
    RecursiveDelete(FUnzippedFilesPath);
    FUnzippedFilesPath := '';
  end;
end;

destructor TSIList.Destroy;
begin
  DeleteTempFiles;
  FFiles.Free;
  inherited Destroy;
end;

initialization
finalization
  FreeAndNil(FSIClasses);
end.
