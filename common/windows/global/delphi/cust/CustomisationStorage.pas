(*
  Name:             CustomisationStorage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Fix unzip processmessages ugly
                    23 Aug 2006 - mcdurdin - Remove ftDFM, ftMacros, ftIcon and ftBitmap file types in customisation
                    04 Dec 2006 - mcdurdin - Add app icon and cfg icon as standard icons
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    09 Apr 2009 - mcdurdin - I1920 - Fix crash when starting windows - product pxx file locked for reading
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    25 Oct 2016 - mcdurdin - I5124 - CrashID:keyman.exe_10.0.751.0_2C6EA085_EOleException
*)
unit CustomisationStorage;

interface

uses Windows, sysutils, classes, contnrs, Vcl.Imaging.jpeg, StockFileNames;

type
  ECustomisationError = class(Exception);

  TCustFileType =
    (ftOther,
    ftKeymanMenu,       // data file describing menu entries and actions for the Keyman menu
    ftTrayIcon,         // Icon to go in task tray
    ftAppIcon,          // Main program icon
    ftCfgIcon);         // Configuration app icon

const
  CustFileTypeName: array[TCustFileType] of string =
    ('Other', 'Keyman Menu', 'Keyman Tray Icon', 'Application Icon', 'Configuration Icon');

type
  TCustFile = class
  private
    FFileName: string;
    FStream: TStream;
    function GetFileType: TCustFileType;
  protected
    procedure Load; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function IsStockFile: Boolean;
    property FileName: string read FFileName write FFileName;
    property FileType: TCustFileType read GetFileType;
    property Stream: TStream read FStream;
  end;

  TCustFileClass = class of TCustFile;

  TCustFileList = class(TObjectList)
  private
    FObjectClass: TCustFileClass;
    function Get(Index: Integer): TCustFile;
    procedure Put(Index: Integer; const Value: TCustFile);
  public
    constructor Create(AObjectClass: TCustFileClass);
    function IndexOfFileName(AFileName: string): Integer;
    function AddCustFile: TCustFile; virtual;
    property Items[Index: Integer]: TCustFile read Get write Put; default;
  end;

  TCustomisationStorage = class
  private
    FFileName: string;
    FCustFiles: TCustFileList;
  public
    constructor Create(AFileName: string; AObjectClass: TCustFileClass = nil; ARunTime: Boolean = False);
    destructor Destroy; override;
    function GetFileOfType(FileType: TCustFileType; StartIndex: Integer = 0): TCustFile;
    property CustFiles: TCustFileList read FCustFiles;
    property FileName: string read FFileName write FFileName;
  end;

function DetermineFileType(FFileName: string): TCustFileType;

implementation

uses klog;

function DetermineFileType(FFileName: string): TCustFileType;
var
  ext, nm: string;
begin
  // Determine file type by extension or name
  ext := LowerCase(ExtractFileExt(ffilename));
  nm := LowerCase(ExtractFileName(ffilename));
  if nm = StockFileName_TrayIcon then Result := ftTrayIcon
  else if nm = StockFileName_Menu then Result := ftKeymanMenu
  else if nm = StockFileName_AppIcon then Result := ftAppIcon
  else if nm = StockFileName_ConfigIcon then Result := ftCfgIcon
  else Result := ftOther;
end;

{ TCustomisationStorage }

constructor TCustomisationStorage.Create(AFileName: string; AObjectClass: TCustFileClass; ARunTime: Boolean);
var
  i: Integer;
  fs: TFileStream;
begin
  KL.MethodEnter(Self, 'Create', [AFileName, AObjectClass, ARunTime]);
  try
    inherited Create;

    //TODO REFACTOR THIS WHOLE THING AWAY

    FFileName := AFileName;
    if AObjectClass = nil then AObjectClass := TCustFile;
    FCustFiles := TCustFileList.Create(AObjectClass);

    for i := 0 to High(SStockFileNames) do
      with FCustFiles.AddCustFile do
      begin
        FileName := SStockFileNames[i];
        fs := TFileStream.Create(ExtractFilePath(Self.FFileName)+FileName, fmOpenRead or fmShareDenyWrite);   // I5124
        try
          Stream.CopyFrom(fs, 0);
        finally
          fs.Free;
        end;
        Stream.Position := 0;
      end;
  finally
    KL.MethodExit(Self, 'Create');
  end;
end;

destructor TCustomisationStorage.Destroy;
begin
  FreeAndNil(FCustFiles);
  inherited Destroy;
end;

function TCustomisationStorage.GetFileOfType(FileType: TCustFileType; StartIndex: Integer): TCustFile;
var
  i: Integer;
begin
  for i := StartIndex to CustFiles.Count - 1 do
    if CustFiles[i].FileType = FileType then begin Result := CustFiles[i]; Exit; end;
  Result := nil;
end;

{ TCustFileList }

function TCustFileList.AddCustFile: TCustFile;
begin
  Result := Items[inherited Add(FObjectClass.Create)];
end;

constructor TCustFileList.Create(AObjectClass: TCustFileClass);
begin
  inherited Create;
  FObjectClass := AObjectClass;
end;

function TCustFileList.Get(Index: Integer): TCustFile;
begin
  Result := TCustFile(inherited Get(Index));
end;

function TCustFileList.IndexOfFileName(AFileName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].FileName, AFileName) = 0 then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TCustFileList.Put(Index: Integer; const Value: TCustFile);
begin
  inherited Put(Index, Value);
end;

{ TCustFile }

constructor TCustFile.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TCustFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TCustFile.GetFileType: TCustFileType;
begin
  Result := DetermineFileType(FFileName);
end;

function TCustFile.IsStockFile: Boolean;
begin
  Result := GetFileType <> ftOther;
end;

procedure TCustFile.Load;
begin

end;

end.
