//***********************************************************
//                    Import Favorites                      *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                     Eran Bodankin (bsalsa)               *
//                     bsalsa@gmail.com                     *
//                                                          *
//           Based on ideas by:  Troels Jakobsen            *
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
//$Id: ImportFavorites.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

unit ImportFavorites;

interface

{$I EWB.inc}

uses
  Windows, SysUtils, Classes, Dialogs, IniFiles, SHDocVw_EWB,
  EmbeddedWB, EWBCore, Registry, ComCtrls;

const
  ERROR_FAV_CURRENT_PATH_INVALID = 101;
  ERROR_FAV_CURRENT_FILENAME_INVALID = 102;
  ERROR_FAV_TARGET_FILENAME_EXTINVALID = 103;
  ERROR_FAV_TARGET_SUBFOLDER_INVALID = 104;
  ERROR_FAV_NO_SUCCESS_MESSAGE = 105;
  ERROR_FAV_FILE_NOTEXIST = 106;

type
  TOnErrorEvent = procedure(Sender: TObject; const ErrorMsg: string;
    const ErrorCode: Byte) of object;

  PUrlRec = ^TUrlRec;
  TUrlRec = record
    Rep: string;
    UrlName: string;
    UrlPath: string;
    Level: Integer;
  end;

  TLocalization = class(TPersistent)
  private
    FCurrentPathInvalid: string;
    FCurrentFileNameInvalid: string;
    FTargetFileNameExtInvalid: string;
    FTargetSubFolderInvalid: string;
    FNoSuccessMessage: string;
    FChangeItMessage: string;
    FFavFileNotExistInPath: string;
  published
    property CurrentPathInvalid: string read FCurrentPathInvalid write FCurrentPathInvalid;
    property CurrentFileNameInvalid: string read FCurrentFileNameInvalid write FCurrentFileNameInvalid;
    property TargetFileNameExtInvalid: string read FTargetFileNameExtInvalid write FTargetFileNameExtInvalid;
    property TargetSubFolderInvalid: string read FTargetSubFolderInvalid write FTargetSubFolderInvalid;
    property NoSuccessMessage: string read FNoSuccessMessage write FNoSuccessMessage;
    property ChangeItMessage: string read FChangeItMessage write FChangeItMessage;
    property FavFileNotExistInPath: string read FFavFileNotExistInPath write FFavFileNotExistInPath;
  end;

type
  TImportFavorite = class(TComponent)
  private
    FBookmarkList: TStringList;
    FLocalization: TLocalization;
    FAbout: string;
    FCurrentFileName: string;
    FCurrentFilePath: string;
    FEnabled: Boolean;
    FFavoritesPath: string;
    FNavigateOnComplete: Boolean;
    FShowSuccessMessage: Boolean;
    FStatusBar: TStatusBar;
    FSuccessMessage: TStrings;
    FTargetSubFolder: string;
    FWebBrowser: TCustomEmbeddedWB;
    L: TList;
    LvlList: TStringList;
    TargetFolder: string;
    FOnError: TOnErrorEvent;
    function FindFolder(HtmlStr: string; var Lvl: Integer): Boolean;
    function FindSectionEnd(HtmlStr: string; var Lvl: Integer): Boolean;
    function FindUrl(HtmlStr: string; Lvl: Integer): Boolean;
    function MakeFolder(Folder: string): Boolean;
    function MakeUrlFile(UrlName, UrlPath: string): Boolean;
    procedure Convert(Folder: string);
    procedure GetFavoritesFolder(var Folder: string);
    procedure MakeFavorites;
    procedure ReplaceIllChars(var S: string);
    procedure ScanBmkLine(I: Integer; Lvl: Integer);
    procedure SetAbout(Value: string);
    procedure SetSuccessMessage(Value: TStrings);
    function Check: Boolean;
  public
    SuccessFlag: Boolean;
    NavigatePath: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ImportFavorites: Boolean;
  published
    property About: string read FAbout write SetAbout;
    property CurrentFileName: string read FCurrentFileName write FCurrentFileName;
    property CurrentFilePath: string read FCurrentFilePath write FCurrentFilePath;
    property Enabled: boolean read FEnabled write FEnabled default True;
    property FavoritesPath: string read FFavoritesPath write FFavoritesPath;
    property NarigateOnComplete: Boolean read FNavigateOnComplete write FNavigateOnComplete default False;
    property ShowSuccessMessage: Boolean read FShowSuccessMessage write FShowSuccessMessage default True;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property SuccessMessage: TStrings read FSuccessMessage write SetSuccessMessage;
    property TargetSubFolder: string read FTargetSubFolder write FTargetSubFolder;
    property WebBrowser: TCustomEmbeddedWB read FWebBrowser write FWebBrowser;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property Localization: TLocalization read FLocalization write FLocalization;
  end;

implementation

uses
  EwbIEConst, EwbCoreTools;

constructor TImportFavorite.Create;
begin
  FAbout := 'TImportFavorite by bsalsa. ' + WEB_SITE;
  FCurrentFileName := 'newbook.htm';
  FCurrentFilePath := 'C:\';
  FTargetSubFolder := 'Imported Bookmarks';
  FFavoritesPath := 'Auto';
  FSuccessMessage := TStringList.Create;
  FSuccessMessage.Add('The bookmarks have been imported into your favorites successfully!'
    + #10 + #13 + 'You can find them in your favorites as a sub folder.');

  FLocalization := TLocalization.Create;
  FLocalization.FCurrentPathInvalid := 'The favorites file path is invalid.';
  FLocalization.FCurrentFileNameInvalid := 'The Current file name is invalid.';
  FLocalization.FTargetFileNameExtInvalid := 'The target file name extention is invalid.';
  FLocalization.FTargetSubFolderInvalid := 'You must specify a proper sub folder name.';
  FLocalization.FFavFileNotExistInPath := 'The specified favorite file does not exist in the current path.';
  FLocalization.NoSuccessMessage := 'You must enter a SuccessMessage or turn off messages.';
  FLocalization.ChangeItMessage := 'Please change it.';

  FShowSuccessMessage := True;
  SuccessFlag := False;
  FEnabled := True;
  FNavigateOnComplete := False;
  inherited;
end;

destructor TImportFavorite.Destroy;
begin
  FSuccessMessage.Free;
  inherited Destroy;
end;

procedure TImportFavorite.SetSuccessMessage(Value: TStrings);
begin
  FSuccessMessage.Assign(Value)
end;

procedure TImportFavorite.SetAbout(Value: string);
begin
  Exit;
end;

procedure TImportFavorite.GetFavoritesFolder(var Folder: string);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    Registry.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False);
    Folder := Registry.ReadString('Favorites');
  finally
    Registry.Free;
  end;
end;

function SortFunction(Item1, Item2: Pointer): Integer;
var
  Rec1, Rec2: PUrlRec;
begin
  Result := 0;
  Rec1 := Item1;
  Rec2 := Item2;
  if Rec1.Rep < Rec2.Rep then
    Result := -1;
  if Rec1.Rep > Rec2.Rep then
    Result := 1;
  if Rec1.Rep = Rec2.Rep then
  begin
    if Rec1.UrlName < Rec2.UrlName then
      Result := -1;
    if Rec1.UrlName > Rec2.UrlName then
      Result := 1;
    if Rec1.UrlName = Rec2.UrlName then
      Result := 0;
  end;
end;

procedure TImportFavorite.Convert(Folder: string);
var
  I: Integer;
begin
  L := TList.Create;
  try
    LvlList := TStringList.Create;
    try
      LvlList.Add('');
      ScanBmkLine(0, 0);
      L.Sort(SortFunction);
      MakeFavorites;
      for I := 0 to L.Count - 1 do
        Dispose(PUrlRec(L[I]));
    finally
      LvlList.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TImportFavorite.ScanBmkLine(I: Integer; Lvl: Integer);
begin
  if I < FBookmarkList.Count - 1 then
  begin
    if not FindSectionEnd(FBookmarkList[I], Lvl) then
      if not FindUrl(FBookmarkList[I], Lvl) then begin end;
        FindFolder(FBookmarkList[I], Lvl);
    ScanBmkLine(I + 1, Lvl);
  end;
end;

function TImportFavorite.FindFolder(HtmlStr: string; var Lvl: Integer): Boolean;
const
  FolderSubStr: string = '<H3';
var
  J, Idx: Integer;
  S: array[0..255] of Char;
  UrlRec: PUrlRec;
  I: Integer;
  Folder: string;
  FName: string;
begin
  J := Pos(FolderSubStr, HtmlStr);
  Result := (J <> 0);
  if J <> 0 then
  begin
    Inc(Lvl);
    FillChar(S, SizeOf(S), 0);
    Idx := 1;
    for J := 1 to Length(HtmlStr) - 1 do
      if HtmlStr[J] = '>' then
        Idx := J + 1;
    for J := Idx to Length(HtmlStr) - 5 do
      S[J - Idx] := HtmlStr[J];

    Folder := S;
    ReplaceIllChars(Folder);
    if LvlList.Count > Lvl then
      LvlList[Lvl] := Folder
    else
      LvlList.Add(Folder);
    FName := LvlList[Lvl];
    for I := Lvl - 1 downto 1 do
      FName := LvlList[I] + '\' + FName;
    UrlRec := New(PUrlRec);
    UrlRec.Rep := FName;
    UrlRec.UrlName := '';
    UrlRec.UrlPath := '';
    UrlRec.Level := Lvl;
    L.Add(UrlRec);
  end;
end;

function TImportFavorite.FindUrl(HtmlStr: string; Lvl: Integer): Boolean;
const
  UrlSubStr: string = 'http://';
var
  J, K: Integer;
  S1, S2, S3: array[0..255] of Char;
  Apo1, Apo2: PChar;
  I: Integer;
  SPath, SName: string;
  FName: string;
  UrlRec: PUrlRec;
begin
  J := Pos(UrlSubStr, HtmlStr);
  Result := (J <> 0);
  if J <> 0 then
  begin
    FillChar(S1, SizeOf(S1), 0);
    FillChar(S2, SizeOf(S2), 0);
    FillChar(S3, SizeOf(S3), 0);
    SPath := HtmlStr;
    Apo1 := StrScan(PChar(SPath), '"');
    for K := 1 to StrLen(Apo1) do
      S1[K - 1] := Apo1[K];
    for K := 0 to Pos('"', S1) - 2 do
      S2[K] := S1[K];
    SPath := S2;
    Apo2 := StrScan(Apo1, '>');
    for K := 1 to StrLen(Apo2) do
      S1[K - 1] := Apo2[K];
    for K := 0 to Pos('<', S1) - 2 do
      S3[K] := S1[K];
    SName := S3;
    ReplaceIllChars(SName);
    FName := LvlList[Lvl];
    for I := Lvl - 1 downto 1 do
      FName := LvlList[I] + '\' + FName;
    UrlRec := New(PUrlRec);
    UrlRec.Rep := FName;
    UrlRec.UrlName := SName;
    UrlRec.UrlPath := SPath;
    UrlRec.Level := Lvl;
    L.Add(UrlRec);
  end;
end;

function TImportFavorite.FindSectionEnd(HtmlStr: string; var Lvl: Integer): Boolean;
const
  EndSubStr: string = '</DL>';
var
  J: Integer;
begin
  J := Pos(EndSubStr, HtmlStr);
  Result := (J <> 0);
  if J <> 0 then
    Dec(Lvl);
end;

procedure TImportFavorite.MakeFavorites;
var
  I: Integer;
  UrlRec: PUrlRec;
begin
  for I := 0 to L.Count - 1 do
  begin
    UrlRec := L[I];
    if UrlRec.UrlName = '' then
      MakeFolder(TargetFolder + '\' + UrlRec.Rep)
    else
      MakeUrlFile(TargetFolder + '\' + UrlRec.Rep + '\' + UrlRec.UrlName + '.url', UrlRec.UrlPath);
  end;
end;

function TImportFavorite.MakeFolder(Folder: string): Boolean;
begin
  Result := CreateDirectory(PChar(Folder), nil);
end;

function TImportFavorite.MakeUrlFile(UrlName, UrlPath: string): Boolean;
var
  UrlFile: TIniFile;
begin
  UrlFile := TIniFile.Create(UrlName);
  try
    UrlFile.WriteString('InternetShortcut', 'URL', UrlPath);
    Result := True;
  finally
    UrlFile.Free;
  end;
end;

procedure TImportFavorite.ReplaceIllChars(var S: string);
const
  ReplacedChar: Char = '-';
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if CharInSet(S[I], ['\', '/', ':', '*', '?', '"', '<', '>', '|']) then
      //  if S[I] in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] then
      S[I] := ReplacedChar;
end;

function TImportFavorite.Check: Boolean;
begin
  Result :=  FCurrentFilePath <> '';
  if not Result then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.FCurrentPathInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_CURRENT_PATH_INVALID);
    Result := False;
  end
  else if FCurrentFileName = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.FCurrentFileNameInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_CURRENT_FILENAME_INVALID);
    Result := False;
  end
  else if not (Pos('htm', FCurrentFileName) > 1) then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.FTargetFileNameExtInvalid + #10#13 + FLocalization.ChangeItMessage + '"*.htm"', ERROR_FAV_TARGET_FILENAME_EXTINVALID);
    Result := False;
  end
  else if FTargetSubFolder = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.FTargetSubFolderInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_TARGET_SUBFOLDER_INVALID);
    Result := False;
  end
  else if FSuccessMessage.Text = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.NoSuccessMessage + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_NO_SUCCESS_MESSAGE);
    Result := False;
  end
  else if not FileExists(FCurrentFilePath + '\' + FCurrentFileName) then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.FFavFileNotExistInPath + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_NO_SUCCESS_MESSAGE);
    Result := False;
  end
end;

function TImportFavorite.ImportFavorites: Boolean;
var
  R: TSearchRec;
  FavFolder: string;
begin
  Result := Enabled;
  if Result and Check then
  begin
    FBookmarkList := TStringList.Create;
    try
      FBookmarkList.LoadFromFile(FCurrentFilePath + FCurrentFileName);
      if FFavoritesPath = 'Auto' then
        GetFavoritesFolder(FavFolder)
      else
        FavFolder := FFavoritesPath;
      if (FavFolder <> '') then
      begin
        CreateDirectory(PChar(FavFolder + '\' + TargetSubFolder), nil);
        if FindFirst(FavFolder + '\' + TargetSubFolder, faDirectory, R) = 0 then
        begin
          TargetFolder := FavFolder + '\' + TargetSubFolder;
          Convert(FavFolder + '\' + TargetSubFolder);
          if FShowSuccessMessage then
            MessageDlg(FSuccessMessage.Text, mtInformation, [MbOk], 0);
          if Assigned(FStatusBar) then
            FStatusBar.SimpleText := SuccessMessage.Text;
          SuccessFlag := True;
          Result := True;
          if FNavigateOnComplete then
            if Assigned(FWebBrowser) then
              FWebBrowser.Navigate(FCurrentFilePath + FCurrentFileName)
            else
              MessageDlg(ASS_MESS, mtError, [MbOk], 0);
        end
        else
        begin
          if Assigned(FOnError) then
            FOnError(Self, FLocalization.FTargetSubFolderInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_TARGET_SUBFOLDER_INVALID);
          SuccessFlag := False;
        end;
        FindClose(R);
      end;
    finally
      FBookmarkList.Free;
    end;
  end;
end;

end.

