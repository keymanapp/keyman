//***********************************************************
//                      TExportFavorites                    *
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
//$Id: ExportFavorites.pas,v 1.3 2006/11/26 05:55:31 bsalsa Exp $

unit ExportFavorites;

{$I EWB.inc}

interface

{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Classes, Dialogs, ShellApi, IniFiles, SHDocVw_EWB, ComCtrls, EwbCore, EmbeddedWB;

const
  ERROR_FAV_TARGET_PATH_INVALID = 101;
  ERROR_FAV_TARGET_FILENAME_INVALID = 102;
  ERROR_FAV_TARGET_FILENAME_EXTINVALID = 103;
  ERROR_FAV_FAVORITES_PATH_INVALID = 104;
  ERROR_FAV_NO_SUCCESS_MESSAGE = 105;

type
  PUrlRec = ^TUrlRec;
  TUrlRec = record
    Rep: string;
    UrlName: string;
    UrlPath: string;
    Level: Integer;
  end;

  TOnErrorEvent = procedure(Sender: TObject; const ErrorMsg: string; const ErrorCode: Byte) of object;
  TOnMessageEvent = procedure(Sender: TObject; const Msg: string) of object;

  TLocalization = class(TPersistent)
  private
    FTargetPathInvalid: string;
    FTargetFileNameInvalid: string;
    FTargetFileNameExtInvalid: string;
    FFavoritesPathInvalid: string;
    FNoSuccessMessage: string;
    FChangeItMessage: string;
    FHTMLTitle: string;
  published
    property TargetPathInvalid: string read FTargetPathInvalid write FTargetPathInvalid;
    property TargetFileNameInvalid: string read FTargetFileNameInvalid write FTargetFileNameInvalid;
    property TargetFileNameExtInvalid: string read FTargetFileNameExtInvalid write FTargetFileNameExtInvalid;
    property FavoritesPathInvalid: string read FFavoritesPathInvalid write FFavoritesPathInvalid;
    property NoSuccessMessage: string read FNoSuccessMessage write FNoSuccessMessage;
    property ChangeItMessage: string read FChangeItMessage write FChangeItMessage;
    property HTMLTitle: string read FHTMLTitle write FHTMLTitle;
  end;

  TExportFavorite = class(TComponent)
  private
    FAbout: string;
    FBmkList: TStringList;
    FLocalization: TLocalization;
    FavFolder: string;
    FavList: TList;
    FEnabled: Boolean;
    FExploreFavFileFolder: Boolean;
    FFavoritesPath: string;
    FNavigateOnComplete: Boolean;
    FShowSuccessMessage: Boolean;
    FStatusBar: TStatusBar;
    FSuccessMessage: TStrings;
    FTargetFileName: string;
    FTargetPath: string;
    FEmbeddedWB: TCustomEmbeddedWB;
    FOnError: TOnErrorEvent;
    FOnSuccess: TOnMessageEvent;
    procedure GetFavoritesFolder;
    function MakeBookMarkFile: Boolean;
    procedure SearchURL(Folder: string; Level: Integer);
    procedure SetAbout(Value: string);
    procedure SetSuccessMessage(Value: TStrings);
    procedure TraverseFavList(Idx, PrevIdx: Integer);
    function Check: Boolean;
  protected
    procedure MakeBookmark(UrlRec: PUrlRec);
    procedure MakeDocumentBottom;
    procedure MakeDocumentTop;
    procedure MakeHeaderBottom(UrlRec: PUrlRec);
    procedure MakeHeaderTop(UrlRec: PUrlRec);
  public
    SuccessFlag: Boolean;
    NavigatePath: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExportFavorites: Boolean;
  published
    property About: string read FAbout write SetAbout;
    property Enabled: boolean read FEnabled write FEnabled default True;
    property ExploreFavFileOnComplete: Boolean read FExploreFavFileFolder write FExploreFavFileFolder default False;
    property FavoritesPath: string read FFavoritesPath write FFavoritesPath;
    property NavigateOnComplete: Boolean read FNavigateOnComplete write FNavigateOnComplete default False;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property SuccessMessage: TStrings read FSuccessMessage write SetSuccessMessage;
    property TargetFileName: string read FTargetFileName write FTargetFileName;
    property TargetPath: string read FTargetPath write FTargetPath;
    property EmbeddedWB: TCustomEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property Localization: TLocalization read FLocalization write FLocalization;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property OnSuccess: TOnMessageEvent read FOnSuccess write FOnSuccess;
  end;

implementation

uses
  Windows, SysUtils, {$IFDEF DELPHI5}EwbCoreTools, {$ENDIF}Registry, Forms, EwbIEConst;

constructor TExportFavorite.Create;
begin
  FFavoritesPath := 'Auto';
  FTargetPath := 'C:\';
  FTargetFileName := 'newbook.htm';

  FLocalization := TLocalization.Create;
  FLocalization.TargetPathInvalid := 'The target path is invalid.';
  FLocalization.TargetFileNameInvalid := 'The target file name is invalid.';
  FLocalization.TargetFileNameExtInvalid := 'The target file name extension is invalid. It must be "*.htm".';
  FLocalization.FavoritesPathInvalid := 'The Favorites Path is invalid.';
  FLocalization.NoSuccessMessage := 'You must enter a SuccessMessage or turn off messages.';
  FLocalization.ChangeItMessage := 'Please change it.';
  FLocalization.HTMLTitle := 'Exported Favorites';

  FSuccessMessage := TStringList.Create;
  FSuccessMessage.Add('Your favorites have been exported successfully!');
  FSuccessMessage.Text := 'Your favorites have been successfully exported to %s';
  FShowSuccessMessage := True;
  FAbout := 'TExportFavorites by bsalsa. ' + WEB_SITE;

  FEnabled := True;
  SuccessFlag := False;
  FExploreFavFileFolder := False;
  FNavigateOnComplete := False;
  inherited;
end;

destructor TExportFavorite.Destroy;
begin
  FLocalization.Free;
  FSuccessMessage.Free;
  inherited Destroy;
end;

procedure TExportFavorite.SetSuccessMessage(Value: TStrings);
begin
  FSuccessMessage.Assign(Value)
end;

procedure TExportFavorite.GetFavoritesFolder;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False);
    FavFolder := Reg.ReadString('Favorites');
  finally
    Reg.Free;
  end;
end;

procedure TExportFavorite.SearchURL(Folder: string; Level: Integer);
var
  Found: Integer;
  SearchRec: TSearchRec;
  UrlFile: TIniFile;
  UrlRec: PUrlRec;
begin
  Found := FindFirst(Folder + '\*.*', faAnyFile, SearchRec);
  while Found = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      if (SearchRec.Attr and faDirectory > 0) then
      begin
{$WARNINGS OFF}
        if not (SearchRec.Attr and faSysFile > 0) then
        begin
          UrlRec := New(PUrlRec);
          UrlRec.Rep := Copy(Folder + '\' + SearchRec.Name, Length(FavFolder) + 2, Length(Folder + '\' + SearchRec.Name));
          UrlRec.UrlName := '';
          UrlRec.UrlPath := '';
          UrlRec.Level := Level;
          FavList.Add(UrlRec);
        end;
        SearchURL(Folder + '\' + SearchRec.Name, Level + 1); // Recursion
{$WARNINGS ON}
      end
      else
      begin
        if UpperCase(ExtractFileExt(SearchRec.Name)) = '.URL' then
        begin
          UrlRec := New(PUrlRec);
          UrlRec.Rep := Copy(Folder, Length(FavFolder) + 2, Length(Folder));
          UrlRec.UrlName := Copy(SearchRec.Name, 0, Length(SearchRec.Name) - 3);
          UrlFile := TIniFile.Create(Folder + '\' + SearchRec.Name);
          UrlRec.UrlPath := UrlFile.ReadString('InternetShortcut', 'URL', 'no path');
          UrlRec.Level := Level;
          UrlFile.Free;
          FavList.Add(UrlRec);
        end;
      end;
    Found := FindNext(SearchRec);
  end;

end;

procedure TExportFavorite.SetAbout(Value: string);
begin
  Exit;
end;

procedure TExportFavorite.TraverseFavList(Idx, PrevIdx: Integer);
var
  UrlRec, PrevUrlRec: PUrlRec;
  X: Integer;
begin
  if Idx < FavList.Count then
  begin
    UrlRec := FavList[Idx];
    if PrevIdx = -1 then
      PrevUrlRec := nil
    else
      PrevUrlRec := FavList[PrevIdx];
    if UrlRec.UrlName = '' then
    begin
      if PrevUrlRec <> nil then
      begin
        X := PrevUrlRec.Level;
        while UrlRec.Level <= X do
        begin
          MakeHeaderBottom(PrevUrlRec);
          Dec(PrevUrlRec.Level);
          Dec(X);
        end;
      end;
      MakeHeaderTop(UrlRec);
      PrevIdx := Idx;
    end
    else
      MakeBookmark(UrlRec);
    TraverseFavList(Idx + 1, PrevIdx);
  end;
end;

procedure TExportFavorite.MakeDocumentTop;
begin
  FBmkList.Add('<!-- Made with ' + Application.Title + ' -->');
  FBmkList.Add('');
  FBmkList.Add('<TITLE>' + FLocalization.HTMLTitle + '</TITLE>');
  FBmkList.Add('<H1>' + FLocalization.HTMLTitle + '</H1>');
  FBmkList.Add('');
  FBmkList.Add('<DL><P>');
end;

procedure TExportFavorite.MakeDocumentBottom;
begin
  FBmkList.Add('');
  FBmkList.Add('</DL><P>');
end;

procedure TExportFavorite.MakeHeaderTop(UrlRec: PUrlRec);
var
  I, Idx: Integer;
  S: string;
  A: array[0..255] of Char;
begin
  FillChar(A, SizeOf(A), 0);
  Idx := 1;
  for I := 1 to Length(UrlRec.Rep) do
    if UrlRec.Rep[I] = '\' then
      Idx := I + 1;
  for I := Idx to Length(UrlRec.Rep) do
    A[I - Idx] := UrlRec.Rep[I];
  FBmkList.Add('');
  for I := 1 to UrlRec.Level do
    S := S + '    ';
  FBmkList.Add(S + '<DT><H3>' + A + '</H3>');
  FBmkList.Add(S + '<DL><P>');
end;

procedure TExportFavorite.MakeHeaderBottom(UrlRec: PUrlRec);
var
  I: Integer;
  S: string;
begin
  for I := 1 to UrlRec.Level do
    S := S + '    ';
  FBmkList.Add(S + '</DL><P>');
  FBmkList.Add('');
end;

procedure TExportFavorite.MakeBookmark(UrlRec: PUrlRec);
var
  I: Integer;
  S: string;
begin
  Delete(UrlRec.UrlName, Length(UrlRec.UrlName), 1);
  for I := 1 to UrlRec.Level do
    S := S + '    ';
  FBmkList.Add(S + '<DT><A HREF="' + UrlRec.UrlPath + '">' +
    UrlRec.UrlName + '</A>' + '<BR>');
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

function TExportFavorite.Check: Boolean;
begin
  Result := (FTargetPath <> '') and DirectoryExists(FTargetPath);
  if Result then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.TargetPathInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_TARGET_PATH_INVALID);
    Result := False;
  end
  else if FTargetFileName = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.TargetFileNameInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_TARGET_FILENAME_INVALID);
    Result := False;
  end
  else if not (Pos('htm', FTargetFileName) > 1) then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.TargetFileNameExtInvalid + #10#13 + FLocalization.ChangeItMessage + '"*.htm"', ERROR_FAV_TARGET_FILENAME_EXTINVALID);
    Result := False;
  end
  else if Trim(FFavoritesPath) = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, Localization.FavoritesPathInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_FAVORITES_PATH_INVALID);
    Result := False;
  end
  else if Trim(FSuccessMessage.Text) = '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, FLocalization.NoSuccessMessage + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_NO_SUCCESS_MESSAGE);
    Result := False;
  end;
end;

function TExportFavorite.MakeBookMarkFile: Boolean;
begin
  Result := Check;
  if Result then
  begin
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM OFF}{$ENDIF}
    NavigatePath := IncludeTrailingBackslash(FTargetPath) + FTargetFileName;
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}
    FBmkList := TStringList.Create;
    try
      MakeDocumentTop;
      TraverseFavList(0, -1);
      MakeDocumentBottom;
      try
        FBmkList.SaveToFile(NavigatePath);
      except on EFCreateError do
        begin
          if Assigned(FOnError) then
            FOnError(Self, FLocalization.TargetFileNameInvalid + #10#13 + FLocalization.ChangeItMessage, ERROR_FAV_TARGET_FILENAME_INVALID);
          Result := False;
        end;
      end;
    finally
      FBmkList.Free;
    end;
    if Result then
    begin
      if Assigned(FOnSuccess) then
        FOnSuccess(Self, Format(SuccessMessage.Text, [NavigatePath]));
      if Assigned(FStatusBar) then
        FStatusBar.SimpleText := Format(SuccessMessage.Text, [NavigatePath]);
    end;
  end;
end;

function TExportFavorite.ExportFavorites: Boolean;
var
  I: Integer;
begin
  Result := Enabled;
  if Result then
  begin
    FavList := TList.Create;
    try
      if FFavoritesPath = 'Auto' then
        GetFavoritesFolder
      else
        FavFolder := FFavoritesPath;
      if (Trim(FavFolder) <> '') then
      begin
        SearchURL(FavFolder, 1);
        FavList.Sort(SortFunction);
        Result := MakeBookMarkFile;
        if Result then
        begin
          SuccessFlag := True;
          if FExploreFavFileFolder then
            ShellExecute(Forms.Application.Handle, 'explore', PChar(FTargetPath), nil,
              nil, SW_SHOWNORMAL);
          if FNavigateOnComplete then
            if Assigned(FEmbeddedWB) then
              FEmbeddedWB.Navigate(FTargetPath + FTargetFileName)
            else
              MessageDlg(ASS_MESS, mtError, [MbOk], 0);
        end;
      end
      else if Assigned(FOnError) then
        FOnError(Self, Localization.FavoritesPathInvalid, ERROR_FAV_FAVORITES_PATH_INVALID);

      for I := 0 to FavList.Count - 1 do
        Dispose(PUrlRec(FavList[I]));
    finally
      FavList.Free;
    end;
  end;
end;

end.

