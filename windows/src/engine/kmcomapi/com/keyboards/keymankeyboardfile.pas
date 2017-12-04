(*
  Name:             keymankeyboardfile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    29 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Pass graphics as icons instead of bitmaps
                    04 Dec 2006 - mcdurdin - Add Get_LayoutType, Serialize functions
                    22 Jan 2007 - mcdurdin - Remove Get_Licence unused function
                    25 Jan 2007 - mcdurdin - Fix _AddRef and _Release to detach from parent object
                    19 Mar 2007 - mcdurdin - I701 - fix leaked COM objects
                    05 Nov 2007 - mcdurdin - I930 - Report an error on an invalid keyboard file
                    27 Mar 2008 - mcdurdin - I1374 - Font helper code - chars and scripts used
                    27 Mar 2008 - mcdurdin - I1220 - Fixup ms office languages - read languages required from keyboard
                    14 Jun 2008 - mcdurdin - Add PrimaryLanguage property
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    23 Mar 2010 - mcdurdin - I2239 - Reference counting - Functionality loss - New keyboard not in keyboard layouts list immediately on install
                    23 Mar 2010 - mcdurdin - I2238 - Reference counting - Welcome not showing
                    29 Mar 2010 - mcdurdin - I2239 - Internal crash causes keyboard list to not refresh after installing keyboard
*)
unit keymankeyboardfile;

interface

uses
  Windows, keymanautoobject, ActiveX, ComObj, regkeyboards, keymanapi_TLB, StdVcl, kmxfile,
  keymankeyboard, keymancontext, Classes;

type
  TKeymanKeyboardFile = class(TKeymanKeyboard, IKeymanKeyboardFile)
  private
    FFileName: WideString;
    FError: Boolean;
    FKeyboardInfo: TKeyboardInfo;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
      override;

    { IKeymanKeyboardFile }
    procedure Install(Force: WordBool); safecall;

    { IKeymanKeyboard }
    function Get_Copyright: WideString; override; safecall;
    function Get_Filename: WideString; override; safecall;
    function Get_Message: WideString; override; safecall;
    function Get_Name: WideString; override; safecall;
    function Get_ID: WideString; override; safecall;
    function Get_Bitmap: IPicture; override; safecall;
    function Get_Encodings: KeymanKeyboardEncodings; override; safecall;
    function Get_LayoutType: KeymanKeyboardLayoutType; override; safecall;
    function GetCharsUsed: WideString; override; safecall;
    function Get_DefaultBCP47Languages: WideString; override; safecall;
    function Get_DefaultWindowsLanguages: WideString; override; safecall;
    function Get_DefaultPrimaryLanguage: Integer; override; safecall;
  public
    constructor Create(AContext: TKeymanContext; const Filename: Widestring);
    destructor Destroy; override;
  end;

implementation

uses
  Graphics,
  keymanerrorcodes,
  kmxfileusedchars,
  kpinstallkeyboard,
  SysUtils,
  utilkeyman,
  utilolepicture,
  utilxml,
  Variants;

{ TKeymanKeyboardFile }

constructor TKeymanKeyboardFile.Create(AContext: TKeymanContext; const Filename: Widestring);
begin
  inherited Create(AContext, IKeymanKeyboardFile);

  FFileName := Filename;
  try
    GetKeyboardInfo(FileName, False, FKeyboardInfo, True);
  except
    on E:Exception do
    begin
      FError := True;
      AContext.Errors.AddFmt(KMN_E_Install_InvalidFile, VarArrayOf([Filename, E.Message]), kesError);
    end;
  end;
end;

destructor TKeymanKeyboardFile.Destroy;
begin
  FreeAndNil(FKeyboardInfo.Bitmap);
  FreeAndNil(FKeyboardInfo.Icon);
  FreeAndNil(FKeyboardInfo.MemoryDump);
  inherited Destroy;
end;

function TKeymanKeyboardFile.GetCharsUsed: WideString;
begin
  Result := KMXFile_GetUsedChars(FFileName);
end;

function TKeymanKeyboardFile.Get_Bitmap: IPicture;
var
  pd: TPictDesc;
  FIcon: TIcon;
begin
  pd.cbSizeofstruct := SizeOf(TPictDesc);
  pd.picType := PICTYPE_ICON;
  if Assigned(FKeyboardInfo.Icon) then
  begin
    pd.hicon := FKeyboardInfo.Icon.ReleaseHandle;
  end
  else if Assigned(FKeyboardInfo.Bitmap) then
  begin
    FIcon := TIcon.Create;
    try
      LoadIconFromBitmap(FIcon, FKeyboardInfo.Bitmap);
      pd.hicon := FIcon.ReleaseHandle;
    finally
      FIcon.Free;
    end;
  end
  else
    pd.hicon := LoadIcon(hInstance, 'kbd_noicon');

  OleCreatePictureIndirect(pd, IPicture, True, Result);
end;

function TKeymanKeyboardFile.Get_Copyright: WideString;
begin
  Result := FKeyboardInfo.CopyrightString;
end;

function TKeymanKeyboardFile.Get_Encodings: KeymanKeyboardEncodings;
begin
  Result := 0;
  if kmxfile.keANSI in FKeyboardInfo.Encodings then Result := keymanapi_TLB.keANSI;
  if kmxfile.keUnicode in FKeyboardInfo.Encodings then Result := Result or keymanapi_TLB.keUnicode;
end;

function TKeymanKeyboardFile.Get_Filename: WideString;
begin
  Result := FFileName;
end;

function TKeymanKeyboardFile.Get_Name: WideString;
begin
  Result := FKeyboardInfo.KeyboardName;
end;

function TKeymanKeyboardFile.Get_LayoutType: KeymanKeyboardLayoutType;
begin
  if FKeyboardInfo.MnemonicLayout
    then Result := kltMnemonic
    else Result := kltPositional;
end;

function TKeymanKeyboardFile.Get_Message: WideString;
begin
  Result := FKeyboardInfo.MessageString;
end;

function TKeymanKeyboardFile.Get_ID: WideString;
begin
  Result := GetShortKeyboardName(FFileName); // ChangeFileExt(ExtractFileName(FFileName), '');
end;

function TKeymanKeyboardFile.Get_DefaultBCP47Languages: WideString;
begin
  // TODO: Support BCP47 Languages
  Result := FKeyboardInfo.ISO6393Languages;
end;

function TKeymanKeyboardFile.Get_DefaultPrimaryLanguage: Integer;
begin
  Result := FKeyboardInfo.KeyboardID;
end;

function TKeymanKeyboardFile.Get_DefaultWindowsLanguages: WideString;
begin
  Result := FKeyboardInfo.WindowsLanguages;
end;

procedure TKeymanKeyboardFile.Install(Force: WordBool);
begin
  with TKPInstallKeyboard.Create(Context) do
  try
    Execute(FFileName, '', [], Force);
  finally
    Free;
  end;
end;

function TKeymanKeyboardFile.Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
var
  FBitmap, FEncodings: WideString;
begin
  FEncodings := '';
  if (Get_Encodings and keymanapi_TLB.keUnicode) = keymanapi_TLB.keUnicode then
    FEncodings := FEncodings + '<encoding>Unicode</encoding>';
  if (Get_Encodings and keymanapi_TLB.keANSI) = keymanapi_TLB.keANSI then
    FEncodings := FEncodings + '<encoding>ANSI</encoding>';

  Result := XMLFormat([
    'id', Get_ID,
    'name', Get_Name,
    'filename', Get_Filename,
    'message', Get_Message,
    'copyright', Get_Copyright,
    'x:encodings', FEncodings, ////
    'layoutpositional', Get_LayoutType = kltPositional
  ]);

  if ((Flags and ksfExportImages) = ksfExportImages) and
    (Assigned(FKeyboardInfo.Icon) or
    Assigned(FKeyboardInfo.Bitmap)) then
  begin
    FBitmap := XMLImageTempName(ImagePath, References);
    with TBitmap.Create do
    try
      Width := 16;
      Height := 16;
      if Assigned(FKeyboardInfo.Icon)
        then Canvas.Draw(0,0,FKeyboardInfo.Icon)
        else Canvas.Draw(0,0,FKeyboardInfo.Bitmap);
      SaveToFile(FBitmap);
    finally
      Free;
    end;
      Result := Result + '<bitmap>'+ExtractFileName(FBitmap)+'</bitmap>';
  end;
end;

end.
