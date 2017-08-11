(*
  Name:             utilolepicture
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    19 Mar 2007
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add LoadIconFromBitmap function
                    19 Mar 2007 - mcdurdin - Remove forms.pas, axctrls.pas dependencies
*)
unit utilolepicture;

interface

uses
  System.Types,
  System.UITypes,
  Windows, Graphics, ActiveX, SysUtils;

procedure GetOlePictureEx(Picture: TPicture; var OlePicture: IPictureDisp);
procedure LoadIconFromBitmap(FIcon: TIcon; FBitmap: TBitmap);

procedure GetOleFont(Font: TFont; var OleFont: IFontDisp);

implementation

uses
  Consts, ComObj, Vcl.Imaging.JPeg, Classes;

{ TPictureAdapter.IPictureAccess }

type
  IPictureAccess = interface
    ['{795D4D31-43D7-11D0-9E92-0020AF3D82DA}']
    procedure GetOlePicture(var OlePicture: IPictureDisp);
    procedure SetOlePicture(const OlePicture: IPictureDisp);
  end;

  TCustomAdapterEx = class(TInterfacedObject)
  private
    FOleObject: IUnknown;
    FConnection: Longint;
    FNotifier: IUnknown;
  protected
    Updating: Boolean;
    procedure Changed; virtual;
    procedure ConnectOleObject(OleObject: IUnknown);
    procedure ReleaseOleObject;
    procedure Update; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAdapterNotifierEx = class(TInterfacedObject,
    IPropertyNotifySink)
  private
    FAdapter: TCustomAdapterEx;
  protected
    { IPropertyNotifySink }
    function OnChanged(dispid: TDispID): HResult; stdcall;
    function OnRequestEdit(dispid: TDispID): HResult; stdcall;
  public
    constructor Create(Adapter: TCustomAdapterEx);
  end;

  TPictureAdapterEx = class(TCustomAdapterEx,
    IChangeNotifier, IPictureAccess)
  private
    FPicture: TPicture;
  protected
    { IPictureAccess }
    procedure GetOlePicture(var OlePicture: IPictureDisp);
    procedure SetOlePicture(const OlePicture: IPictureDisp);
    procedure Update; override;
  public
    constructor Create(Picture: TPicture);
  end;

  TOleGraphic = class(TGraphic)
  private
    FPicture: IPicture;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
  protected
    procedure Changed(Sender: TObject); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const Filename: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property MMHeight: Integer read GetMMHeight;      // in .01 mm units
    property MMWidth: Integer read GetMMWidth;
    property Picture: IPicture read FPicture write FPicture;
  end;
  
{ TPictureAdapterEx }

constructor TPictureAdapterEx.Create(Picture: TPicture);
begin
  inherited Create;
  FPicture := Picture;
end;

procedure TPictureAdapterEx.GetOlePicture(var OlePicture: IPictureDisp);
var
  PictureDesc: TPictDesc;
  OwnHandle: Boolean;
  TempM: TMetafile;
  TempB: TBitmap;
begin
  if FOleObject = nil then
  begin
    OwnHandle := False;
    with PictureDesc do
    begin
      cbSizeOfStruct := SizeOf(PictureDesc);
      if FPicture.Graphic is TBitmap then
      begin
        picType := PICTYPE_BITMAP;
        TempB := TBitmap.Create;
        try
          TempB.Assign(FPicture.Graphic);
          hbitmap := TempB.ReleaseHandle;
          hpal := TempB.ReleasePalette;
          OwnHandle := True;
        finally
          TempB.Free;
        end;
      end
      else if FPicture.Graphic is TJpegImage then
      begin
        picType := PICTYPE_BITMAP;
        TempB := TBitmap.Create;
        try
          TempB.Width := FPicture.Width;
          TempB.Height := FPicture.Height;
          TempB.Canvas.Draw(0, 0, FPicture.Graphic);
          hbitmap := TempB.ReleaseHandle;
          hpal := TempB.ReleasePalette;
          OwnHandle := True;
        finally
          TempB.Free;
        end;
      end
      else if FPicture.Graphic is TIcon then
      begin
        picType := PICTYPE_ICON;
        hicon := FPicture.Icon.Handle;
      end
      else
      begin
        picType := PICTYPE_ENHMETAFILE;
        if not (FPicture.Graphic is TMetafile) then
        begin
          TempM := TMetafile.Create;
          try
            TempM.Width := FPicture.Width;
            TempM.Height := FPicture.Height;
            with TMetafileCanvas.Create(TempM,0) do
            try
              Draw(0,0,FPicture.Graphic);
            finally
              Free;
            end;
            hemf := TempM.ReleaseHandle;
            OwnHandle := True;   // IPicture destroys temp metafile when released
          finally
            TempM.Free;
          end;
        end
        else
          hemf := FPicture.Metafile.Handle;
      end;
    end;
    OleCheck(OleCreatePictureIndirect(PictureDesc, IPicture, OwnHandle, OlePicture));
    ConnectOleObject(OlePicture);
  end;
  OlePicture := FOleObject as IPictureDisp;
end;

procedure TPictureAdapterEx.SetOlePicture(const OlePicture: IPictureDisp);
begin
  ConnectOleObject(OlePicture);
  Update;
end;

procedure TPictureAdapterEx.Update;
var
  Temp: TOleGraphic;
begin
  Updating := True;
  Temp := TOleGraphic.Create;
  try
    Temp.Picture := FOleObject as IPicture;
    FPicture.Graphic := Temp;
  finally
    Updating := False;
    Temp.Free;
  end;
end;

procedure GetOlePictureEx(Picture: TPicture; var OlePicture: IPictureDisp);
begin
  if Picture.PictureAdapter = nil then
    Picture.PictureAdapter := TPictureAdapterEx.Create(Picture);

  (Picture.PictureAdapter as IPictureAccess).GetOlePicture(OlePicture);
end;

{ TCustomAdapterEx }

procedure TCustomAdapterEx.Changed;
begin
  if not Updating then ReleaseOleObject;
end;

procedure TCustomAdapterEx.ConnectOleObject(OleObject: IUnknown);
begin
  if FOleObject <> nil then ReleaseOleObject;
  if OleObject <> nil then
    InterfaceConnect(OleObject, IPropertyNotifySink, FNotifier, FConnection);
  FOleObject := OleObject;
end;

constructor TCustomAdapterEx.Create;
begin
  inherited Create;
  FNotifier := TAdapterNotifierEx.Create(Self);
end;

destructor TCustomAdapterEx.Destroy;
begin
  ReleaseOleObject;
  inherited Destroy;
end;

procedure TCustomAdapterEx.ReleaseOleObject;
begin
  InterfaceDisconnect(FOleObject, IPropertyNotifySink, FConnection);
  FOleObject := nil;
end;

{ TAdapterNotifier }

constructor TAdapterNotifierEx.Create(Adapter: TCustomAdapterEx);
begin
  inherited Create;
  FAdapter := Adapter;
end;

{ TAdapterNotifier.IPropertyNotifySink }

function HandleException: HResult;
var
  E: TObject;
begin
  E := ExceptObject;
  if (E is EOleSysError) and (EOleSysError(E).ErrorCode < 0) then
    Result := EOleSysError(E).ErrorCode else
    Result := E_UNEXPECTED;
end;

function TAdapterNotifierEx.OnChanged(dispid: TDispID): HResult;
begin
  try
    FAdapter.Update;
    Result := S_OK;
  except
    Result := HandleException;
  end;
end;

function TAdapterNotifierEx.OnRequestEdit(dispid: TDispID): HResult;
begin
  Result := S_OK;
end;

procedure LoadIconFromBitmap(FIcon: TIcon; FBitmap: TBitmap);
var
  bblack: TBitmap;
  ii: TIconInfo;
begin
  bblack := Graphics.TBitmap.Create;
  try
    bblack.Width := 16;
    bblack.Height := 16;
    bblack.Canvas.Brush.Color := clBlack;
    bblack.Canvas.FillRect(Rect(0,0,16,16));
    ii.hbmColor := FBitmap.Handle;
    ii.hbmMask := bblack.Handle;
    FIcon.Handle := CreateIconIndirect(ii);
  finally
    bblack.Free;
  end;
end;

{ TOleGraphic }

procedure TOleGraphic.Assign(Source: TPersistent);
begin
  if Source is TOleGraphic then
    FPicture := TOleGraphic(Source).Picture
  else
    inherited Assign(Source);
end;

procedure TOleGraphic.Changed(Sender: TObject);
begin
  //!!
end;

procedure TOleGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  DC: HDC;
  Pal: HPalette;
  RestorePalette: Boolean;
  PicType: SmallInt;
  hemf: OLE_HANDLE; //HENHMETAFILE;
begin
  if FPicture = nil then Exit;
  ACanvas.Lock;  // OLE calls might cycle the message loop
  try
    DC := ACanvas.Handle;
    Pal := Palette;
    RestorePalette := False;
    if Pal <> 0 then
    begin
      Pal := SelectPalette(DC, Pal, True);
      RealizePalette(DC);
      RestorePalette := True;
    end;
    FPicture.get_Type(PicType);
    if PicType = PICTYPE_ENHMETAFILE then
    begin
      FPicture.get_Handle(hemf);
      PlayEnhMetafile(DC, hemf, Rect);
    end
    else
      OleCheck(FPicture.Render(DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left,
        Rect.Bottom - Rect.Top, 0, MMHeight - 1, MMWidth, -MMHeight, Rect));
    if RestorePalette then
      SelectPalette(DC, Pal, True);
  finally
    ACanvas.Unlock;
  end;
end;

function TOleGraphic.GetEmpty: Boolean;
var
  PicType: Smallint;
begin
  Result := (FPicture = nil) or (FPicture.get_Type(PicType) <> 0) or (PicType <= 0);
end;

function HIMETRICtoDP(P: TPoint): TPoint;
var
  DC: HDC;
begin
  DC := GetDC(0);
  SetMapMode(DC, MM_HIMETRIC);
  Result := P;
  Result.Y := -Result.Y;
  LPTODP(DC, Result, 1);
  ReleaseDC(0,DC);
end;

function TOleGraphic.GetHeight: Integer;
begin
  Result := HIMETRICtoDP(Point(0, MMHeight)).Y;
end;

function TOleGraphic.GetMMHeight: Integer;
begin
  Result := 0;
  if FPicture <> nil then FPicture.get_Height(Result);
end;

function TOleGraphic.GetMMWidth: Integer;
begin
  Result := 0;
  if FPicture <> nil then FPicture.get_Width(Result);
end;

function TOleGraphic.GetPalette: HPALETTE;
var
  Handle: OLE_HANDLE;
begin
  Result := 0;
  if FPicture <> nil then
  begin
    FPicture.Get_HPal(Handle);
    Result := HPALETTE(Handle);
  end;
end;

function TOleGraphic.GetTransparent: Boolean;
var
  Attr: Integer;
begin
  Result := False;
  if FPicture <> nil then
  begin
    FPicture.Get_Attributes(Attr);
    Result := (Attr and PICTURE_TRANSPARENT) <> 0;
  end;
end;

function TOleGraphic.GetWidth: Integer;
begin
  Result := HIMETRICtoDP(Point(MMWidth,0)).X;
end;

procedure InvalidOperation(const Str: string);
begin
  raise EInvalidGraphicOperation.Create(Str);
end;

procedure TOleGraphic.SetHeight(Value: Integer);
begin
  InvalidOperation(sOleGraphic);
end;

procedure TOleGraphic.SetPalette(Value: HPALETTE);
begin
  if FPicture <> nil then OleCheck(FPicture.Set_hpal(Value));
end;

procedure TOleGraphic.SetWidth(Value: Integer);
begin
  InvalidOperation(sOleGraphic);
end;

procedure TOleGraphic.LoadFromFile(const Filename: string);
begin
  //!!
end;

procedure TOleGraphic.LoadFromStream(Stream: TStream);
begin
  OleCheck(OleLoadPicture(TStreamAdapter.Create(Stream), 0, True, IPicture,
    FPicture));
end;

procedure TOleGraphic.SaveToStream(Stream: TStream);
begin
  OleCheck((FPicture as IPersistStream).Save(TStreamAdapter.Create(Stream), True));
end;

procedure TOleGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  InvalidOperation(sOleGraphic);
end;

procedure TOleGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  InvalidOperation(sOleGraphic);
end;



type
  IFontAccess = interface
    ['{CBA55CA0-0E57-11D0-BD2F-0020AF0E5B81}']
    procedure GetOleFont(var OleFont: IFontDisp);
    procedure SetOleFont(const OleFont: IFontDisp);
  end;

  TFontAdapter = class(TCustomAdapterEx,
    IChangeNotifier,
    IFontAccess)
  private
    FFont: TFont;
  protected
    { IFontAccess }
    procedure GetOleFont(var OleFont: IFontDisp);
    procedure SetOleFont(const OleFont: IFontDisp);
    procedure Changed; override;
    procedure Update; override;
  public
    constructor Create(Font: TFont);
  end;


function GetFontAccess(Font: TFont): IFontAccess;
begin
  if Font.FontAdapter = nil then
    Font.FontAdapter := TFontAdapter.Create(Font);
  Result := Font.FontAdapter as IFontAccess;
end;


procedure GetOleFont(Font: TFont; var OleFont: IFontDisp);
begin
  GetFontAccess(Font).GetOleFont(OleFont);
end;

{ TFontAdapter }

constructor TFontAdapter.Create(Font: TFont);
begin
  inherited Create;
  FFont := Font;
end;

procedure TFontAdapter.Update;
var
  TempFont: TFont;
  Name: WideString;
  Size: Currency;
  Temp: Longbool;
  Charset: Smallint;
  Style: TFontStyles;
  FOleFont: IFont;
begin
  if Updating then Exit;
  FOleFont := FOleObject as IFont;
  if FOleFont = nil then Exit;
  FOleFont.get_Name(Name);
  FOleFont.get_Size(Size);

  Style := [];
  FOleFont.get_Bold(Temp);
  if Temp then Include(Style, fsBold);
  FOleFont.get_Italic(Temp);
  if Temp then Include(Style, fsItalic);
  FOleFont.get_Underline(Temp);
  if Temp then Include(Style, fsUnderline);
  FOleFont.get_Strikethrough(Temp);
  if Temp then Include(Style, fsStrikeout);
  FOleFont.get_Charset(Charset);

  TempFont := TFont.Create;
  Updating := True;
  try
    TempFont.Assign(FFont);
    TempFont.Name := Name;
    TempFont.Size := Integer(Round(Size));
    TempFont.Style := Style;
    TempFont.Charset := Charset;
    FFont.Assign(TempFont);
  finally
    Updating := False;
    TempFont.Free;
  end;
end;

procedure TFontAdapter.Changed;
begin  // TFont has changed.  Need to update IFont
  if Updating then Exit;
  if FOleObject = nil then Exit;
  Updating := True;
  try
    with FOleObject as IFont do
    begin
      Put_Name(FFont.Name);
      Put_Size(FFont.Size);
      Put_Bold(fsBold in FFont.Style);
      Put_Italic(fsItalic in FFont.Style);
      Put_Underline(fsUnderline in FFont.Style);
      Put_Strikethrough(fsStrikeout in FFont.Style);
      Put_Charset(FFont.Charset);
    end;
  finally
    Updating := False;
  end;
end;

{ TFontAdapter.IFontAccess }

procedure TFontAdapter.GetOleFont(var OleFont: IFontDisp);
var
  FontDesc: TFontDesc;
  FontName: WideString;
  Temp: IFont;
begin
  if FOleObject = nil then
  begin
    FontName := FFont.Name;
    with FontDesc do
    begin
      cbSizeOfStruct := SizeOf(FontDesc);
      lpstrName := PWideChar(FontName);
      cySize := FFont.Size;
      if fsBold in FFont.Style then sWeight := 700 else sWeight := 400;
      sCharset := FFont.Charset;
      fItalic := fsItalic in FFont.Style;
      fUnderline := fsUnderline in FFont.Style;
      fStrikethrough := fsStrikeout in FFont.Style;
    end;
    OleCheck(OleCreateFontIndirect(FontDesc, IFont, Temp));
    ConnectOleObject(Temp);
  end;
  OleFont := FOleObject as IFontDisp;
end;

procedure TFontAdapter.SetOleFont(const OleFont: IFontDisp);
begin
  ConnectOleObject(OleFont as IFont);
  Update;
end;

end.

