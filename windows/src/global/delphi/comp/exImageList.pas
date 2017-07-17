(*
  Name:             exImageList
  Copyright:        Copyright (C) Bill Friedrich http://cc.embarcadero.com/Item/21269
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            Requires Windows XP or later
  History:          14 Sep 2006 - mcdurdin - Initial version
                    08 Jun 2012 - mcdurdin - I3352 - V9.0 - TExImageList remove dependency on version lookup and assume XP or later
*)
unit exImageList;
interface

uses
  Windows, SysUtils, Consts, Messages, Classes, Controls, Graphics,
  ImgList, CommCtrl, Math;

type
  TCustomRenderType = (crtAuto,crtPixel,crtImage);
  TRenderPixelEvent = procedure(var Red,Green,Blue,Transparency:Byte) of object;
  TRenderImageEvent = procedure(Image,Mask:TBitmap) of object;


type
  TexImageList = class(TImageList)
  protected
    FOnRenderPixelCustom: TRenderPixelEvent;
    FOnRenderImageCustom: TRenderImageEvent;
    FCustomRenderType: TCustomRenderType;
    FDisplayPixelFormat:TPixelFormat;
    FInternalPixelFormat:TPixelFormat;
    FPreferedPixelFormat:TPixelFormat;

    FDisabledImages : TCustomImageList;
    FDisabledImageChangeLink : TChangeLink;

    FInitialized:boolean;

    procedure CheckInitialized;

    procedure SetDisabledImages(const Value: TCustomImageList);
    procedure DisabledImagesChanged(Sender: TObject);

    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;

    procedure RenderImagePixels(Index:Integer; RenderPixelEvent:TRenderPixelEvent);
    procedure RenderImage(Index:Integer; RenderImageEvent:TRenderImageEvent);

    procedure RenderPixelDisabledEvent(var Red,Green,Blue,Transparency:Byte);
    procedure RenderImageDisabledEvent(Image,Mask:TBitmap);
    procedure RenderPixelHotEvent(var Red,Green,Blue,Transparency:Byte);

    procedure CopyImages(ImageListHandle: HImageList; Index: Integer = -1);
    procedure GetImagesFromHandle(ImageListHandle:HImageList; Index: Integer; Image, Mask: TBitmap);
    procedure GetImages(Index: Integer; Image, Mask: TBitmap);

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure InitializeImageList;

    function AddIconResource(ModuleHandle:HModule; const ResourceName:string):Integer; overload;
    function AddIconResource(ModuleHandle:HModule; const ResourceName:string; PreferedPixelFormat:TPixelFormat):Integer; overload;
    procedure AddNumberedIconResources(ModuleHandle:HModule; const ResourceNamePrefix:string); overload;
    procedure AddNumberedIconResources(ModuleHandle:HModule; const ResourceNamePrefix:string; PreferedPixelFormat:TPixelFormat); overload;

    procedure AddImages(ImageList: TCustomImageList);
    procedure Replace(Index: Integer; Image, Mask: TBitmap);

    procedure RenderImageDisabled(Index:Integer);
    procedure RenderImagesDisabled;

    procedure RenderImageHot(Index:Integer);
    procedure RenderImagesHot;

    procedure RenderImageCustom(Index:Integer);
    procedure RenderImagesCustom;
  published
    property DisabledImages : TCustomImageList read fDisabledImages write SetDisabledImages;
    property CustomRenderType:TCustomRenderType read FCustomRenderType write FCustomRenderType default crtAuto;
    property OnRenderPixelCustom:TRenderPixelEvent read fOnRenderPixelCustom write fOnRenderPixelCustom;
    property OnRenderImageCustom:TRenderImageEvent read fOnRenderImageCustom write fOnRenderImageCustom;
  end;

procedure Register;


implementation

const
  cPixelRenderCutoff = 16;
  cAlphaOpaque = 255;  

(** Begin Local Routines *****************************************************)

const                                                  // pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom
  cBitsPerPixel: array[pfDevice..pfCustom] of Integer = (24, 1, 4, 8, 15, 16, 24, 32, 24);
  cImageListColors: array[pfDevice..pfCustom] of Integer = (ILC_COLOR24, ILC_COLOR4, ILC_COLOR4, ILC_COLOR8, ILC_COLOR16, ILC_COLOR16, ILC_COLOR24, ILC_COLOR32, ILC_COLOR24);

type
  {Info about Icon resource records:

  The idCount member indicates how many images are present in the icon
  resource. The size of the idEntries array is determined by idCount. There
  exists one ICONDIRENTRY for each icon image in the file, providing details
  about its location in the file, size and color depth. The ICONDIRENTRY
  structure is defined as:}

  TGrpIconDirEntry = packed record
    bWidth:BYTE;          // Width, in pixels, of the image
    bHeight:BYTE;         // Height, in pixels, of the image
    bColorCount:BYTE;     // Number of colors in image (0 if >=8bpp)
    bReserved:BYTE;       // Reserved ( must be 0)
    wPlanes:WORD;         // Color Planes
    wBitCount:WORD;       // Bits per pixel
    dwBytesInRes:DWORD;   // How many bytes in this resource?
    nID:WORD;             // the ID
  end;
  PGrpIconDirEntry = ^TGrpIconDirEntry;

  TGrpIconDir = packed record
    idReserved:WORD;   // Reserved (must be 0)
    idType:WORD;       // Resource Type (1 for icons)
    idCount:WORD;      // How many images?
    idEntries: array[0..0] of TGrpIconDirEntry; // An entry for each image (idCount of 'em)
  end;
  PGrpIconDir = ^TGrpIconDir;

  TIconImage = packed record
    icHeader:BITMAPINFOHEADER;         // DIB header
    icColors:array[0..0] of TRGBQuad;  // Color table
    icXOR:array[0..0] of BYTE;         // DIB bits for XOR mask
    icAND:array[0..0] of BYTE;         // DIB bits for AND mask
  end;
  PIconImage = ^TIconImage;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

function LoadBestIconResource(ModuleHandle:HModule; const IconResourceName:string; PreferedWidth,PreferedHeight:integer; PreferedBitsPerPixel:Integer):HICON;
var
  IconGroupResourceHandle:HRSRC;
  IconGroupGlobalHandle:HGLOBAL;
  IconResourceHandle:HRSRC;
  IconGlobalHandle:HGLOBAL;
  pIconGroupDirectory:PGrpIconDir;
  pIconImageData:PIconImage;

  FoundBestIconResource:boolean;
  pBestIconImageData:PIconImage;
  BestIconImageDataLength:Integer;
  BestBitsPerPixel:integer;

  i:integer;
begin
  result := 0;

  // Find the icon group resource by name (the icon group is the .ico file which is stored in the module)
  IconGroupResourceHandle := FindResource(ModuleHandle, PChar(IconResourceName), RT_GROUP_ICON );
  if IconGroupResourceHandle = 0 then
    exit;

  // Load and Lock to get a pointer to a GRPICONDIR
  IconGroupGlobalHandle := LoadResource( ModuleHandle, IconGroupResourceHandle );
  if IconGroupGlobalHandle = 0 then
    exit;

  pIconGroupDirectory := LockResource( IconGroupGlobalHandle );
  if not Assigned(pIconGroupDirectory) then
    exit;

  FoundBestIconResource := False;
  pBestIconImageData := nil;
  BestIconImageDataLength := 0;
  
  // Find Exact Size and Best Color
  BestBitsPerPixel := 0;
  for i := 0 to pIconGroupDirectory^.idCount-1 do begin
    if (pIconGroupDirectory^.idEntries[i].bWidth = PreferedWidth) and
       (pIconGroupDirectory^.idEntries[i].bHeight = PreferedHeight) then begin

      IconResourceHandle := FindResource( ModuleHandle, MAKEINTRESOURCE(pIconGroupDirectory^.idEntries[i].nID), RT_ICON );
      if IconResourceHandle = 0 then
        break;

      IconGlobalHandle := LoadResource( ModuleHandle, IconResourceHandle );
      if IconGlobalHandle = 0 then
        break;

      pIconImageData := LockResource( IconGlobalHandle );
      if not assigned(pIconImageData) then
        break;

      if (pIconImageData.icHeader.biBitCount <= PreferedBitsPerPixel) and
         (pIconImageData.icHeader.biBitCount > BestBitsPerPixel) then begin
        BestBitsPerPixel := pIconImageData.icHeader.biBitCount;
        pBestIconImageData := pIconImageData;
        BestIconImageDataLength := pIconGroupDirectory^.idEntries[i].dwBytesInRes;
        FoundBestIconResource := True;
        if BestBitsPerPixel = PreferedBitsPerPixel then
          break;
      end; {if color}
    end; {if size}
  end; {for}

  if FoundBestIconResource then begin
    result := CreateIconFromResourceEx( PBYTE(pBestIconImageData), BestIconImageDataLength, True, $00030000, PreferedWidth, PreferedHeight, LR_DEFAULTCOLOR);
  end;

end;

function GetCurrentDisplayFormat:TPixelFormat;
var
  ScreenDCHandle:HDC;
  BitsPerPixel:Integer;
begin
  ScreenDCHandle := GetDC(0);
  try
    //Get bits per pixel
    BitsPerPixel := GetDeviceCaps(ScreenDCHandle, BITSPIXEL);
  finally
    //Release dc
    ReleaseDC(0, ScreenDCHandle);
  end;

  //Calculate color depth
  case  (BitsPerPixel) of
    1: result := pf1Bit;
    4: result := pf4Bit;
    8: result := pf8Bit;    
    15: result := pf15Bit;
    16: result := pf16Bit;
    24: result := pf24Bit;
    32: result := pf32Bit;
  else
    result := pfCustom;
  end;
end;


function ImageListCapableOf32bit:Boolean;
begin
  Result := True;  // I3352
end;


(** End Local Routines *******************************************************)



procedure Register;
begin
  RegisterComponents('Keyman', [TexImageList]);
end;

{ TexImageList }


constructor TexImageList.Create(AOwner: TComponent);
begin
  inherited;
  FInitialized := False;
  FCustomRenderType := crtAuto;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImagesChanged;
end;

destructor TexImageList.Destroy;
begin
  FDisabledImageChangeLink.Free;
  inherited;
end;


function TexImageList.AddIconResource(ModuleHandle: HModule; const ResourceName: string; PreferedPixelFormat:TPixelFormat):Integer;
var
  PreferedBitsPerPixel:Integer;
  Icon:TIcon;
begin
  CheckInitialized;

  result := -1;

  PreferedBitsPerPixel := cBitsPerPixel[PreferedPixelFormat];
  if PreferedBitsPerPixel = 16 then
    PreferedBitsPerPixel := 24
  else if PreferedBitsPerPixel <= 8 then
    PreferedBitsPerPixel := 4;

  Icon := TIcon.Create;
  try
    Icon.ReleaseHandle;
    Icon.Handle := LoadBestIconResource(ModuleHandle,ResourceName,Width,Height,PreferedBitsPerPixel);
    if Icon.Handle <> 0 then
      result := AddIcon(Icon);
  finally
    Icon.Free;
  end;
end;

function TexImageList.AddIconResource(ModuleHandle: HModule; const ResourceName: string): Integer;
begin
  CheckInitialized;
  
  result := AddIconResource(ModuleHandle,ResourceName,FPreferedPixelFormat);
end;

procedure TexImageList.AddNumberedIconResources(ModuleHandle: HModule; const ResourceNamePrefix: string);
begin
  CheckInitialized;
  AddNumberedIconResources(ModuleHandle,ResourceNamePrefix,FPreferedPixelFormat);
end;


procedure TexImageList.AddNumberedIconResources(ModuleHandle: HModule; const ResourceNamePrefix: string; PreferedPixelFormat:TPixelFormat);
var
  i:integer;
begin
  CheckInitialized;
  
  Clear;
  i := 0;
  while AddIconResource(ModuleHandle,ResourceNamePrefix+IntToStr(i),PreferedPixelFormat) <> -1 do begin
    inc(i);
  end;
end;

procedure TexImageList.AddImages(ImageList: TCustomImageList);
begin
  CheckInitialized;
  if ImageList <> nil then
    CopyImages(ImageList.Handle);
end;


procedure TexImageList.DisabledImagesChanged(Sender: TObject);
begin
  Change;
end;

procedure TexImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  if HandleAllocated then begin
    if (not Enabled) and (DisabledImages <> nil) then
      ImageList_DrawEx(DisabledImages.Handle, Index, Canvas.Handle, X, Y, 0, 0,GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
      inherited;
  end;
end;

procedure TexImageList.CopyImages(ImageListHandle:HImageList; Index: Integer);
var
  I: Integer;
  Image, Mask: TBitmap;
begin
  //BeginUpdate;
  try
    Image := TBitmap.Create;
    try
      Image.HandleType := bmDIB;
      Image.PixelFormat := FInternalPixelFormat;
      Image.Height := Height;
      Image.Width := Width;

      Mask := TBitmap.Create;
      try
        Mask.Monochrome := True;
        Mask.Height := Height;
        Mask.Width := Width;
        
        for I := 0 to ImageList_GetImageCount(ImageListHandle) - 1 do
          if (Index = -1) or (i = Index) then begin
            GetImagesFromHandle(ImageListHandle,i,Image,Mask);
            Add(Image, Mask);
          end;
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  finally
    //EndUpdate;
  end;
end;

procedure TexImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DisabledImages) and (Operation = opRemove) then
    DisabledImages := nil;
end;


procedure TexImageList.RenderImagePixels(Index:Integer; RenderPixelEvent:TRenderPixelEvent);
var
  Bitmap,Mask:TBitmap;
  pPixel32:PRGBQuad;
  pPixel24:PRGBTriple;
  IgnoreAlpha:Byte;
  i,j:integer;
begin
  IgnoreAlpha := cAlphaOpaque;

  Bitmap := TBitmap.Create;
  try
    Mask := TBitmap.Create;
    try
      Bitmap.HandleType := bmDIB;
      Bitmap.PixelFormat := FInternalPixelFormat;
      Bitmap.Width := Width;
      Bitmap.Height := Height;

      Mask.Monochrome := True;
      Mask.Width := Width;
      Mask.Height := Height;

      Self.GetImages(Index,Bitmap,Mask);
      if FInternalPixelFormat = pf32Bit then begin
        for i := 0 to Bitmap.Height-1 do begin
          pPixel32 := Bitmap.ScanLine[i];
          for j := 0 to Bitmap.Width-1 do begin
            RenderPixelEvent(pPixel32^.rgbRed,pPixel32^.rgbGreen,pPixel32^.rgbBlue,pPixel32^.rgbReserved);
            inc(pPixel32);
          end;
        end;
      end else if FInternalPixelFormat = pf24Bit then begin
        for i := 0 to Bitmap.Height-1 do begin
          pPixel24 := Bitmap.ScanLine[i];
          for j := 0 to Bitmap.Width-1 do begin
            RenderPixelEvent(pPixel24^.rgbtRed,pPixel24^.rgbtGreen,pPixel24^.rgbtBlue,IgnoreAlpha);
            inc(pPixel24);
          end;
        end;
      end else begin
        raise Exception.Create(Name+':TexImageList Pixel Format must be at least 24bit to support render operations.');
      end;
      Self.Replace(Index,Bitmap,Mask);

    finally
      Mask.Free;
    end;
  finally
    Bitmap.Free;

  end;
end;

procedure TexImageList.RenderImageCustom(Index: Integer);
begin
  CheckInitialized;
  
  if CustomRenderType = crtAuto then begin
    if cBitsPerPixel[FDisplayPixelFormat] >= cPixelRenderCutoff then begin
      if assigned(FOnRenderPixelCustom) then
        RenderImagePixels(Index,FOnRenderPixelCustom);
    end else begin
      if assigned(FOnRenderImageCustom) then
        RenderImage(Index,FOnRenderImageCustom);
    end;
  end else begin
    if CustomRenderType = crtPixel then begin
      if assigned(FOnRenderPixelCustom) then
        RenderImagePixels(Index,FOnRenderPixelCustom);
    end else begin
      if assigned(FOnRenderImageCustom) then
        RenderImage(Index,FOnRenderImageCustom);    
    end;
  end;
end;

procedure TexImageList.RenderImageDisabled(Index: Integer);
begin
  CheckInitialized;

  if cBitsPerPixel[FDisplayPixelFormat] >= cPixelRenderCutoff then
    RenderImagePixels(Index,RenderPixelDisabledEvent)
  else
    RenderImage(Index,RenderImageDisabledEvent);
end;

procedure TexImageList.RenderPixelDisabledEvent(var Red, Green, Blue, Transparency: Byte);
var
  GrayScaleValue:byte;
begin
  GrayScaleValue := HiByte(Red * 77 + Green * 151 + Blue * 28);
  GrayScaleValue := (GrayScaleValue + $B0) div 2;

  Red := GrayScaleValue;
  Green := GrayScaleValue;
  Blue := GrayScaleValue;
end;

procedure TexImageList.RenderImageHot(Index: Integer);
begin
  CheckInitialized;

  if cBitsPerPixel[FDisplayPixelFormat] >= cPixelRenderCutoff then
    RenderImagePixels(Index,RenderPixelHotEvent);
end;

procedure TexImageList.RenderPixelHotEvent(var Red, Green, Blue, Transparency: Byte);
const
  BrightnessDecrease:integer = 15;
begin

  // Only "hotten" pixels which are opaque because Windows automatically
  // "hottens" the transparent/translucent pixels for some reason.

  if Transparency = cAlphaOpaque then begin
    Red := Max(0,Red - BrightnessDecrease);
    Green := Max(0,Green - BrightnessDecrease);
    Blue := Max(0,Blue - BrightnessDecrease);
  end else begin
    Transparency := Max(0,Transparency - 30);
  end;
end;

procedure TexImageList.RenderImagesCustom;
var i:integer;
begin
  CheckInitialized;
  
  for i := 0 to Count-1 do begin
    RenderImageCustom(i);
  end;
end;

procedure TexImageList.RenderImagesDisabled;
var i:integer;
begin
  CheckInitialized;

  for i := 0 to Count-1 do begin
    RenderImageDisabled(i);
  end;
end;

procedure TexImageList.RenderImagesHot;
var i:integer;
begin
  CheckInitialized;
  
  for i := 0 to Count-1 do begin
    RenderImageHot(i);
  end;
end;

procedure TexImageList.SetDisabledImages(const Value: TCustomImageList);
begin
  if Assigned(FDisabledImages) then
    FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if Assigned(FDisabledImages) then begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
end;

procedure TexImageList.Replace(Index: Integer; Image, Mask: TBitmap);
begin
  CheckInitialized;

  if not HandleAllocated then
    raise EInvalidOperation.Create(SReplaceImage);

  if not ImageList_Replace(Handle, Index, Image.Handle, Mask.Handle) then
    raise EInvalidOperation.Create(SReplaceImage);

  Change;
end;

procedure TexImageList.GetImages(Index: Integer; Image, Mask: TBitmap);
begin
  GetImagesFromHandle(Self.Handle,Index,Image, Mask);
end;

procedure TexImageList.GetImagesFromHandle(ImageListHandle: HImageList; Index: Integer; Image, Mask: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  with Image.Canvas do
  begin
    Brush.Color := clBlack;
    FillRect(R);
    ImageList_Draw(ImageListHandle, Index, Handle, 0, 0, ILD_NORMAL);
  end;
  with Mask.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList_Draw(ImageListHandle, Index, Handle, 0, 0, ILD_MASK);
  end;
end;


procedure TexImageList.RenderImage(Index: Integer; RenderImageEvent: TRenderImageEvent);
var
  Bitmap,Mask:TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Mask := TBitmap.Create;
    try
      Bitmap.HandleType := bmDIB;
      Bitmap.PixelFormat := FInternalPixelFormat;
      Bitmap.Width := Width;
      Bitmap.Height := Height;

      Mask.Monochrome := True;
      Mask.Width := Width;
      Mask.Height := Height;

      Self.GetImages(Index,Bitmap,Mask);
      RenderImageEvent(Bitmap,Mask);
      Self.Replace(Index,Bitmap,Mask);

    finally
      Mask.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TexImageList.RenderImageDisabledEvent(Image, Mask: TBitmap);
var
  Mask2,DarkColorsMask: TBitmap;
  BoundsRect:TRect;
begin
  BoundsRect := Rect(0,0,Width,Height);

  Mask2 := TBitmap.Create;
  DarkColorsMask := TBitmap.Create;
  try
    // Paint the image onto DarkColorsMask bitmap
    DarkColorsMask.PixelFormat := FInternalPixelFormat;
    DarkColorsMask.Width := Width;
    DarkColorsMask.Height := Height;
    DarkColorsMask.Canvas.Draw(0,0,Mask);
    DarkColorsMask.Canvas.CopyMode := cmSrcPaint;
    DarkColorsMask.Canvas.CopyRect(BoundsRect,Image.Canvas,BoundsRect);
    // Make DarkColorsMask monochrome. (This is why it's called the "DarkColorsMask")
    DarkColorsMask.Monochrome := True;

    // Prepare the destination image by filling it with clButtonFace.
    Image.Canvas.Brush.Color := clBtnFace;
    Image.Canvas.FillRect(BoundsRect);

    DarkColorsMask.Canvas.Brush.Color := clWhite;

    // Draw the highlight on the destination image (for chisled effect)
    Image.Canvas.Brush.Color := clBtnHighlight;
    SetTextColor(Image.Canvas.Handle, clWhite);
    SetBkColor(Image.Canvas.Handle, clBlack);
    BitBlt(Image.Canvas.Handle, 1, 1, Width, Height, DarkColorsMask.Canvas.Handle, 0, 0, $00E20746);

    // Draw the image with the shadow color
    Image.Canvas.Brush.Color := clBtnShadow;
    SetTextColor(Image.Canvas.Handle, clWhite);
    SetBkColor(Image.Canvas.Handle, clBlack);
    BitBlt(Image.Canvas.Handle, 0, 0, Width, Height, DarkColorsMask.Canvas.Handle, 0, 0, $00E20746);

    // make sure the mask now includes the white chisled part of the image
    Mask2.Assign(Mask);
    BitBlt(Mask.Canvas.Handle,1,1,Width,Height,Mask2.Canvas.Handle,0,0,cmSrcAnd);

  finally
    Mask2.Free;
    DarkColorsMask.Free;
  end
end;

procedure TexImageList.InitializeImageList;
var
  Flags:UINT;
begin
  Clear;

  FDisplayPixelFormat := GetCurrentDisplayFormat;

  if (FDisplayPixelFormat = pf32Bit) and (ImageListCapableOf32Bit) then
    FInternalPixelFormat := pf32Bit
  else
    FInternalPixelFormat := pf24Bit;

  if (FDisplayPixelFormat = pf32Bit) and (FInternalPixelFormat = pf24Bit) then
    FPreferedPixelFormat := pf24Bit
  else
    FPreferedPixelFormat := FDisplayPixelFormat;

  // Set up the correct bitmap type for the image list bitmap.
  Flags := cImageListColors[FInternalPixelFormat];
  if Masked then
    Flags := Flags or ILC_MASK;
  Handle := ImageList_Create(Width, Height, Flags , 0, AllocBy);
  if not HandleAllocated then
    raise EInvalidOperation.Create(SInvalidImageList);

  FInitialized := True;    
end;

procedure TexImageList.CheckInitialized;
begin
  if not FInitialized then
    raise Exception.Create(Name+': TexImageList has not been Initialized.');
end;

end.

