(*
  Name:             utilicon
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Aug 2014

  Modified Date:    12 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Aug 2014 - mcdurdin - I4317 - V9.0 - Some .ico formats do not load correctly in icon conversion
                    12 Aug 2014 - mcdurdin - I4314 - V9.0 - Icon size in tool tray is wrong when using large fonts
*)
unit utilicon;

interface

uses
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.Imaging.Pngimage,

  kmxfile;

function ConvertKeyboardBitmapToAlphaIcon(ki: TKeyboardInfo; const IconFileName: string): Boolean;
function ConvertBitmapsToAlphaIcon(b: array of Vcl.Graphics.TBitmap; const IconFileName: string): Boolean;
function GetTrayIconSize: TPoint;   // I4314

{$R 'tip_icon_base.res' 'tip_icon_base.rc'}

implementation

uses
  System.Classes,
  System.TypInfo,
  System.Win.Registry,
  Winapi.ActiveX,
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPObj;

procedure MergeImagesIntoIcon(bmp: array of TGPBitmap; const Filename: string); forward;
function CreateIconInFrame(SourceBitmap: TGPBitmap; const SourceFrameResourceName: string; FX, FY: Integer; FWidth, FHeight, FScale: Integer): TGPBitmap; forward;

type
  TIconDirEntry = packed record
    bWidth: BYTE;          // Width, in pixels, of the image
    bHeight: BYTE;         // Height, in pixels, of the image
    bColorCount: BYTE;     // Number of colors in image (0 if >=8bpp)
    bReserved: BYTE;       // Reserved ( must be 0)
    wPlanes: WORD;         // Color Planes
    wBitCount: WORD;       // Bits per pixel
    dwBytesInRes: DWORD;    // How many bytes in this resource?
    dwImageOffset: DWORD;   // Where in the file is this image?
  end;

  PIconDirEntry = ^TIconDirEntry;

  TIconDir = packed record
    idReserved: WORD;   // Reserved (must be 0)
    idType: WORD;       // Resource Type (1 for icons)
    idCount: WORD;      // How many images?
  end;

  PIconDir = ^TIconDir;

procedure GDIPCheck(status: TStatus);
begin
  case status of
    Ok: ;
    Win32Error: RaiseLastOSError;
    else raise Exception.Create('GDI Error: '+GetEnumName(Typeinfo(TStatus), Ord(status)));
  end;
end;

function ConvertBitmapsToAlphaIcon(b: array of Vcl.Graphics.TBitmap; const IconFileName: string): Boolean;
var
  bmp: array of TGPBitmap;
  i: Integer;
{$IFDEF VER330}
  gdiptoken: Cardinal;
{$ELSE}
  gdiptoken: ULong_ptr;
{$ENDIF}
  StartupInput: TGdiplusStartupInput;
begin
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;

  GDIPCheck(GdiPlusStartup(gdiptoken, @StartupInput, nil));
  try
    SetLength(bmp, Length(b));
    for i := 0 to High(bmp) do
      bmp[i] := nil;

    try
      for i := 0 to High(b) do
      begin
        bmp[i] := TGPBitmap.Create(b[i].Handle, b[i].Palette);
        GDIPCheck(bmp[i].GetLastStatus);
      end;

      MergeImagesIntoIcon(bmp, IconFileName);

    finally
      for i := 0 to High(bmp) do
        FreeAndNil(bmp[i]);
    end;
  finally
    GdiplusShutdown(gdiptoken);
  end;
  Result := True;
end;

function ConvertKeyboardBitmapToAlphaIcon(ki: TKeyboardInfo; const IconFileName: string): Boolean;
var
  pkfh: PKeyboardFileHeader;
  ms: TMemoryStream;
  srcbmp: TGPBitmap;
  bmp: array[0..5] of TGPBitmap;
  i: Integer;
  bm: Vcl.Graphics.TBitmap;
{$IFDEF VER330}
  gdiptoken: Cardinal;
{$ELSE}
  gdiptoken: ULong_ptr;
{$ENDIF}
  StartupInput: TGdiplusStartupInput;
begin
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;

  GDIPCheck(GdiPlusStartup(gdiptoken, @StartupInput, nil));
  try
    pkfh := PKeyboardFileHeader(ki.MemoryDump.Memory);
    if pkfh.dwBitmapOffset = 0 then
      Exit(False);

    bm := nil;

    ms := TMemoryStream.Create;
    try
      ki.MemoryDump.Position := pkfh.dwBitmapOffset;
      ms.CopyFrom(ki.MemoryDump, pkfh.dwBitmapSize);
      ms.Position := 0;

      if Assigned(ki.Icon) and (ms.Size > sizeof(TIconDir)) then
      begin
        // .ico format

        if PIconDir(ms.Memory).idCount > 1 then
        begin
          // If we have more than 1 icon format in the file, we assume that
          // the developer knows what they are doing
          ki.Icon.SaveToFile(IconFileName);
          Exit(True);
        end;

        bm := Vcl.Graphics.TBitmap.Create;   // I4317
        bm.SetSize(16, 16);
        bm.PixelFormat := pf32Bit;
        DrawIconEx(bm.Canvas.Handle, 0, 0, ki.Icon.Handle,
          16, 16, 0, 0, DI_NORMAL);
        srcbmp := TGPBitmap.Create(bm.Handle, bm.Palette);
        GDIPCheck(srcbmp.GetLastStatus);
      end
      else if Assigned(ki.Bitmap) then
      begin
        // .bmp format
        // use GDI to convert graphic to 32 bit for GDI+ compatibility
        bm := Vcl.Graphics.TBitmap.Create;
        bm.SetSize(16, 16);
        bm.PixelFormat := pf32bit;
        bm.Canvas.Draw(0, 0, ki.Bitmap);

        srcbmp := TGPBitmap.Create(bm.Handle, bm.Palette);
        GDIPCheck(srcbmp.GetLastStatus);
      end
      else
        // Not a valid graphic
        Exit(False);
    finally
      ms.Free;
    end;

    bmp[0] := CreateIconInFrame(srcbmp, '', 0, 0, 16, 16, 1);
    bmp[1] := CreateIconInFrame(srcbmp, 'TIP_ICON_BASE_20', 2, 2, 20, 20, 1);
    bmp[2] := CreateIconInFrame(srcbmp, 'TIP_ICON_BASE_24', 4, 4, 24, 24, 1);
    bmp[3] := CreateIconInFrame(srcbmp, 'TIP_ICON_BASE_32', 3, 3, 32, 32, 1);
    bmp[4] := CreateIconInFrame(srcbmp, 'TIP_ICON_BASE_40', 12, 12, 40, 40, 1);
    bmp[5] := CreateIconInFrame(srcbmp, 'TIP_ICON_BASE_48', 5, 5, 48, 48, 2);

    MergeImagesIntoIcon(bmp, IconFileName);

    for i := 0 to 5 do
      FreeAndNil(bmp[i]);

    FreeAndNil(srcbmp);
    FreeAndNil(bm);
  finally
    GdiplusShutdown(gdiptoken);
  end;
  Result := True;
end;

function GetEncoderClsid(format: string; var clsid_encoder: TGUID): Boolean;
var
  num, size, j: UINT;
  p, q: PImageCodecInfo;
begin
  GetImageEncodersSize(num, size);
  if size = 0 then Exit(False);

  p := AllocMem(size);
  try
    GetImageEncoders(num, size, p);

    q := p;
    for j := 0 to num - 1 do
    begin
      if q.MimeType = format then
      begin
        clsid_encoder := q.Clsid;
        Exit(True);
      end;
      Inc(q);
    end;
  finally
    FreeMem(p);
  end;

  Exit(False);
end;

procedure MergeImagesIntoIcon(bmp: array of TGPBitmap; const Filename: string);
var
  dir: TIconDir;
  direntry: array of TIconDirEntry;
  fs: TFileStream;
  ms: array of TMemoryStream;
  i: Integer;
  dwOffset: DWord;
  clsid: TGUID;
  msa: IStream;
begin
  SetLength(direntry, Length(bmp));
  SetLength(ms, Length(bmp));

  if not GetEncoderClsid('image/png', clsid) then
    raise Exception.Create('No PNG format registered');

  fs := TFileStream.Create(Filename, fmCreate);
  with fs do
  try
    dir.idReserved := 0;
    dir.idType := 1;
    dir.idCount := Length(bmp);
    Write(dir, sizeof(dir));

    dwOffset := sizeof(TIconDir) + dir.idCount * sizeof(TIconDirEntry);

    for i := 0 to dir.idCount - 1 do
    begin
      ms[i] := TMemoryStream.Create;
      msa := TStreamAdapter.Create(ms[i], soReference);
      try
        GDIPCheck(bmp[i].Save(msa, clsid));
      finally
        msa := nil;
      end;

      direntry[i].bWidth := bmp[i].GetWidth;
      direntry[i].bHeight := bmp[i].GetHeight;
      direntry[i].bColorCount := 0;
      direntry[i].bReserved := 0;
      direntry[i].wPlanes := 1;
      direntry[i].wBitCount := 32;
      direntry[i].dwBytesInRes := ms[i].Size;
      direntry[i].dwImageOffset := dwOffset;
      Inc(dwOffset, ms[i].Size);
      Write(direntry[i], sizeof(TIconDirEntry));
    end;

    for i := 0 to dir.idCount - 1 do
    begin
      ms[i].Position := 0;
      CopyFrom(ms[i], ms[i].Size);
      ms[i].Free;
    end;

  finally
    Free;
  end;
end;

function CreateIconInFrame(SourceBitmap: TGPBitmap; const SourceFrameResourceName: string; FX, FY: Integer; FWidth, FHeight, FScale: Integer): TGPBitmap;
var
  bitmapdst, bitmapsrc: TGPBitmap;
  img: TGPGraphics;
  r: TResourceStream;
  rsa: IStream;
begin
  bitmapdst := TGPBitmap.Create(FWidth, FHeight);
  img := TGPGraphics.Create(bitmapdst);
  try
    GDIPCheck(img.GetLastStatus);
    if SourceFrameResourceName <> '' then
    begin
      r := TResourceStream.Create(HInstance, SourceFrameResourceName, RT_RCDATA);
      rsa := TStreamAdapter.Create(r, soReference);
      try
        bitmapsrc := TGPBitmap.Create(rsa);
        try
          GDIPCheck(bitmapsrc.GetLastStatus);
          GDIPCheck(img.DrawImage(bitmapsrc, 0, 0));
        finally
          bitmapsrc.Free;
        end;
      finally
        rsa := nil;
      end;
    end;

    GDIPCheck(img.DrawImage(SourceBitmap, FX, FY, 16 * FScale, 16 * FScale));
    img.Flush;
  finally
    img.Free;
  end;

  Result := bitmapdst;
end;


function GetTrayIconSize: TPoint;   // I4314
  function GetDPI: TPoint;
  var
    h: HDC;
  begin
    h := GetDC(0);
    if h <> 0 then
    begin
      Result.X := GetDeviceCaps(h, LOGPIXELSX);
      Result.Y := GetDeviceCaps(h, LOGPIXELSY);
      ReleaseDC(0, h);
    end
    else
    begin
      Result.X := 96;
      Result.Y := 96;
    end;
  end;

var
  dpi: TPoint;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  if IsProcessDPIAware then
{$WARN SYMBOL_DEPRECATED DEFAULT}
  begin
    dpi := GetDPI;
  end
  else
  begin
    // We have to simulate for prettiness
    with TRegistry.Create do
    try
      if OpenKeyReadOnly('Control Panel\Desktop\WindowMetrics') and
        ValueExists('AppliedDPI') then
      begin
        try
          dpi.X := ReadInteger('AppliedDPI');
        except
          dpi.X := 96;
        end;
      end
      else
        dpi.X := 96;
      dpi.Y := dpi.X;
    finally
      Free;
    end;
  end;

  Result := Point(MulDiv(16, dpi.X, 96), MulDiv(16, dpi.Y, 96));
end;

end.
