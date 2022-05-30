{******************************************************************************}
{ JEDI API example WinLogon Notification Package  			                       }
{ http://jedi-apilib.sourceforge.net					                                 }
{ 									                                                           }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 									                                                           }
{ Author(s): stOrM!, Christian Wimmer   			                                 }
{ Creation date: 23th May 2008 					 	                                     }
{ Last modification date:	26th May 2008				                                 }
{ 									                                                           }
{ Description: Demonstrates how to create a Winlogon Notification Package and  }
{    draws a transparent window inside Winlogon Process	containing a 	         }
{ 	 PNG Image							                                                   }
{ Preparations: JwaWindows, any layer based graphic apllication e.g. Gimp or   }
{ 				Adobe Photoshop				                                               }
{ Article link:   							                                               }
{ http://blog.delphi-jedi.net/2008/05/27/                                      }
{      winlogon-notification-packagewinlogon-notification-package/	           }
{ Version history: 23/05/2008 first release        			                       }
{ 									                                                           }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 	       }
{ productive environments.						                                         }
{ The code has surely some errors that need to be fixed. In such a case	       }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.						                                         }
{ 									                                                           }
{ Be aware that third party components used by this example may have other     }
{ licenses. Please see the corresponding component what they are.              }
{ You have to download them manually.                                          }
{ 									                                                           }
{ The JEDI API Logo is copyrighted and must not be used without permission     }
{ 									                                                           }
{ External components:                                                         }
{   PNG components: http://www.thany.org/article/18/Delphi_components          }
{ 	Graphics32 : http://sourceforge.net/projects/graphics32/                   }
{******************************************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, GR32;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BMP32 : TBitmap32;
    BlendF: TBlendFunction;
    P: TPoint;
    Size: TSize;
    procedure BlendIt;
  end;

var
  Form1: TForm1;

  procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                                Filename: String;
                                out AlphaChannelUsed: Boolean); overload;

  procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                                SrcStream: TStream;
                                out AlphaChannelUsed: Boolean); overload;

  procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                                szSection : PChar;
                                szName    : String;
                                out AlphaChannelUsed: Boolean); overload;

implementation

{$R *.dfm}
{$R Logo.res}

uses PNGImage;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                              SrcStream: TStream;
                              out AlphaChannelUsed: Boolean);
var
  PNGObject: TPNGObject;
  TransparentColor: TColor32;
  PixelPtr: PColor32;
  AlphaPtr: PByte;
  X, Y: Integer;
begin
  PNGObject := nil;
  try
    PNGObject := TPngObject.Create;
    PNGObject.LoadFromStream(SrcStream);

    DstBitmap.Assign(PNGObject);
    DstBitmap.ResetAlpha;

    case PNGObject.TransparencyMode of
      ptmPartial:
        begin
          if (PNGObject.Header.ColorType = COLOR_GRAYSCALEALPHA) or
             (PNGObject.Header.ColorType = COLOR_RGBALPHA) then
          begin
            PixelPtr := PColor32(@DstBitmap.Bits[0]);
            for Y := 0 to DstBitmap.Height - 1 do
            begin
              AlphaPtr := PByte(PNGObject.AlphaScanline[Y]);
              for X := 0 to DstBitmap.Width - 1 do
              begin
                PixelPtr^ := (PixelPtr^ and $00FFFFFF) or (TColor32(AlphaPtr^) shl 24);
                Inc(PixelPtr);
                Inc(AlphaPtr);
              end;
            end;

            AlphaChannelUsed := True;
          end;
        end;
      ptmBit:
        begin
          TransparentColor := Color32(PNGObject.TransparentColor);
          PixelPtr := PColor32(@DstBitmap.Bits[0]);
          for X := 0 to (DstBitmap.Height - 1) * (DstBitmap.Width - 1) do
          begin
            if PixelPtr^ = TransparentColor then
              PixelPtr^ := PixelPtr^ and $00FFFFFF;
            Inc(PixelPtr);
          end;

          AlphaChannelUsed := True;
        end;
      ptmNone:
        AlphaChannelUsed := False;
    end;
  finally
    if Assigned(PNGObject) then PNGObject.Free;
  end;
end;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                              Filename: String;
                              out AlphaChannelUsed: Boolean);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadPNGintoBitmap32(DstBitmap, FileStream, AlphaChannelUsed);
  finally
    FileStream.Free;
  end;
end;

procedure LoadPNGintoBitmap32(DstBitmap: TBitmap32;
                              szSection : PChar;
                              szName    : String;
                              out AlphaChannelUsed: Boolean);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(hInstance, szName, PChar(szSection));
  try
    LoadPNGintoBitmap32(DstBitmap, Stream, AlphaChannelUsed);
    finally
     Stream.Free;
   end;
end;

{ Works only Win2000_Up }

procedure TForm1.BlendIt;
  var
    Alpha: Boolean;
begin
  BMP32 := TBitmap32.Create;
 {Load PNG from File
  LoadPNGintoBitmap32(BMP32, ExtractFilePath(ParamStr(0)) + 'Skin2.png', Alpha);
 }
  // Load PNG From Resource
  LoadPNGintoBitmap32(BMP32, PChar('PNG'), 'JWCL', Alpha);

  setWindowLong(Handle, GWL_EXSTYLE,
  getWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED {or WS_EX_TRANSPARENT});
  // WS_EX_TRANSPARENT makes the Window for MouseClicks transparent...
  
  BlendF.BlendOp := AC_SRC_OVER;
  BlendF.BlendFlags := 0;
  BlendF.SourceConstantAlpha := 255;
  BlendF.AlphaFormat := AC_SRC_ALPHA;
  P := Point(0, 0);
  Size.cx := BMP32.Width;
  Size.cy := BMP32.Height;

  UpdateLayeredWindow(Handle, 0, nil, @Size, BMP32.Handle, @P, 0, @BlendF, ULW_ALPHA);

  // Set Window on Top
  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height,
  SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);

  // Set Parent to Desktop
  SetWindowLong(Handle, GWL_HWNDPARENT, 0);

  // Hide Window from Taskbar
  SetWindowLong(Handle, GWL_EXSTYLE,
  GetWindowLong(Handle, GWL_EXSTYLE) or
  WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BlendIt;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BMP32);
end;

end.
