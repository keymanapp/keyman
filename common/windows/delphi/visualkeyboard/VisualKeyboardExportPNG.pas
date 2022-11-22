(*
  Name:             VisualKeyboardExportPNG
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit VisualKeyboardExportPNG;  // I3306

interface

uses Graphics, VisualKeyboard, VisualKeyboardExportBMP;

type
  TVisualKeyboardExportPNG = class(TVisualKeyboardExportBMP)
  protected
    procedure SaveToFile(bmp: TBitmap; FileName: WideString); override;
  end;

implementation

uses
  PngImage, SysUtils, Classes;

{ TVisualKeyboardExportPNG }

procedure TVisualKeyboardExportPNG.SaveToFile(bmp: TBitmap; FileName: WideString);
var
  stream: TStream;
begin
  with TPngImage.Create do
  try
    Assign(bmp);

    stream := TFileStream.Create(FileName, fmCreate);
    try
      SaveToStream(stream);
    finally
      stream.Free;
    end;
  finally
    Free;
  end;
end;

end.
