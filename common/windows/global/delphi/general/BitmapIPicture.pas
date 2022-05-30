(*
  Name:             BitmapIPicture
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    20 Jun 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
*)
unit BitmapIPicture;

interface

uses Windows, Graphics, ActiveX;

type
  TBitmapIPicture = class(TBitmap)
    procedure LoadFromIPicture(p: IPicture);
  end;

implementation

uses Classes;

{ TBitmapIPicture }

procedure TBitmapIPicture.LoadFromIPicture(p: IPicture);
var
  p2, pt: TPoint;
  x: Smallint;
begin
  if p = nil then Assign(nil)
  else
  begin
    p.get_Type(x);
    p.get_Width(pt.x);
    p.get_Height(pt.y);

    p2.x := MulDiv(pt.x, GetDeviceCaps(Canvas.Handle, LOGPIXELSX), 2540);
    p2.y := MulDiv(pt.y, GetDeviceCaps(Canvas.Handle, LOGPIXELSY), 2540);

    Width := p2.x;
    Height := p2.y;

    p.Render(Canvas.Handle, 0,0,p2.x,p2.y,0,pt.y,pt.x,-pt.y, Rect(0,0,0,0));
  end;
end;

end.
 
