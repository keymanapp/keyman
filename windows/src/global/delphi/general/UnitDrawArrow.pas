unit UnitDrawArrow;

interface

uses Windows, Graphics;

procedure ArrowTo(Canvas: TCanvas; X, Y: Integer; bFill: Boolean = True; nWidth: Integer = 7; fTheta: Extended = 0.5); overload;
procedure ArrowTo(Canvas: TCanvas; Point: TPoint; bFill: Boolean = True; nWidth: Integer = 7; fTheta: Extended = 0.5); overload;

implementation

uses Math, Classes, System.Types;

procedure ArrowTo(Canvas: TCanvas; X, Y: Integer; bFill: Boolean = True; nWidth: Integer = 7; fTheta: Extended = 0.5);
begin
  ArrowTo(Canvas, Point(X,Y), bFill, nWidth, fTheta);
end;

procedure ArrowTo(Canvas: TCanvas; Point: TPoint; bFill: Boolean = True; nWidth: Integer = 7; fTheta: Extended = 0.5);
var
  pFrom, pBase: TPoint;
  aptPoly: array[0..2] of TPoint;
  vecLine, vecLeft: array[0..1] of Extended;
  fLength, th, ta: Extended;
begin;
	// get from point
	MoveToEx(Canvas.Handle, 0, 0, @pFrom);

	// set to point
	aptPoly[0].x := Point.x;
	aptPoly[0].y := Point.y;

	// build the line vector
	vecLine[0] := aptPoly[0].x - pFrom.x;
	vecLine[1] := aptPoly[0].y - pFrom.y;

	// build the arrow base vector - normal to the line
	vecLeft[0] := -vecLine[1];
	vecLeft[1] := vecLine[0];

	// setup length parameters
	fLength := sqrt(vecLine[0] * vecLine[0] + vecLine[1] * vecLine[1]);
	th := nWidth / (2.0 * fLength);
	ta := nWidth / (2.0 * (tan(fTheta) / 2.0) * fLength);

	// find the base of the arrow
	pBase.x := Trunc(aptPoly[0].x + -ta * vecLine[0]);
	pBase.y := Trunc(aptPoly[0].y + -ta * vecLine[1]);

	// build the points on the sides of the arrow
	aptPoly[1].x := Trunc(pBase.x + th * vecLeft[0]);
	aptPoly[1].y := Trunc(pBase.y + th * vecLeft[1]);
	aptPoly[2].x := Trunc(pBase.x + -th * vecLeft[0]);
	aptPoly[2].y := Trunc(pBase.y + -th * vecLeft[1]);

	MoveToEx(Canvas.Handle, pFrom.x, pFrom.y, nil);

	if bFill then
  begin
  	// draw we're fillin'...
		LineTo(Canvas.Handle, aptPoly[0].x, aptPoly[0].y);
		Polygon(Canvas.Handle, aptPoly, 3);
	end
	else
  begin
  	// ... or even jes chillin'...
		LineTo(Canvas.Handle, pBase.x, pBase.y);
		LineTo(Canvas.Handle, aptPoly[1].x, aptPoly[1].y);
		LineTo(Canvas.Handle, aptPoly[0].x, aptPoly[0].y);
		LineTo(Canvas.Handle, aptPoly[2].x, aptPoly[2].y);
		LineTo(Canvas.Handle, pBase.x, pBase.y);
		MoveToEx(Canvas.Handle, aptPoly[0].x, aptPoly[0].y, nil);
	end;
end;

end.
 