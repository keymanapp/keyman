unit olepro32;

interface

uses ActiveX, Windows, SysUtils;

function OleCreatePictureIndirect(const PictDesc: TPictDesc; const iid: TIID;  fOwn: BOOL; out vObject): HResult stdcall;

implementation

function OleCreatePictureIndirect(const PictDesc: TPictDesc; const iid: TIID;  fOwn: BOOL; out vObject): HResult stdcall; external 'olepro32.dll';

end.
