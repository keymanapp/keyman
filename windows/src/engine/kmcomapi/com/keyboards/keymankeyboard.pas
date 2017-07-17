(*
  Name:             keymankeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Add Get_LayoutType function
                    27 Mar 2008 - mcdurdin - I1374 - Font helper code - chars and scripts used
                    27 Mar 2008 - mcdurdin - I1220 - Fixup ms office languages - read languages required from keyboard
                    14 Jun 2008 - mcdurdin - I1400 - Add PrimaryLanguage property
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymankeyboard;

interface

uses
  Windows, ActiveX, keymanautoobject, keymanapi_TLB, internalinterfaces;

type
  TKeymanKeyboard = class(TKeymanAutoObject, IKeymanKeyboard)
  protected
    { IKeymanKeyboard }
    function Get_Bitmap: IPicture; virtual; safecall;
    function Get_Copyright: WideString; virtual; safecall;
    function Get_DefaultBCP47Languages: WideString; virtual; safecall;
    function Get_DefaultHotkey: IKeymanHotkey; virtual; safecall;
    function Get_DefaultPrimaryLanguage: Integer; virtual; safecall;
    function Get_DefaultWindowsLanguages: WideString; virtual; safecall;
    function Get_Encodings: KeymanKeyboardEncodings; virtual; safecall;
    function Get_Filename: WideString; virtual; safecall;
    function Get_ID: WideString; virtual; safecall;
    function Get_LayoutType: KeymanKeyboardLayoutType; virtual; safecall;
    function Get_Message: WideString; virtual; safecall;
    function Get_Name: WideString; virtual; safecall;
    function Get_Version: WideString; virtual; safecall;
    function GetCharsUsed: WideString; virtual; safecall;
  end;

  TKeyboardList = TAutoObjectList;

implementation

{ TKeymanKeyboard }

function TKeymanKeyboard.GetCharsUsed: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Bitmap: IPicture;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Copyright: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_DefaultBCP47Languages: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_DefaultHotkey: IKeymanHotkey;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_DefaultPrimaryLanguage: Integer;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_DefaultWindowsLanguages: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Encodings: KeymanKeyboardEncodings;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Filename: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_ID: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_LayoutType: KeymanKeyboardLayoutType;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Message: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Name: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

function TKeymanKeyboard.Get_Version: WideString;
begin
  Error(Cardinal(E_NOTIMPL));
end;

end.

