(*
  Name:             OnlineConstants
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    17 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Remove KeymanDeveloper_Light, test matching product IDs
                    04 Jan 2007 - mcdurdin - Add Module_Identifiers
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    22 Oct 2010 - mcdurdin - I2521 - Activation Client does not activate Developer 8.0
                    26 Jun 2012 - mcdurdin - I3380 - KM9 - Add Keyman 9 product ids
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Temp rename of *_80 to *_80_ for validation
*)
unit OnlineConstants;

interface

const
  { Version 7.0 }

  OnlineProductID_KeymanDesktop_Light_70 = 8;
  OnlineProductID_KeymanDesktop_Professional_70 = 1;
  OnlineProductID_KeymanDesktop_Corporate_70 = 2;

  //OnlineProductID_KeymanDeveloper_Standard = 3;
  OnlineProductID_KeymanDeveloper_Professional_70 = 4;
  //OnlineProductID_KeymanDeveloper_Light = 5;

  OnlineProductID_KeymanDeveloper_KeymanWebPack_70 = 6;
  OnlineProductID_KeymanDeveloper_BrandingPack_70 = 7;

  OnlineProductIDs_KeymanDeveloper_Edition_70: array[0..1] of Integer = (3,4); //
  OnlineProduct_KeymanDeveloper_Edition_Names_70: array[0..1] of WideString = (
    'Keyman Developer Standard', 'Keyman Developer Professional'); //, 'Keyman Developer Light');
  OnlineProduct_KeymanDeveloper_Edition_ShortNames_70: array[0..1] of WideString = (
    'Standard Edition', 'Professional Edition'); // 'Light Edition');

  OnlineProductIDs_KeymanDeveloper_Module_70: array[0..1] of Integer = (6, 7);
  OnlineProduct_KeymanDeveloper_Module_Names_70: array[0..1] of WideString = (
    'KeymanWeb Pack', 'Branding Pack');
  OnlineProduct_KeymanDeveloper_Module_Identifiers_70: array[0..1] of WideString = (
    'KeymanWebPack', 'BrandingPack');

  { Version 8.0 }

  OnlineProductID_KeymanDesktop_Professional_80_ = 12;
  OnlineProductID_KeymanDesktop_Light_80_ = 13;
  OnlineProductID_KeymanDesktop_Corporate_80_ = 16;

  OnlineProductID_KeymanDeveloper_Professional_80_ = 14;
  OnlineProductID_KeymanDeveloper_BrandingPack_80_ = 15;

  OnlineProductIDs_KeymanDeveloper_Module_80_: array[0..0] of Integer = (15);
  OnlineProduct_KeymanDeveloper_Module_Names_80_: array[0..0] of WideString = ('Branding Pack');
  OnlineProduct_KeymanDeveloper_Module_Identifiers_80_: array[0..0] of WideString = ('BrandingPack');

  OnlineProductIDs_KeymanDeveloper_80_: array[0..1] of Integer = (14,15);  //I2521

  { Version 9.0 }

  OnlineProductID_KeymanDesktop_Professional_90_ = 20;
  OnlineProductID_KeymanDesktop_Light_90_ = 21;

  OnlineProductID_KeymanDeveloper_Professional_90_ = 22;
  OnlineProductID_KeymanDeveloper_BrandingPack_90_ = 23;

  OnlineProductIDs_KeymanDeveloper_Module_90_: array[0..0] of Integer = (23);
  OnlineProduct_KeymanDeveloper_Module_Names_90_: array[0..0] of WideString = ('Branding Pack');
  OnlineProduct_KeymanDeveloper_Module_Identifiers_90_: array[0..0] of WideString = ('BrandingPack');

  OnlineProductIDs_KeymanDeveloper_90_: array[0..1] of Integer = (22,23);  //I2521

  { Version 10.0 }

  OnlineProductID_KeymanDesktop_100 = 30;
  OnlineProductID_KeymanDeveloper_100 = 31;

  { Other Identifiers }

//  OnlineLicenceTypeID_Freeware = 1;
//  OnlineLicenceTypeID_Global = 2;
//  OnlineLicenceTypeID_Shareware = 3;
//
//const
//  DAYS_DEMO_VERSION_KeymanDeveloper = 30;
//
//function GetKeymanDeveloperModuleIndex(OnlineProductID: Integer): Integer; deprecated;

function IsKeymanDeveloper(OnlineProductID: Integer): Boolean; deprecated; //I2521
function IsKeymanDesktopEdition(OnlineProductID: Integer): Boolean; deprecated;
function IsMatchingProductID(ProductID, KeyboardProductID: Integer): Boolean; deprecated;

function OnlineProductName(OnlineProductID: Integer): string;

implementation

uses
  System.SysUtils,

  KeymanVersion;

function IsKeymanDeveloper(OnlineProductID: Integer): Boolean; //I2521
begin
  Result := OnlineProductID in
    [OnlineProductID_KeymanDeveloper_Professional_70,
     OnlineProductID_KeymanDeveloper_KeymanWebPack_70,
     OnlineProductID_KeymanDeveloper_BrandingPack_70,
     OnlineProductID_KeymanDeveloper_Professional_80_,
     OnlineProductID_KeymanDeveloper_BrandingPack_80_,
     OnlineProductID_KeymanDeveloper_Professional_90_,
     OnlineProductID_KeymanDeveloper_BrandingPack_90_,
     OnlineProductID_KeymanDeveloper_100];
end;

function IsMatchingProductID(ProductID, KeyboardProductID: Integer): Boolean;
begin
  Result := (ProductID = KeyboardProductID) or
          ((KeyboardProductID = OnlineProductID_KeymanDesktop_Professional_70) and
            IsKeymanDesktopEdition(ProductID));
end;

function IsKeymanDesktopEdition(OnlineProductID: Integer): Boolean;
begin
  Result := OnlineProductID in [
    OnlineProductID_KeymanDesktop_Light_80_,
    OnlineProductID_KeymanDesktop_Professional_80_,
    OnlineProductID_KeymanDesktop_Corporate_80_,
    OnlineProductID_KeymanDesktop_Light_90_,
    OnlineProductID_KeymanDesktop_Professional_90_,
    OnlineProductID_KeymanDesktop_100];
end;

{function GetKeymanDeveloperModuleIndex(OnlineProductID: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to High(OnlineProductIDs_KeymanDeveloper_Module_90_) do
    if OnlineProductIDs_KeymanDeveloper_Module_90_[i] = OnlineProductID then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;}

function OnlineProductName(OnlineProductID: Integer): string;
begin
  case OnlineProductID of
  OnlineProductID_KeymanDesktop_Light_70:         Result := 'Keyman Desktop Light '+SKeymanVersion70;
  OnlineProductID_KeymanDesktop_Professional_70:  Result := 'Keyman Desktop Professional '+SKeymanVersion70;
  OnlineProductID_KeymanDesktop_Corporate_70:     Result := 'Keyman Desktop Corporate '+SKeymanVersion70;

  OnlineProductID_KeymanDeveloper_Professional_70:  Result := 'Keyman Developer Professional '+SKeymanVersion70;

  OnlineProductID_KeymanDeveloper_KeymanWebPack_70:  Result := 'Keyman Developer '+SKeymanVersion70+' KeymanWeb Pack';
  OnlineProductID_KeymanDeveloper_BrandingPack_70:   Result := 'Keyman Developer '+SKeymanVersion70+' Branding Pack';

  OnlineProductID_KeymanDesktop_Professional_80_:    Result := 'Keyman Desktop Professional '+SKeymanVersion80;
  OnlineProductID_KeymanDesktop_Light_80_:           Result := 'Keyman Desktop Light '+SKeymanVersion80;
  OnlineProductID_KeymanDesktop_Corporate_80_:       Result := 'Keyman Desktop Corporate '+SKeymanVersion80;

  OnlineProductID_KeymanDeveloper_Professional_80_:  Result := 'Keyman Developer '+SKeymanVersion80;
  OnlineProductID_KeymanDeveloper_BrandingPack_80_:  Result := 'Keyman Developer '+SKeymanVersion80+' Branding Pack';

  OnlineProductID_KeymanDesktop_Professional_90_:    Result := 'Keyman Desktop Professional '+SKeymanVersion90;
  OnlineProductID_KeymanDesktop_Light_90_:           Result := 'Keyman Desktop Light '+SKeymanVersion90;

  OnlineProductID_KeymanDeveloper_Professional_90_:  Result := 'Keyman Developer '+SKeymanVersion90;
  OnlineProductID_KeymanDeveloper_BrandingPack_90_:  Result := 'Keyman Developer '+SKeymanVersion90+' Branding Pack';

  OnlineProductID_KeymanDesktop_100:  Result := 'Keyman Desktop '+SKeymanVersion100;
  OnlineProductID_KeymanDeveloper_100:  Result := 'Keyman Developer '+SKeymanVersion100;

  else Result := 'Product '+IntToStr(OnlineProductID);
  end;
end;
end.

