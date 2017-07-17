(*
  Name:             UfrmOSKPlugInBase
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
*)
unit UfrmOSKPlugInBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfrmOSKPlugInBase = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ConstrainSizing(SizeDir: Integer; var Rect: TRect); virtual;
    procedure Deactivating; virtual;
  end;

implementation

{$R *.dfm}

{ TfrmOSKPlugInBase }

procedure TfrmOSKPlugInBase.ConstrainSizing(SizeDir: Integer; var Rect: TRect);
begin
end;

procedure TfrmOSKPlugInBase.Deactivating;
begin
end;

end.
