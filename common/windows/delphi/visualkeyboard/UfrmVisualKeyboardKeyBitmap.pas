(*
  Name:             UfrmVisualKeyboardKeyBitmap
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial refactor for new visual keyboard
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmVisualKeyboardKeyBitmap;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ExtDlgs, VisualKeyboardParameters, Dialogs;

type
  TfrmVisualKeyboardKeyBitmap = class(TForm)
    imgKeyImage: TImage;
    lblOptions2: TLabel;
    lblOptions3: TLabel;
    lblOptions4: TLabel;
    lblKeyImageParameters: TLabel;
    lblOptions5: TLabel;
    Label2: TLabel;
    cmdBrowseKeyImage: TButton;
    editKeyImageParameters: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    dlgBrowseBitmap: TOpenPictureDialog;
    procedure cmdBrowseKeyImageClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    //FParameters: string;
    procedure UpdateKeyBitmap;
    function GetKeyBitmap: TKeyBitmapInfo;
    procedure SetKeyBitmap(const Value: TKeyBitmapInfo);
  public
    property KeyBitmap: TKeyBitmapInfo read GetKeyBitmap write SetKeyBitmap;
  end;

implementation

{$R *.DFM}

procedure TfrmVisualKeyboardKeyBitmap.cmdBrowseKeyImageClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  if dlgBrowseBitmap.Execute then
  begin
    bmp := TBitmap.Create;
    try
      bmp.LoadFromFile(dlgBrowseBitmap.FileName);
      imgKeyImage.Picture.Bitmap := bmp;
    finally
      bmp.Free;
    end;
    UpdateKeyBitmap;
  end;
end;

procedure TfrmVisualKeyboardKeyBitmap.UpdateKeyBitmap;
begin
  if Assigned(imgKeyImage.Picture.Bitmap) then
  begin
    imgKeyImage.Width := imgKeyImage.Picture.Width div 6;
    imgKeyImage.Height := imgKeyImage.Picture.Height;
  end
  else
    imgKeyImage.Picture := nil;
end;


procedure TfrmVisualKeyboardKeyBitmap.cmdOKClick(Sender: TObject);
begin
  if not Assigned(imgKeyImage.Picture.Bitmap) then
  begin
    ShowMessage('You must load a bitmap to continue.');
    Exit;
  end;

  {try
    KeyBtnParametersFromString(editKeyImageParameters.Text);
  except
    on E:EKeyBtnParameters do
    begin
      ShowMessage(E.Message);
      editKeyImageParameters.SetFocus;
    end;
  end;}

  ModalResult := mrOk;
end;

function TfrmVisualKeyboardKeyBitmap.GetKeyBitmap: TKeyBitmapInfo;
begin
  Result.Bitmap := TBitmap.Create;
  Result.Bitmap.Assign(imgKeyImage.Picture.Bitmap);
  //Result.Params :=  editKeyImageParameters.Text;
end;

procedure TfrmVisualKeyboardKeyBitmap.SetKeyBitmap(const Value: TKeyBitmapInfo);
begin
  if Assigned(Value.Bitmap)
    then imgKeyImage.Picture.Bitmap := Value.Bitmap
    else imgKeyImage.Picture := nil;

  UpdateKeyBitmap;
  
  //editKeyImageParameters.Text := Value.ParamStr;
end;

end.
