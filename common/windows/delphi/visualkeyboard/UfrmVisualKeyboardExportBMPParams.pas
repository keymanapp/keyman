(*
  Name:             UfrmVisualKeyboardExportBMPParams
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
                    22 Jan 2007 - mcdurdin - Rework to support pixel size for export
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmVisualKeyboardExportBMPParams;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, System.ImageList;

type
  TfrmVisualKeyboardExportBMPParams = class(TForm)
    gbOutputType: TGroupBox;
    rbMulti: TRadioButton;
    rbSingle: TRadioButton;
    Label1: TLabel;
    lblFileName: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    gbOptions: TGroupBox;
    chkUnicode: TCheckBox;
    chkANSI: TCheckBox;
    cmdOK: TButton;
    cmdCancel: TButton;
    imglistKeys: TImageList;
    gbSize: TGroupBox;
    lblPixelWidth: TLabel;
    editPixelWidth: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    function GetMultiFile: Boolean;
    function GetOutputANSI: Boolean;
    function GetOutputUnicode: Boolean;
    procedure SetFileName(Value: string);
    function GetPixelWidth: Integer;
  public
    property FileName: string write SetFileName;
    property MultiFile: Boolean read GetMultiFile;
    property OutputUnicode: Boolean read GetOutputUnicode;
    property OutputANSI: Boolean read GetOutputANSI;
    property PixelWidth: Integer read GetPixelWidth;
  end;

implementation

{$R *.DFM}

uses
  ErrorControlledRegistry, 
  RegistryKeys;

{ TfrmVisualKeyboardExportBMPParams }

function TfrmVisualKeyboardExportBMPParams.GetMultiFile: Boolean;
begin
  Result := rbMulti.Checked;
end;

function TfrmVisualKeyboardExportBMPParams.GetOutputANSI: Boolean;
begin
  Result := chkANSI.Checked;
end;

function TfrmVisualKeyboardExportBMPParams.GetOutputUnicode: Boolean;
begin
  Result := chkUnicode.Checked;
end;

function TfrmVisualKeyboardExportBMPParams.GetPixelWidth: Integer;
begin
  Result := StrToIntDef(editPixelWidth.Text, 720);
end;

procedure TfrmVisualKeyboardExportBMPParams.SetFileName(Value: string);
begin
  lblFileName.Caption := ChangeFileExt(ExtractFileName(Value), '') + '_U_<shift>' +
    ExtractFileExt(Value);
end;

procedure TfrmVisualKeyboardExportBMPParams.FormCreate(Sender: TObject);
var
  b: Boolean;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_IDEVisualKeyboard_CU) then
    begin
      if ValueExists(SRegValue_IDEVKbd_ExportBMPMulti)
        then b := ReadString(SRegValue_IDEVKbd_ExportBMPMulti) = '1'
        else b := False;
      rbMulti.Checked := b;
      rbSingle.Checked := not b;

      chkANSI.Checked := not ValueExists(SRegValue_IDEVKbd_ExportBMPANSI) or
        (ReadString(SRegValue_IDEVKbd_ExportBMPANSI) = '1');

      chkUnicode.Checked := not ValueExists(SRegValue_IDEVKbd_ExportBMPUnicode) or
        (ReadString(SRegValue_IDEVKbd_ExportBMPUnicode) = '1');

      if ValueExists(SRegValue_IDEVKbd_ExportBMPPixelWidth) then
        editPixelWidth.Text := IntToStr(ReadInteger(SRegValue_IDEVKbd_ExportBMPPixelWidth));
    end;
  finally
    Free;
  end;
end;

procedure TfrmVisualKeyboardExportBMPParams.cmdOKClick(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_IDEVisualKeyboard_CU, True) then
    begin
      if rbMulti.Checked
        then WriteString(SRegValue_IDEVKbd_ExportBMPMulti, '1')
        else WriteString(SRegValue_IDEVKbd_ExportBMPMulti, '0');

      if chkANSI.Checked
        then WriteString(SRegValue_IDEVKbd_ExportBMPANSI, '1')
        else WriteString(SRegValue_IDEVKbd_ExportBMPANSI, '0');

      if chkUnicode.Checked
        then WriteString(SRegValue_IDEVKbd_ExportBMPUnicode, '1')
        else WriteString(SRegValue_IDEVKbd_ExportBMPUnicode, '0');

      WriteInteger(SRegValue_IDEVKbd_ExportBMPPixelWidth, StrToIntDef(editPixelWidth.Text, 720));
    end;
  finally
    Free;
  end;
  ModalResult := mrOk;
end;

end.

