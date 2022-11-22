object frmSMTPSetup: TfrmSMTPSetup
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'SMTP Configuration'
  ClientHeight = 99
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblSMTPServer: TLabel
    Left = 21
    Top = 23
    Width = 61
    Height = 13
    Caption = 'SMTP &Server'
    FocusControl = editSMTPServer
  end
  object editSMTPServer: TEdit
    Left = 88
    Top = 20
    Width = 161
    Height = 21
    TabOrder = 0
  end
  object cmdOK: TButton
    Left = 57
    Top = 55
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 138
    Top = 55
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
