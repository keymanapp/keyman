object frmStartInstallNow: TfrmStartInstallNow
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Update'
  ClientHeight = 164
  ClientWidth = 354
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
  object lblUpdateMessage: TLabel
    Left = 16
    Top = 32
    Width = 323
    Height = 19
    Caption = 'Updating now will require a computer restart.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object cmdInstall: TButton
    Left = 147
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Update'
    ModalResult = 1
    TabOrder = 0
  end
  object cmdLater: TButton
    Left = 247
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
  end
end
