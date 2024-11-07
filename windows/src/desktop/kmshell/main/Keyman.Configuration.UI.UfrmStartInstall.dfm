object frmStartInstall: TfrmStartInstall
  Left = 0
  Top = 0
  Caption = 'Keyman Update'
  ClientHeight = 225
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblInstallUpdate: TLabel
    Left = 128
    Top = 96
    Width = 175
    Height = 19
    Caption = 'Keyman update available'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cmdInstall: TButton
    Left = 228
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Install'
    ModalResult = 1
    TabOrder = 0
  end
  object cmdLater: TButton
    Left = 336
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
  end
end
