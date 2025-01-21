object frmStartInstall: TfrmStartInstall
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Update'
  ClientHeight = 142
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblInstallUpdate: TLabel
    Left = 72
    Top = 48
    Width = 180
    Height = 19
    Caption = 'Keyman update available.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cmdInstall: TButton
    Left = 140
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Install'
    ModalResult = 1
    TabOrder = 0
  end
  object cmdLater: TButton
    Left = 234
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
  end
end
