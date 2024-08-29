object frmStartInstall: TfrmStartInstall
  Left = 0
  Top = 0
  Caption = 'frmStartInstall'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Install: TButton
    Left = 168
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 0
    OnClick = InstallClick
  end
  object Later: TButton
    Left = 288
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Later'
    TabOrder = 1
    OnClick = LaterClick
  end
end
