object NotificiationForm: TNotificiationForm
  Left = 828
  Top = 354
  Width = 484
  Height = 424
  Caption = 'Example app to demonstration WTSRegisterSessionNotification'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    468
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 48
    Width = 449
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object RegisterButton: TButton
    Left = 16
    Top = 16
    Width = 169
    Height = 25
    Caption = 'Register for Notifications'
    TabOrder = 1
    OnClick = RegisterButtonClick
  end
  object CheckBoxAllSessions: TCheckBox
    Left = 200
    Top = 24
    Width = 257
    Height = 17
    Caption = 'Notify for all sessions'
    TabOrder = 2
  end
end
