object frmSendURLsToEmail: TfrmSendURLsToEmail
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Send Debug Addresses to Email'
  ClientHeight = 128
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblEmail: TLabel
    Left = 16
    Top = 19
    Width = 77
    Height = 13
    Caption = 'Email Addresses'
    FocusControl = editEmail
  end
  object lblEmailNote: TLabel
    Left = 16
    Top = 51
    Width = 256
    Height = 13
    Caption = 'Separate multiple addresses with comma or semicolon'
  end
  object editEmail: TEdit
    Left = 104
    Top = 16
    Width = 225
    Height = 21
    TabOrder = 0
    OnChange = editEmailChange
  end
  object cmdOK: TButton
    Left = 138
    Top = 87
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 219
    Top = 87
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cmdSMTPSettings: TButton
    Left = 8
    Top = 87
    Width = 97
    Height = 25
    Caption = '&SMTP Settings...'
    TabOrder = 1
    OnClick = cmdSMTPSettingsClick
  end
end
