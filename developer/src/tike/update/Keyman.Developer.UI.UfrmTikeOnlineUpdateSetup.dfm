object frmTikeOnlineUpdateSetup: TfrmTikeOnlineUpdateSetup
  Left = 192
  Top = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Configure HTTP Proxy'
  ClientHeight = 145
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblHTTPProxy: TLabel
    Left = 32
    Top = 20
    Width = 57
    Height = 13
    Caption = '&HTTP proxy'
    FocusControl = editProxy
  end
  object lblPort: TLabel
    Left = 232
    Top = 20
    Width = 19
    Height = 13
    Caption = '&Port'
    FocusControl = editPort
  end
  object lblUsername: TLabel
    Left = 32
    Top = 47
    Width = 48
    Height = 13
    Caption = '&Username'
  end
  object Label2: TLabel
    Left = 32
    Top = 75
    Width = 46
    Height = 13
    Caption = 'P&assword'
  end
  object editProxy: TEdit
    Left = 92
    Top = 16
    Width = 129
    Height = 21
    TabOrder = 0
  end
  object editPort: TEdit
    Left = 260
    Top = 16
    Width = 37
    Height = 21
    TabOrder = 1
  end
  object cmdOK: TButton
    Left = 82
    Top = 108
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 166
    Top = 108
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object editUsername: TEdit
    Left = 92
    Top = 44
    Width = 129
    Height = 21
    TabOrder = 2
  end
  object editPassword: TEdit
    Left = 92
    Top = 72
    Width = 129
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
end
