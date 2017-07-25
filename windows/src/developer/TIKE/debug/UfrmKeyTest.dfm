inherited frmKeyTest: TfrmKeyTest
  Left = 227
  Top = 211
  HelpContext = 1240
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Virtual Key Identifier'
  ClientHeight = 149
  ClientWidth = 297
  Font.Name = 'MS Sans Serif'
  KeyPreview = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 251
    Height = 26
    Caption = 'Press any key to see its Keyman virtual key code:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object lblClose: TLabel
    Left = 36
    Top = 96
    Width = 252
    Height = 13
    Caption = 'Press Shift+Enter to insert or Shift+Esc to close dialog'
  end
  object lblDistinguish: TLabel
    Left = 27
    Top = 128
    Width = 91
    Height = 13
    Caption = 'left and right ctrl/alt'
  end
  object panKeyCode: TPanel
    Left = 8
    Top = 40
    Width = 281
    Height = 49
    TabOrder = 0
    object lblKeyCodeCaption: TLabel
      Left = 8
      Top = 8
      Width = 48
      Height = 13
      Caption = 'Key code:'
    end
    object lblKeymanNameCaption: TLabel
      Left = 8
      Top = 28
      Width = 70
      Height = 13
      Caption = 'Keyman name:'
    end
    object lblKeyCode: TLabel
      Left = 88
      Top = 8
      Width = 3
      Height = 13
    end
    object lblKeymanName: TLabel
      Left = 88
      Top = 28
      Width = 3
      Height = 13
    end
  end
  object cmdClose: TButton
    Left = 216
    Top = 116
    Width = 73
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = cmdCloseClick
  end
  object cmdInsert: TButton
    Left = 132
    Top = 116
    Width = 73
    Height = 25
    Caption = 'Insert'
    Default = True
    TabOrder = 1
    OnClick = cmdInsertClick
  end
  object chkLRDistinguish: TCheckBox
    Left = 8
    Top = 112
    Width = 117
    Height = 17
    Caption = '&Distinguish between'
    TabOrder = 3
  end
  object AppEvent: TApplicationEvents
    OnMessage = AppEventMessage
    Left = 264
    Top = 8
  end
end
