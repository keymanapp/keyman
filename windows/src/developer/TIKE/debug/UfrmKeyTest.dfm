inherited frmKeyTest: TfrmKeyTest
  Left = 227
  Top = 211
  HelpContext = 1240
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Virtual Key Identifier'
  ClientHeight = 240
  ClientWidth = 297
  Font.Name = 'MS Sans Serif'
  KeyPreview = True
  Position = poScreenCenter
  ExplicitWidth = 303
  ExplicitHeight = 269
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
    Top = 187
    Width = 252
    Height = 13
    Caption = 'Press Shift+Enter to insert or Shift+Esc to close dialog'
  end
  object lblDistinguish: TLabel
    Left = 27
    Top = 219
    Width = 91
    Height = 13
    Caption = 'left and right ctrl/alt'
  end
  object panKeyCode: TPanel
    Left = 8
    Top = 40
    Width = 281
    Height = 129
    TabOrder = 0
    object lblKeyCodeCaption: TLabel
      Left = 8
      Top = 41
      Width = 48
      Height = 13
      Caption = 'Key code:'
    end
    object lblKeymanNameCaption: TLabel
      Left = 8
      Top = 60
      Width = 70
      Height = 13
      Caption = 'Keyman name:'
    end
    object lblKeyCode: TLabel
      Left = 88
      Top = 40
      Width = 3
      Height = 13
    end
    object lblKeymanName: TLabel
      Left = 88
      Top = 60
      Width = 3
      Height = 13
    end
    object lblScanCode: TLabel
      Left = 88
      Top = 79
      Width = 3
      Height = 13
    end
    object lblScanCodeCaption: TLabel
      Left = 8
      Top = 79
      Width = 55
      Height = 13
      Caption = 'Scan code:'
    end
    object lblISOPosition: TLabel
      Left = 88
      Top = 98
      Width = 3
      Height = 13
    end
    object lblISOPositionCaption: TLabel
      Left = 8
      Top = 98
      Width = 58
      Height = 13
      Caption = 'ISO Position'
    end
    object lblActiveLayout: TLabel
      Left = 88
      Top = 8
      Width = 3
      Height = 13
    end
    object lblActiveLayoutCaption: TLabel
      Left = 8
      Top = 8
      Width = 64
      Height = 13
      Caption = 'Active layout:'
    end
  end
  object cmdClose: TButton
    Left = 216
    Top = 207
    Width = 73
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = cmdCloseClick
  end
  object cmdInsert: TButton
    Left = 132
    Top = 207
    Width = 73
    Height = 25
    Caption = 'Insert'
    Default = True
    TabOrder = 1
    OnClick = cmdInsertClick
  end
  object chkLRDistinguish: TCheckBox
    Left = 8
    Top = 203
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
