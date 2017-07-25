object Form1: TForm1
  Left = 319
  Top = 345
  Width = 677
  Height = 488
  Caption = 'TEditDesigner Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 246
    Width = 669
    Height = 150
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 7
      Top = 6
      Width = 71
      Height = 13
      Caption = 'Document Info'
      Transparent = True
    end
    object Label2: TLabel
      Left = 216
      Top = 6
      Width = 51
      Height = 13
      Caption = 'Inner Text'
      Transparent = True
    end
    object Label3: TLabel
      Left = 423
      Top = 6
      Width = 50
      Height = 13
      Caption = 'Inner Html'
      Transparent = True
    end
    object MemoDocumentInfo: TMemo
      Left = 0
      Top = 22
      Width = 210
      Height = 122
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object MemoInnerText: TMemo
      Left = 216
      Top = 22
      Width = 201
      Height = 122
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object MemoInnerHTML: TMemo
      Left = 423
      Top = 22
      Width = 221
      Height = 123
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 669
    Height = 33
    Align = alTop
    TabOrder = 1
    object btnGo: TButton
      Left = 351
      Top = 8
      Width = 43
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = btnGoClick
    end
    object IEAddress1: TIEAddress
      Left = 7
      Top = 8
      Width = 338
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      EmbeddedWB = EmbeddedWB1
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
    end
    object btnRemoveDesigner: TButton
      Left = 536
      Top = 8
      Width = 108
      Height = 22
      Caption = 'Disconnect Designer'
      Enabled = False
      TabOrder = 2
      OnClick = btnRemoveDesignerClick
    end
    object btnConnectDesigner: TButton
      Left = 432
      Top = 8
      Width = 98
      Height = 22
      Caption = 'Connect Designer'
      TabOrder = 3
      OnClick = btnConnectDesignerClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 396
    Width = 669
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 70
      end
      item
        Width = 100
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 90
      end
      item
        Width = 90
      end
      item
        Width = 70
      end
      item
        Width = 70
      end>
  end
  object StatusBar2: TStatusBar
    Left = 0
    Top = 415
    Width = 669
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 250
      end
      item
        Width = 70
      end>
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 33
    Width = 669
    Height = 213
    Align = alClient
    TabOrder = 4
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'www.google.com')
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bSeite &p von &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    ControlData = {
      4C0000008F420000651500000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object EditDesigner1: TEditDesigner
    About = 'TEditDesigner - from http://www.bsalsa.com/'
    EmbeddedWB = EmbeddedWB1
    OnPreDrag = EditDesigner1PreDrag
    OnPreHandle = EditDesigner1PreHandleEvent
    OnPostHandle = EditDesigner1PostHandleEvent
    OnPostEditorNotify = EditDesigner1PostEditorEventNotify
    OnError = EditDesigner1Error
    OnInnerText = EditDesigner1InnerText
    OnInnerHtml = EditDesigner1InnerHtml
    OnKeyPress = EditDesigner1KeyPress
    OnKeyState = EditDesigner1KeyState
    OnTranslateAccelerator = EditDesigner1TranslateAccelerator
    OnMousePosition = EditDesigner1MousePosition
    OnMouseButton = EditDesigner1MouseButton
    OnEvtDispId = EditDesigner1EvtDispId
    OnSnapRect = EditDesigner1SnapRect
    OnType_ = EditDesigner1Type_
    OnToString = EditDesigner1ToString
    OnTagName = EditDesigner1TagName
    Left = 50
    Top = 145
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 88
    object File1: TMenuItem
      Caption = 'File'
      object SaveToFile1: TMenuItem
        Caption = 'Save To Demo.html'
        OnClick = SaveToFile1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Add1: TMenuItem
      Caption = 'Insert'
      object HyperLink1: TMenuItem
        Caption = 'HyperLink...'
        OnClick = HyperLink1Click
      end
      object Image1: TMenuItem
        Caption = 'Image...'
        OnClick = Image1Click
      end
      object RadioButton1: TMenuItem
        Caption = 'Radio Button'
        OnClick = RadioButton1Click
      end
    end
    object Fonts1: TMenuItem
      Caption = 'Fonts'
      object Bold1: TMenuItem
        Caption = 'Set Selected Text As Bold'
        OnClick = Bold1Click
      end
      object SetSelTextAsUndelline1: TMenuItem
        Caption = 'Set Selected Text As Undelline'
        OnClick = SetSelTextAsUndelline1Click
      end
      object Italic1: TMenuItem
        Caption = 'Set Selected Text Italic'
        OnClick = Italic1Click
      end
    end
  end
end
