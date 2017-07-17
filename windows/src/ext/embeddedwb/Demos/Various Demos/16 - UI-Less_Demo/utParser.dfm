object Form1: TForm1
  Left = 314
  Top = 312
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Text IE Parser Demo'
  ClientHeight = 440
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 0
    object Go: TButton
      Left = 640
      Top = 11
      Width = 39
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoClick
    end
    object IEAddress1: TIEAddress
      Left = 8
      Top = 11
      Width = 627
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      ButtonColor = clBlack
      ButtonPressedColor = clBlack
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 404
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 50
      end>
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 688
    Height = 363
    Align = alClient
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 53
      Height = 13
      Caption = 'Anchor List'
    end
    object Label2: TLabel
      Left = 344
      Top = 6
      Width = 52
      Height = 13
      Caption = 'Links List'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 344
      Top = 193
      Width = 66
      Height = 13
      Caption = 'Images List'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoLinkList: TMemo
      Left = 344
      Top = 20
      Width = 337
      Height = 166
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object MemoAnchorList: TRichEditWB
      Left = 1
      Top = 1
      Width = 337
      Height = 361
      Hint = 
        'File Name: Untitled. | '#10#13'Position: Line:   1   Col:   1. | '#10#13'Mod' +
        'ified. | '#10#13'Caps Lock: Off. | '#10#13'NumLock: Off. | '#10#13'Insert: On. | '#10 +
        #13'Total Lines Count: 0. |'
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 1
      WordWrap = False
      AutoNavigate = True
      FileName = 'Untitled'
      HighlightHTML = True
      HighlightURL = True
      HighlightXML = True
      RTFText = 
        '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil Tahoma;}}'#13#10'{\colortbl ;\red0' +
        '\green0\blue0;}'#13#10'\viewkind4\uc1\pard\cf1\lang2055\f0\fs16\par'#13#10'}' +
        #13#10
      SupprtMoreThen64KB = False
      TextAlignment = taLeftJustify
      HideCaret = False
      Themes = tDefault
    end
    object MemoImageList: TRichEditWB
      Left = 344
      Top = 210
      Width = 337
      Height = 152
      Hint = 
        'File Name: Untitled. | '#10#13'Position: Line:   1   Col:   1. | '#10#13'Mod' +
        'ified. | '#10#13'Caps Lock: Off. | '#10#13'NumLock: Off. | '#10#13'Insert: On. | '#10 +
        #13'Total Lines Count: 0. |'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 2
      WordWrap = False
      AutoNavigate = True
      FileName = 'Untitled'
      HighlightHTML = True
      HighlightURL = True
      HighlightXML = True
      RTFText = 
        '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil Tahoma;}}'#13#10'{\colortbl ;\red0' +
        '\green0\blue0;}'#13#10'\viewkind4\uc1\pard\cf1\lang2055\f0\fs16\par'#13#10'}' +
        #13#10
      SupprtMoreThen64KB = False
      TextAlignment = taLeftJustify
      HideCaret = False
      Themes = tDefault
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 423
    Width = 688
    Height = 17
    Align = alBottom
    TabOrder = 3
  end
  object UILess1: TUILess
    About = 'TUILess parser  - Help & Support: http://www.bsalsa.com/'
    Left = 8
    Top = 56
  end
end
