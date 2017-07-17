object Form1: TForm1
  Left = 236
  Top = 318
  Width = 865
  Height = 474
  Caption = 'IE HTML  Parser Demo'
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
    Width = 857
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnGo: TButton
      Left = 749
      Top = 12
      Width = 42
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = btnGoClick
    end
    object edtAddress: TEdit
      Left = 8
      Top = 12
      Width = 729
      Height = 21
      TabOrder = 1
      Text = 'http://bsalsa.com/'
    end
    object Button1: TButton
      Left = 800
      Top = 12
      Width = 42
      Height = 22
      Caption = 'Stop'
      TabOrder = 2
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 421
    Width = 857
    Height = 19
    Panels = <
      item
        Width = 70
      end
      item
        Width = 350
      end>
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 857
    Height = 380
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 855
      Height = 378
      ActivePage = tsScript
      Align = alClient
      TabOrder = 0
      object tsParse: TTabSheet
        Caption = 'Parse'
        ImageIndex = 15
        object memParse: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsProp: TTabSheet
        Caption = 'Doc Properties'
        ImageIndex = 4
        object memProp: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsAnchor: TTabSheet
        Caption = 'Anchor'
        object memAnchor: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object Images: TTabSheet
        Caption = 'Images'
        ImageIndex = 2
        object memImages: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object Meta: TTabSheet
        Caption = ' MetaTags'
        ImageIndex = 3
        object memMeta: TMemo
          Left = 0
          Top = 0
          Width = 847
          Height = 350
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsBase: TTabSheet
        Caption = 'Base'
        ImageIndex = 5
        object memBase: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsBaseFont: TTabSheet
        Caption = 'BaseFont'
        ImageIndex = 6
        object memBaseFont: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsBody: TTabSheet
        Caption = 'Body'
        ImageIndex = 7
        object memBody: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsBR: TTabSheet
        Caption = 'BR'
        ImageIndex = 8
        object memBR: TMemo
          Left = 0
          Top = 0
          Width = 847
          Height = 350
          Align = alClient
          Lines.Strings = (
            'Memo2')
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsComment: TTabSheet
        Caption = 'Comment'
        ImageIndex = 9
        object memComment: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsDiv: TTabSheet
        Caption = 'Div'
        ImageIndex = 10
        object memDiv: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsFont: TTabSheet
        Caption = 'Font'
        ImageIndex = 11
        object memFont: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsForm: TTabSheet
        Caption = 'Form'
        ImageIndex = 12
        object memForm: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsHR: TTabSheet
        Caption = 'HR'
        ImageIndex = 13
        object memHR: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsMarquee: TTabSheet
        Caption = 'Marquee'
        ImageIndex = 14
        object memMarquee: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsPError: TTabSheet
        Caption = 'ParseError'
        ImageIndex = 16
        object memParseError: TMemo
          Left = 0
          Top = 0
          Width = 844
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsScript: TTabSheet
        Caption = 'Script'
        ImageIndex = 17
        object memScript: TMemo
          Left = 0
          Top = 0
          Width = 847
          Height = 350
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
  end
  object IEParser1: TIEParser
    About = 'TIEParser from: http://www.bsalsa.com'
    OnAnchor = IEParser1Anchor
    OnBase = IEParser1Base
    OnBaseFont = IEParser1BaseFont
    OnBody = IEParser1Body
    OnBR = IEParser1BR
    OnBusyStateChange = IEParser1BusyStateChange
    OnComment = IEParser1Comment
    OnDiv = IEParser1Div
    OnDocInfo = IEParser1DocInfo
    OnParseComplete = IEParser1ParseComplete
    OnFont = IEParser1Font
    OnForm = IEParser1Form
    OnHR = IEParser1HR
    OnImage = IEParser1Image
    OnMarquee = IEParser1Marquee
    OnMeta = IEParser1Meta
    OnParseDocument = IEParser1ParseDocument
    OnParseError = IEParser1ParseError
    OnScript = IEParser1Script
    OnStateChange = IEParser1StateChange
    Left = 32
    Top = 112
  end
end
