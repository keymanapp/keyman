object Form1: TForm1
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Brazilian Keyboard Test'
  ClientHeight = 337
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tabs: TPageControl
    Left = 4
    Top = 4
    Width = 585
    Height = 297
    ActivePage = TabSheet7
    TabOrder = 0
    object TabSheet1: TTabSheet
      TabVisible = False
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 68
        Top = 8
        Width = 141
        Height = 20
        Caption = 'Please exit Keyman.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
    object TabSheet2: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 68
        Top = 8
        Width = 360
        Height = 20
        Caption = 'Ensure that Brazilian layout is selected in Windows.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
    object TabSheet3: TTabSheet
      ImageIndex = 2
      TabVisible = False
      object Label6: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 68
        Top = 8
        Width = 315
        Height = 60
        Caption = 
          'Please press all character keys (in keyboard order: '#39'-=, q-[, a-' +
          '], etc.), followed by key #126.  Use backspace to correct mistak' +
          'es.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object editTestBr: TEdit
        Left = 68
        Top = 76
        Width = 185
        Height = 21
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      ImageIndex = 3
      TabVisible = False
      object Label7: TLabel
        Left = 68
        Top = 8
        Width = 296
        Height = 20
        Caption = 'Please select US English keyboard layout.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object TabSheet5: TTabSheet
      ImageIndex = 4
      TabVisible = False
      object Label10: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 5'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 68
        Top = 8
        Width = 315
        Height = 60
        Caption = 
          'Please press all character keys (in keyboard order: '#39'-=, q-[, a-' +
          '], etc.), followed by key #126.  Use backspace to correct mistak' +
          'es.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object editTestUS: TEdit
        Left = 68
        Top = 76
        Width = 185
        Height = 21
        TabOrder = 0
      end
    end
    object TabSheet6: TTabSheet
      ImageIndex = 5
      TabVisible = False
      object Label11: TLabel
        Left = 68
        Top = 8
        Width = 486
        Height = 20
        Caption = 
          'Select the Brazilian layout, then start Keyman, and select a key' +
          'board.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 6'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object TabSheet7: TTabSheet
      ImageIndex = 6
      TabVisible = False
      object Label13: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 7'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 68
        Top = 8
        Width = 315
        Height = 60
        Caption = 
          'Please press all character keys (in keyboard order: '#39'-=, q-[, a-' +
          '], etc.), followed by key #126.  Use backspace to correct mistak' +
          'es.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object editTestKM: TEdit
        Left = 68
        Top = 76
        Width = 185
        Height = 21
        TabOrder = 0
      end
    end
    object TabSheet8: TTabSheet
      ImageIndex = 7
      TabVisible = False
      object Label16: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 20
        Caption = 'Step 8'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label17: TLabel
        Left = 68
        Top = 8
        Width = 430
        Height = 40
        Caption = 
          'Thank you.  Please copy the text in the box below and send it to' +
          ' mcdurdin@tavultesoft.com.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object memoLog: TMemo
        Left = 68
        Top = 56
        Width = 505
        Height = 205
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object cmdExit: TButton
    Left = 436
    Top = 308
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'E&xit'
    TabOrder = 1
    OnClick = cmdExitClick
  end
  object cmdNext: TButton
    Left = 516
    Top = 308
    Width = 73
    Height = 25
    Caption = '&Next  >'
    Default = True
    TabOrder = 2
    OnClick = cmdNextClick
  end
end
