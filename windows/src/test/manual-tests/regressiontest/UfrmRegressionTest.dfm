object frmRegressionTest: TfrmRegressionTest
  Left = 63
  Top = 104
  Caption = 'Regression Testing'
  ClientHeight = 300
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = mnuMain
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 491
    Height = 300
    ActivePage = tabRunTests
    Align = alClient
    MultiLine = True
    TabOrder = 0
    ExplicitWidth = 499
    ExplicitHeight = 290
    object tabSelectKeyboards: TTabSheet
      Caption = 'Select Keyboards'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblSelect: TLabel
        Left = 8
        Top = 16
        Width = 168
        Height = 13
        Caption = 'Select keyboards to regression test:'
      end
      object lbKeyboards: TListBox
        Left = 8
        Top = 36
        Width = 157
        Height = 149
        ItemHeight = 13
        TabOrder = 0
      end
      object cmdAdd: TButton
        Left = 8
        Top = 192
        Width = 73
        Height = 25
        Caption = '&Add...'
        TabOrder = 1
        OnClick = cmdAddClick
      end
      object cmdRemove: TButton
        Left = 88
        Top = 192
        Width = 73
        Height = 25
        Caption = '&Remove'
        TabOrder = 2
        OnClick = cmdRemoveClick
      end
    end
    object tabCreateTests: TTabSheet
      Caption = 'Create Tests'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        483
        272)
      object lblKeyboard: TLabel
        Left = 36
        Top = 11
        Width = 45
        Height = 13
        Caption = '&Keyboard'
        FocusControl = cbKeyboard
      end
      object lblNewTest: TLabel
        Left = 40
        Top = 43
        Width = 42
        Height = 13
        Caption = '&New test'
        FocusControl = editNewTest
      end
      object cbKeyboard: TComboBox
        Left = 88
        Top = 8
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object editNewTest: TEdit
        Left = 88
        Top = 40
        Width = 145
        Height = 21
        TabOrder = 1
      end
      object cmdAddTest: TButton
        Left = 246
        Top = 40
        Width = 75
        Height = 25
        Caption = 'Add test'
        Default = True
        TabOrder = 2
      end
      object gridTests: TStringGrid
        Left = 8
        Top = 84
        Width = 475
        Height = 142
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 4
        DefaultRowHeight = 16
        FixedCols = 0
        TabOrder = 3
        ColWidths = (
          26
          100
          84
          64)
      end
      object cmdRuleDelete: TButton
        Left = 8
        Top = 231
        Width = 73
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Delete'
        TabOrder = 4
      end
      object cmdRuleUp: TButton
        Left = 88
        Top = 231
        Width = 25
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '^'
        TabOrder = 5
      end
      object cmdRuleDown: TButton
        Left = 120
        Top = 231
        Width = 25
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'v'
        TabOrder = 6
      end
      object cmdRulesClear: TButton
        Left = 152
        Top = 231
        Width = 73
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Clear'
        TabOrder = 7
      end
    end
    object tabRunTests: TTabSheet
      Caption = 'Run Tests'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        483
        272)
      object lblTestWindow: TLabel
        Left = 112
        Top = 20
        Width = 63
        Height = 13
        Caption = 'Test Window'
      end
      object cmdRunTests: TButton
        Left = 8
        Top = 12
        Width = 75
        Height = 25
        Caption = '&Run Tests...'
        TabOrder = 0
      end
      object chkShowChangesOnly: TCheckBox
        Left = 8
        Top = 44
        Width = 181
        Height = 17
        Caption = 'Show only changed test results'
        TabOrder = 1
      end
      object gridResults: TStringGrid
        Left = 8
        Top = 68
        Width = 475
        Height = 186
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 4
        DefaultRowHeight = 16
        FixedCols = 0
        TabOrder = 2
        ColWidths = (
          22
          64
          64
          64)
      end
      object cmdColumns: TButton
        Left = 410
        Top = 36
        Width = 73
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Columns'
        TabOrder = 3
      end
      object editTestWindow: TEdit
        Left = 180
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 4
      end
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 744
    Top = 8
  end
  object mnuMain: TMainMenu
    Left = 744
    Top = 36
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuEditCut: TMenuItem
        Caption = 'Cu&t'
        GroupIndex = 1
        ImageIndex = 0
        ShortCut = 16472
      end
      object mnuEditCopy: TMenuItem
        Caption = '&Copy'
        GroupIndex = 1
        ImageIndex = 1
        ShortCut = 16451
      end
      object mnuEditPaste: TMenuItem
        Caption = '&Paste'
        GroupIndex = 1
        ImageIndex = 2
        ShortCut = 16470
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuEditFont: TMenuItem
        Caption = 'F&ont...'
        GroupIndex = 1
        ShortCut = 24646
      end
      object mnuEditResetFont: TMenuItem
        Caption = '&Reset font'
        GroupIndex = 1
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'kmx'
    Filter = 'Keyman keyboards (*.kmx)|*.kmx|All files (*.*)|*.*'
    Title = 'Add Keyboard'
    Left = 468
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'kmn'
    Filter = 'Regression Test (*.krt)|*.krt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save Regression Test As'
    Left = 438
    Top = 2
  end
end
