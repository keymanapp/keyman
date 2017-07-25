object FormMain: TFormMain
  Left = 2009
  Top = 136
  Width = 722
  Height = 587
  Caption = 'FormMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 197
    Top = 29
    Height = 483
  end
  object MainTreeView: TTreeView
    Left = 0
    Top = 29
    Width = 197
    Height = 483
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnChange = MainTreeViewChange
    OnDeletion = MainTreeViewDeletion
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 512
    Width = 706
    Height = 19
    Panels = <>
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 706
    Height = 29
    Caption = 'ToolBar'
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'add'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 2
      Caption = 'ToolButton2'
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 46
      Top = 2
      Caption = 'ToolButton3'
      ImageIndex = 2
    end
  end
  object Panel1: TPanel
    Left = 200
    Top = 29
    Width = 506
    Height = 483
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 1
      Top = 244
      Width = 504
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object MainListView: TListView
      Left = 1
      Top = 1
      Width = 504
      Height = 243
      Align = alTop
      Columns = <>
      ReadOnly = True
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 1
      Top = 247
      Width = 504
      Height = 235
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      DesignSize = (
        504
        235)
      object ACLListView: TListView
        Left = 8
        Top = 8
        Width = 486
        Height = 123
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 8
        Top = 138
        Width = 97
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'CheckBox1'
        TabOrder = 1
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 170
        Width = 97
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'CheckBox2'
        TabOrder = 2
      end
      object ACLEditButton: TBitBtn
        Left = 421
        Top = 202
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Edit'
        TabOrder = 3
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 208
    Top = 40
    object file1: TMenuItem
      Caption = 'file'
    end
  end
end
