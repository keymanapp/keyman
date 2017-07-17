object Form1: TForm1
  Left = 87
  Top = 121
  Width = 836
  Height = 511
  Caption = 'TUrlHistory Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 480
    Top = 397
    Width = 25
    Height = 13
    Caption = 'Filter:'
  end
  object Label2: TLabel
    Left = 232
    Top = 376
    Width = 36
    Height = 13
    Caption = 'Sort by:'
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 37
    Width = 820
    Height = 404
    Align = alClient
    DefaultColWidth = 75
    DefaultRowHeight = 16
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      114
      234
      222
      112
      107)
  end
  object Panel1: TPanel
    Left = 0
    Top = 441
    Width = 820
    Height = 34
    Align = alBottom
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 675
      Top = 10
      Width = 137
      Height = 15
      Alignment = taLeftJustify
      Caption = 'Only sites visited today'
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 820
    Height = 37
    Align = alTop
    TabOrder = 2
    object Label4: TLabel
      Left = 604
      Top = 11
      Width = 33
      Height = 13
      Caption = 'Sort by'
    end
    object Label3: TLabel
      Left = 8
      Top = 12
      Width = 58
      Height = 13
      Caption = 'Search Text'
    end
    object ComboSortBy: TComboBox
      Left = 648
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Last visted'
        'Title'
        'Url'
        'Last updated'
        'Expires')
    end
    object Button1: TButton
      Left = 245
      Top = 6
      Width = 153
      Height = 25
      Caption = 'Enumerate !'
      TabOrder = 1
      OnClick = Button1Click
    end
    object edSearchText: TEdit
      Left = 80
      Top = 8
      Width = 153
      Height = 21
      TabOrder = 2
    end
  end
  object UrlHistory1: TUrlHistory
    OnAccept = UrlHistory1Accept
    SortField = sfLastVisited
    SearchField = seBoth
    SortDirection = sdAscending
    Left = 40
    Top = 136
  end
end
