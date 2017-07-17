object Form1: TForm1
  Left = 304
  Top = 258
  Width = 854
  Height = 527
  Caption = 'Zones & Security Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 413
    Width = 98
    Height = 13
    Caption = 'Current template:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 7
    Top = 434
    Width = 105
    Height = 13
    Caption = 'Minimum template:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 7
    Top = 456
    Width = 109
    Height = 13
    Caption = 'Recomm. template:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 589
    Height = 397
    ColCount = 2
    DefaultColWidth = 265
    DefaultRowHeight = 16
    FixedCols = 0
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 118
    Top = 409
    Width = 297
    Height = 22
    BevelInner = bvLowered
    TabOrder = 1
    object CurrentImage: TImage
      Left = 5
      Top = 3
      Width = 16
      Height = 16
    end
    object CurrentDisplay: TLabel
      Left = 28
      Top = 5
      Width = 3
      Height = 13
    end
  end
  object Panel2: TPanel
    Left = 118
    Top = 431
    Width = 297
    Height = 22
    BevelInner = bvLowered
    TabOrder = 2
    object MinimumImage: TImage
      Left = 5
      Top = 3
      Width = 16
      Height = 16
    end
    object MinimumDisplay: TLabel
      Left = 28
      Top = 5
      Width = 3
      Height = 13
    end
  end
  object Panel3: TPanel
    Left = 118
    Top = 453
    Width = 297
    Height = 22
    BevelInner = bvLowered
    TabOrder = 3
    object RecommImage: TImage
      Left = 5
      Top = 3
      Width = 16
      Height = 16
    end
    object RecommDisplay: TLabel
      Left = 27
      Top = 5
      Width = 3
      Height = 13
    end
  end
  object Panel4: TPanel
    Left = 600
    Top = 0
    Width = 246
    Height = 493
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    object Label2: TLabel
      Left = 11
      Top = 219
      Width = 204
      Height = 13
      Caption = 'Sites or urlpatterns added to selected zone:'
    end
    object Label4: TLabel
      Left = 13
      Top = 8
      Width = 176
      Height = 13
      Caption = 'Select an Urlzone from the list:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoURLPatterns: TMemo
      Left = 11
      Top = 237
      Width = 221
      Height = 244
      ReadOnly = True
      TabOrder = 0
    end
    object ListView1: TListView
      Left = 11
      Top = 24
      Width = 220
      Height = 177
      Columns = <
        item
          AutoSize = True
        end>
      ReadOnly = True
      TabOrder = 1
      ViewStyle = vsReport
      OnSelectItem = ListView1SelectItem
    end
  end
end
