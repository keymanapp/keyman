object frmRenderingTestCases: TfrmRenderingTestCases
  Left = 0
  Top = 0
  Caption = 'Rendering Test Cases'
  ClientHeight = 613
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    573
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 148
    Top = 180
    Width = 3
    Height = 13
  end
  object lblFontSizeCaption: TLabel
    Left = 361
    Top = 180
    Width = 48
    Height = 13
    Caption = 'Font Size:'
  end
  object lblFontSize: TLabel
    Left = 543
    Top = 180
    Width = 22
    Height = 13
    Alignment = taRightJustify
    Caption = '12pt'
  end
  object gridResults: TStringGrid
    Left = 8
    Top = 206
    Width = 557
    Height = 399
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnDrawCell = gridResultsDrawCell
    OnExit = gridResultsExit
    OnMouseDown = gridResultsMouseDown
    OnMouseLeave = gridResultsMouseLeave
    OnMouseMove = gridResultsMouseMove
  end
  object memoSource: TMemo
    Left = 8
    Top = 39
    Width = 557
    Height = 130
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object cmdFindExamples: TButton
    Left = 8
    Top = 175
    Width = 125
    Height = 25
    Caption = 'Find Font Examples'
    TabOrder = 2
    OnClick = cmdFindExamplesClick
  end
  object tbFontSize: TTrackBar
    Left = 415
    Top = 175
    Width = 126
    Height = 25
    Max = 64
    Min = 8
    Frequency = 8
    Position = 12
    TabOrder = 3
    OnChange = tbFontSizeChange
  end
  object cmdNew: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&New'
    TabOrder = 4
    OnClick = cmdNewClick
  end
  object cmdOpen: TButton
    Left = 89
    Top = 8
    Width = 88
    Height = 25
    Caption = '&Open Project...'
    TabOrder = 5
    OnClick = cmdOpenClick
  end
  object cmdSave: TButton
    Left = 183
    Top = 8
    Width = 88
    Height = 25
    Caption = '&Save Project'
    TabOrder = 6
    OnClick = cmdSaveClick
  end
  object cmdExport: TButton
    Left = 277
    Top = 8
    Width = 88
    Height = 25
    Caption = '&Export to HTML'
    TabOrder = 7
    OnClick = cmdExportClick
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Left = 280
    Top = 312
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'#13#10'*.*'
    Left = 328
    Top = 312
  end
  object dlgSaveHTML: TSaveDialog
    DefaultExt = 'html'
    Filter = 'HTML files (*.htm,*.html)|*.htm*|All files (*.*)|*.*'#13#10'*.*'
    Left = 364
    Top = 312
  end
end
