object frmCharacterMapFilter: TfrmCharacterMapFilter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Character Map Filter'
  ClientHeight = 345
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblRangeTo: TLabel
    Left = 149
    Top = 17
    Width = 22
    Height = 13
    Caption = '- U+'
    FocusControl = editFilter
  end
  object lblStar: TLabel
    Left = 63
    Top = 154
    Width = 156
    Height = 13
    Caption = 'Match any number of characters'
  end
  object lblQuestionMark: TLabel
    Left = 63
    Top = 187
    Width = 129
    Height = 13
    Caption = 'Match any single character'
  end
  object lblRange: TLabel
    Left = 63
    Top = 219
    Width = 314
    Height = 13
    Caption = 
      'Match any character in range (replace '#39'A'#39' and '#39'Z'#39' with your rang' +
      'e)'
  end
  object lblSet: TLabel
    Left = 63
    Top = 251
    Width = 264
    Height = 13
    Caption = 'Match any character in set (replace '#39'abc'#39' with your set)'
  end
  object lblDollar: TLabel
    Left = 63
    Top = 283
    Width = 104
    Height = 13
    Caption = 'End of string delimiter'
  end
  object lblBlock: TLabel
    Left = 63
    Top = 122
    Width = 225
    Height = 13
    Caption = 'Search for a block name, not a character name'
  end
  object lblRangeFrom: TLabel
    Left = 80
    Top = 17
    Width = 15
    Height = 13
    Caption = 'U+'
    FocusControl = editFilter
  end
  object lblCharactersInCurrentFontOnly: TLabel
    Left = 63
    Top = 90
    Width = 167
    Height = 13
    Caption = 'Characters only in the current font'
  end
  object editFilter: TEdit
    Left = 80
    Top = 48
    Width = 282
    Height = 21
    AutoSelect = False
    HideSelection = False
    TabOrder = 4
    OnChange = editFilterChange
  end
  object cmdStar: TButton
    Left = 16
    Top = 148
    Width = 41
    Height = 25
    Caption = '*'
    TabOrder = 7
    OnClick = cmdInsertClick
  end
  object cmdQuestionMark: TButton
    Left = 16
    Top = 180
    Width = 41
    Height = 25
    Caption = '?'
    TabOrder = 8
    OnClick = cmdInsertClick
  end
  object cmdRange: TButton
    Left = 16
    Top = 212
    Width = 41
    Height = 25
    Caption = '[A-Z]'
    TabOrder = 9
    OnClick = cmdInsertClick
  end
  object cmdSet: TButton
    Left = 16
    Top = 244
    Width = 41
    Height = 25
    Caption = '[abc]'
    TabOrder = 10
    OnClick = cmdInsertClick
  end
  object cmdOK: TButton
    Left = 119
    Top = 308
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 12
  end
  object cmdCancel: TButton
    Left = 200
    Top = 308
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 13
  end
  object cmdDollar: TButton
    Left = 16
    Top = 276
    Width = 41
    Height = 25
    Caption = '$'
    TabOrder = 11
    OnClick = cmdInsertClick
  end
  object cmdBlock: TButton
    Left = 16
    Top = 116
    Width = 41
    Height = 25
    Caption = '<'
    TabOrder = 6
    OnClick = cmdBlockClick
  end
  object editRangeStart: TEdit
    Left = 96
    Top = 14
    Width = 45
    Height = 21
    AutoSelect = False
    HideSelection = False
    TabOrder = 1
    OnChange = editRangeStartChange
    OnKeyPress = editRangeStopKeyPress
  end
  object editRangeStop: TEdit
    Left = 172
    Top = 14
    Width = 45
    Height = 21
    AutoSelect = False
    HideSelection = False
    TabOrder = 2
    OnChange = editRangeStopChange
    OnKeyPress = editRangeStopKeyPress
  end
  object rbRange: TRadioButton
    Left = 16
    Top = 16
    Width = 57
    Height = 15
    Caption = '&Range'
    TabOrder = 0
    OnClick = rbRangeClick
  end
  object rbName: TRadioButton
    Left = 16
    Top = 52
    Width = 57
    Height = 15
    Caption = '&Name'
    TabOrder = 3
    OnClick = rbNameClick
  end
  object cmdCharactersInCurrentFontOnly: TButton
    Left = 16
    Top = 84
    Width = 41
    Height = 25
    Caption = '>'
    TabOrder = 5
    OnClick = cmdCharactersInCurrentFontOnlyClick
  end
end
