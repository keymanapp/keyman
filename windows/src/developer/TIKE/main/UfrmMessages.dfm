inherited frmMessages: TfrmMessages
  Left = 148
  Top = 561
  HelpContext = 1100
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Messages'
  ClientHeight = 84
  ClientWidth = 312
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnDestroy = FormDestroy
  ExplicitWidth = 328
  ExplicitHeight = 123
  PixelsPerInch = 96
  TextHeight = 13
  object memoMessage: TMemo
    Left = 0
    Top = 0
    Width = 312
    Height = 84
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PopupMenu = mnuPopup
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnClick = memoMessageClick
    OnDblClick = memoMessageDblClick
    OnKeyDown = memoMessageKeyDown
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Left = 40
    Top = 20
  end
  object mnuPopup: TPopupMenu
    OnPopup = mnuPopupPopup
    Left = 88
    Top = 24
    object mnuViewItem: TMenuItem
      Caption = '&View selected item'
      Default = True
      OnClick = mnuViewItemClick
    end
    object mnuSeparator1: TMenuItem
      Caption = '-'
    end
    object cmdmSaveToFile: TMenuItem
      Caption = '&Save to file...'
      OnClick = cmdmSaveToFileClick
    end
    object cmdmClear: TMenuItem
      Caption = '&Clear'
      OnClick = cmdmClearClick
    end
    object mnuSeparator2: TMenuItem
      Caption = '-'
    end
    object cmdmNextMessage: TMenuItem
      Caption = '&Next message'
      OnClick = cmdmNextMessageClick
    end
    object cmdmPreviousMessage: TMenuItem
      Caption = '&Previous message'
      OnClick = cmdmPreviousMessageClick
    end
  end
end
