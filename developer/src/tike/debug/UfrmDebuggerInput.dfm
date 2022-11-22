inherited frmDebuggerInput: TfrmDebuggerInput
  Left = 392
  Top = 325
  Width = 258
  Height = 157
  Caption = 'Keyman Developer Debug Input'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 103
    Width = 121
    Height = 13
    Caption = 'Press Shift+Esc to cancel'
  end
  object panInput: TPanel
    Left = 8
    Top = 8
    Width = 233
    Height = 81
    BevelOuter = bvLowered
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 4
      Width = 205
      Height = 40
      Alignment = taCenter
      Caption = 'The Keyman Developer debugger is awaiting input'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object cmdCancel: TButton
    Left = 168
    Top = 96
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
